spots <- ds %>%
  filter(team == "football") %>%
  mutate(across(c(playId,gameId,frameId),as.integer)) %>%
  inner_join(
    play_crop %>%
      rename(frameId = snap) %>%
      mutate(across(frameId,as.integer)),
    by = c("gameId","playId","frameId")
  ) %>%
  select(gameId,playId,spot_x = x,spot_y = y) %>%
  collect

alignment_by_play <- ds %>%
  list(
    filter(.,team == "football",event %in% c("autoevent_ballsnap","ball_snap")) %>%
      group_by(gameId,playId) %>%
      mutate(frame_snap = min(frameId)) %>%
      filter(frameId == frame_snap) %>%
      ungroup %>%
      select(gameId,playId,frameId,spot_x = x,spot_y = y)
  ) %>%
  reduce(inner_join,by = c("gameId","playId","frameId")) %>%
  left_join(
    plays_clean %>%
      select(gameId,playId,defensiveTeam,yardsToGo,season) %>%
      mutate(across(c(gameId,playId),as.integer)),
    by = c("gameId","playId")
  ) %>%
  mutate(across(nflId,as.integer)) %>%
  left_join(
    players %>%
      select(nflId,season,position) %>%
      mutate(across(nflId,as.integer)),
    on = c("nflId","season")
  ) %>%
  mutate(
    flip_direction = ifelse(playDirection == "right","counterclockwise","clockwise"),
    newx = ifelse(flip_direction == "counterclockwise", y, -y + 53.33),
    newy = ifelse(flip_direction == "counterclockwise", -x, x),
    newo = ifelse(flip_direction == "counterclockwise",-(o +90), -(o - 90)),
    newspot_y = ifelse(flip_direction == "counterclockwise", -spot_x,spot_x),
    newspot_x = ifelse(flip_direction == "counterclockwise",spot_y, -spot_y + 53.33),
    line_to_gain = newspot_y - yardsToGo
  ) %>%
  mutate(across(
    c(newy,newspot_y,line_to_gain),
    ~ifelse(flip_direction == "counterclockwise",.x + 110,.x - 10)
  )) %>%
  mutate(side = ifelse(team == "football","football",ifelse(team == defensiveTeam,"defense","offense"))) %>%
  filter(side == "offense") %>%
  mutate(
    width = (newx - newspot_x),
    depth = (newy - newspot_y),
    alignment = ifelse(abs(width) < 1,"middle",ifelse(width > 0,"right","left")),
    backfield = depth > 2  & abs(width) < 5
  ) %>%
  collect

formation_by_play <-
  alignment_by_play %>%
  #filter(playId == 3607,gameId == 2018090900) %>%
  group_by(gameId,playId,alignment,backfield) %>%
  mutate(
    width_rk = data.table::frankv(abs(width),order = 1),
    split = abs(width) - lag(abs(width),order_by = width_rk,default = 0),
    split2 = abs(width) - lag(abs(width),order_by = width_rk,default = 0,n = 2),
    split_break = split > 2.5
  ) %>%
  arrange(gameId,playId,alignment,backfield,width_rk) %>%
  filter(position %in% c("TE","WR","RB","FB")) %>%
  group_by(gameId,playId,alignment,backfield) %>%
  mutate(
    horiz_group = cumsum(split_break) + 1,
    core = horiz_group == 1 & backfield == F,
    width_rk = data.table::frankv(abs(width),order = 1),
    bunch = split2 < 6 & width_rk >= 3 & backfield == F
  ) %>%
  group_by(gameId,playId) %>%
  filter(n() == 5) %>%
  ungroup %>%
  #print(width = Inf)
  list(
    group_by(.,gameId,playId) %>%
      summarise(
        core = sum(core),
        backs = sum(backfield),
        bunch = sum(bunch),
        .groups = "drop"
      ),
    filter(.,alignment != "middle") %>%
      group_by(gameId,playId,alignment,core) %>%
      summarise(
        bunch = sum(bunch),
        count = sum(backfield == F),
        .groups = "drop"
      ) %>%
      mutate(
        across(core,~ifelse(.,"c","s")),
        flank = paste0(count,core)
      ) %>%
      mutate(
        across(bunch,~ifelse(. == 1,"(bu)","")),
        across(flank,~paste0(.x,bunch))
      ) %>%
      group_by(gameId,playId,alignment,bunch) %>%
      summarise(
        flank = paste(sort(flank,decreasing = F),collapse = ""),
        .groups = "drop"
      ) %>%
      group_by(gameId,playId) %>%
      summarise(
        formation = paste(sort(flank,decreasing = T),collapse = "x"),
        .groups = "drop"
      )
  ) %>%
  .[-1] %>%
  reduce(left_join,by = c("gameId","playId"))

formation_by_play %>%
  count(backs,core,bunch,formation) %>%
  arrange(desc(n)) %>%
  mutate(pct = n/sum(n),
         cumu_pct = cumsum(pct))

formation_by_play %>%
  write_csv("formations_by_play_id.csv")
