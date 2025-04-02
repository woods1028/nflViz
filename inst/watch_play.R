
ds <- load_ds()

plays_clean <- load_plays()

games <- load_games()

play_details <- play_detailer(plays_clean, games)

color_scheme <- color_generator()

players <- load_players()

hierarchy_preds <- "preds/hierarchy_preds_20250315.csv" %>%
  read_csv %>%
  mutate(across(actual,~data.table::setattr(factor(.),"levels",c("2-High","1-High"))))

formations_by_play_id <- "formations_by_play_id.csv" %>%
  read_csv

formation_names <- c(
  "3sx1s" = "3x1 open", "2sx1c1s" = "slot", "2sx2s" = "2x2 open", "3sx2s" = "empty open",
  "1sx1c2s" = "trey", "2sx1s" = "flex", "1sx1c1s" = "pro", "2sx2c" = "nub twins", "1s2cx1s" = "trey", "2sx1c2s" = "empty h",
  "1c1sx1c1s" = "ace", "3sx0s1c" = "nub trips", "2sx0s2c" = "flank", "3sx1c" = "nub trips", "1sx1s" = "bone", "2sx1c" = "twins",
  "3cx1s" = "trips bunch"
) %>%
  tibble(
    id = names(.),
    formation = .
  )

snap_events <- c("ball_snap","autoevent_ballsnap","snap_direct")

events_to_clip <- c(
  'pass_forward','autoevent_passforward','qb_sack','run','pass_outcome_incomplete','pass_arrived',
  'autoevent_passinterrupted','qb_strip_sack','pass_tipped','fumble','pass_outcome_caught','handoff',
  'pass_shovel','qb_spike','pass_outcome_interception', 'lateral', 'tackle',
  'fumble_defense_recovered', 'fumble_offense_recovered', 'qb_slide',
  'pass_outcome_touchdown', 'dropped_pass', 'safety'
)

play_crop <- ds %>%
  distinct(gameId,playId,frameId,event) %>%
  group_by(gameId,playId) %>%
  mutate(
    snap_event = event %in% snap_events,
    clip_event = event %in% events_to_clip
  ) %>%
  list(
    filter(.,snap_event == T),
    filter(.,clip_event == T)
  ) %>%
  .[-1] %>%
  map(function(x) {

    group_by(x,gameId,playId) %>%
      summarise(across(frameId,min),.groups = "drop") %>%
      collect

  }) %>%
  list(c("snap","clip")) %>%
  pmap(function(x,y) mutate(x,type = y)) %>%
  bind_rows %>%
  tidyr::pivot_wider(id_cols = c(gameId,playId),names_from = type,values_from = frameId)

play_selection <- hierarchy_preds %>%
  list(formations_by_play_id) %>%
  reduce(left_join,by = c("gameId","playId")) %>%
  rename(id = formation) %>%
  left_join(formation_names,by = "id") %>%
  mutate(across(formation,~ifelse(is.na(.),id,.))) %>%
  list(
    plays_clean %>%
      select(gameId,playId,defensiveTeam,season)
  ) %>%
  reduce(left_join,by = c("gameId","playId")) %>%
  list(
    play_crop %>%
      mutate(frames = clip - snap) %>%
      select(gameId,playId,frames)
  ) %>%
  reduce(left_join,by = c("gameId","playId")) %>%
  filter(
    #actual == "2-High",
    pff_passCoverage %in% c("Quarters","Cover-6"),
    #defensiveTeam %in% c("LAR","LAC"),
    season == 2022,
    between(pred0,.4,1),
    formation %in% c("2x2 open","slot","empty open"),
    frames >= 25
  ) %>%
  #filter(clip_id == 15875) %>%
  slice_sample(n = 1) %>%
  print(width = Inf);play_animator(ds, play_selection$gameId, play_selection$playId, play_details, color_scheme)

