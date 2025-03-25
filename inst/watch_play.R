
ds <- load_ds()

plays_clean <- load_plays()

games <- load_games()

play_details <- play_detailer(plays_clean, games)

color_scheme <- color_generator()

players <- load_players()

x_breaks <- c(-50,-10,10,50)
y_breaks <- c(-100, -20, -10, 0, 15)

game_id <- 2022091901
play_id <- 64

play_animator(ds, game_id, play_id, play_details, color_scheme)

## quintessential 2 high
play_animator(ds, 2021101012, 754, play_details, color_scheme)

## quintessential quarters
play_animator(ds, 2021103112, 4317, play_details, color_scheme)

## quintessential 1-high
play_animator(ds, 2021101000, 3144, play_details, color_scheme)

hierarchy_preds <- "preds/hierarchy_preds_20250315.csv" %>%
  read_csv

formations_by_clip_id <- "formations_by_clip_id.csv" %>%
  read_csv

play_selection <- hierarchy_preds %>%
  list(formations_by_clip_id) %>%
  reduce(left_join,by = "clip_id") %>%
  filter(actual == "1-High",between(pred1,.25,.375)) %>%
  slice_sample(n = 1) %>%
  print(width = Inf) %>%
  select(playId,gameId)

play_animator(ds, 2021091201, 3004, play_details, color_scheme)

play_personnel <- ds %>%
  mutate(across(nflId,as.integer)) %>%
  left_join(
    plays_clean %>%
      select(gameId,playId,season) %>%
      mutate(across(c(gameId,playId),as.integer)),
    by = c("gameId","playId")
  ) %>%
  left_join(
    players %>%
      select(nflId,season,position) %>%
      mutate(across(nflId,as.integer)),
    by = c("nflId","season")
  ) %>%
  distinct(gameId,playId,nflId,position) %>%
  group_by(gameId,playId,position) %>%
  summarise(count = n(),.groups = "drop") %>%
  filter(position %in% c("WR","TE")) %>%
  mutate(pers = paste(count,position)) %>%
  group_by(gameId,playId) %>%
  collect %>%
  summarise(
    across(pers,~paste(sort(.),collapse = ", ")),
    .groups = "drop"
  )

hierarchy_preds %>%
  list(formations_by_clip_id) %>%
  reduce(left_join,by = "clip_id") %>%
  filter(pff_passCoverage == "Quarters",splits == "3x1",between(pred1,.5,.7)) %>%
  left_join(play_personnel,by = c("gameId","playId")) %>%
  count(pers)

hierarchy_preds %>%
  list(formations_by_clip_id) %>%
  reduce(left_join,by = "clip_id") %>%
  filter(actual == 1,between(pred1,.5,.7)) %>%
  left_join(play_personnel,by = c("gameId","playId")) %>%
  count(pff_passCoverage,pers) %>%
  arrange(desc(n))



play_selection <- hierarchy_preds %>%
  list(formations_by_clip_id) %>%
  reduce(left_join,by = "clip_id") %>%
  left_join(play_personnel,by = c("gameId","playId")) %>%
  filter(pff_passCoverage == "Quarters",splits == "3x2",between(pred1,.5,.7)) %>%
  slice_sample(n = 1) %>%
  print(width = Inf)

play_animator(ds, play_selection$gameId, play_selection$playId, play_details, color_scheme)
