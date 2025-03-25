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

accuracy_by_formation <- hierarchy_preds %>%
  list(formations_by_play_id) %>%
  reduce(left_join,by = c("gameId","playId")) %>%
  rename(id = formation) %>%
  left_join(formation_names,by = "id") %>%
  mutate(across(formation,~ifelse(is.na(.),id,.))) %>%
  mutate(pred = ifelse(pred1 > .5,"1-High","2-High"),
         match = (actual == pred)) %>%
  group_by(formation,backs,actual) %>%
  summarise(
    count = n(),
    correct = sum(match),
    .groups = "drop"
  ) %>%
  mutate(pct = correct/count) %>%
  arrange(desc(count)) %>%
  filter(count > 100)

accuracy_by_formation %>%
  filter(backs %in% c(0,1)) %>%
  ggplot(mapping = aes(x = formation,y = pct,size = count,color = actual))+
  geom_point()+
  theme(axis.text.x = element_text(size = 10))+
  scale_size_continuous(range = c(2,13))

accuracy_by_formation %>%
  group_by(actual,backs) %>%
  mutate(
    diff = min(pct) - max(pct),
    across(diff,~ifelse(pct == min(pct),.x,-.x))
  ) %>%
  ungroup %>%
  arrange(diff)

hierarchy_preds %>%
  list(formations_by_play_id) %>%
  reduce(left_join,by = c("gameId","playId")) %>%
  filter(actual == "2-High",pred1 > .5) %>%
  mutate(across(pff_passCoverage,~ifelse(. == "Cover-6","Quarters",.x))) %>%
  count(backs,formation,pff_passCoverage) %>%
  mutate(pct = n/sum(n)) %>%
  arrange(desc(n))

hierarchy_preds %>%
  list(formations_by_play_id) %>%
  reduce(left_join,by = c("gameId","playId")) %>%
  rename(id = formation) %>%
  left_join(formation_names,by = "id") %>%
  mutate(across(formation,~ifelse(is.na(.),id,.))) %>%
  filter(formation %in% c("empty open","3x1 open","2x2 open","trey")) %>%
  ggplot(mapping = aes(x = pred1,fill = pff_passCoverage))+
  geom_vline(xintercept = .5,lty = 2)+
  geom_density(alpha = .7)+
  facet_wrap(vars(formation,actual),nrow = 2)+
  labs(fill = "",x = "1-High Odds",y = "density")+
  scale_x_continuous(labels = scales::percent)+
  scale_fill_manual(values = c(
    "2-Man" = "firebrick1",
    "Cover-3" = "turquoise",
    "Cover-1" = "purple",
    "Cover-2" = "blue1",
    "Cover-6" = "green3",
    "Quarters" = "gold1"
  )) +
  theme(
    axis.title.y = element_text(angle = 0,vjust = .5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 14,color = "white",face = "bold"),
    strip.background = element_rect(fill = "dodgerblue4"),
    legend.text = element_text(size = 12)
  )

hierarchy_preds %>%
  list(formations_by_play_id) %>%
  reduce(left_join,by = c("gameId","playId")) %>%
  rename(id = formation) %>%
  left_join(formation_names,by = "id") %>%
  mutate(across(formation,~ifelse(is.na(.),id,.))) %>%
  list(
    play_crop %>%
      mutate(frames = clip - snap) %>%
      select(gameId,playId,frames)
  ) %>%
  reduce(left_join,by = c("gameId","playId")) %>%
  filter(formation %in% c("empty open","3x1 open","2x2 open","trey")) %>%
  mutate(
    across(actual,~as.factor(.) %>% data.table::setattr("levels",c("2-High","1-High"))),
    pred = ifelse(pred1 > .5,"1-High","2-High"),
    match = (actual == pred)
  ) %>%
  ggplot(mapping = aes(x = formation,y = frames,fill = match))+
  geom_boxplot(position = "dodge",varwidth = T)+
  facet_wrap(vars(actual))

## quintessential 2-high
play_selection <- hierarchy_preds %>%
  list(formations_by_play_id) %>%
  reduce(left_join,by = c("gameId","playId"))  %>%
  filter(actual == "2-High") %>%
  arrange(pred1) %>%
  slice(1) %>%
  select(playId,gameId)

## quintessential quarters
play_selection <- hierarchy_preds %>%
  list(formations_by_play_id) %>%
  reduce(left_join,by = c("gameId","playId")) %>%
  filter(pff_passCoverage == "Quarters") %>%
  arrange(pred1) %>%
  slice(1) %>%
  select(playId,gameId)

play_animator(ds, play_selection$gameId, play_selection$playId, play_details, color_scheme)

## quintessential 1-high: notice the blitz
play_selection <- hierarchy_preds %>%
  list(formations_by_play_id) %>%
  reduce(left_join,by = c("gameId","playId")) %>%
  filter(actual == "1-High") %>%
  arrange(desc(pred1)) %>%
  slice(1) %>%
  select(playId,gameId)

## mostly confidently wrong that it's 2-High
play_selection <- hierarchy_preds %>%
  list(formations_by_play_id) %>%
  reduce(left_join,by = c("gameId","playId"))  %>%
  filter(actual == "1-High") %>%
  arrange(pred1) %>%
  slice(2) %>%
  select(playId,gameId)

#### view ####

formation1 <- "empty open"

## wrong

play_selection <- hierarchy_preds %>%
  list(formations_by_play_id) %>%
  reduce(left_join,by = c("gameId","playId")) %>%
  rename(id = formation) %>%
  left_join(formation_names,by = "id") %>%
  mutate(across(formation,~ifelse(is.na(.),id,.))) %>%
  list(
    play_crop %>%
      mutate(frames = clip - snap) %>%
      select(gameId,playId,frames)
  ) %>%
  reduce(left_join,by = c("gameId","playId")) %>%
  filter(actual == "2-High",between(pred1,.5,.75),formation == formation1,frames >= 25) %>%
  slice_sample(n = 1) %>%
  print(width = Inf) %>%
  select(playId,gameId); play_animator(ds, play_selection$gameId, play_selection$playId, play_details, color_scheme)

## right

play_selection <- hierarchy_preds %>%
  list(formations_by_play_id) %>%
  reduce(left_join,by = c("gameId","playId")) %>%
  rename(id = formation) %>%
  left_join(formation_names,by = "id") %>%
  mutate(across(formation,~ifelse(is.na(.),id,.)))  %>%
  list(
    play_crop %>%
      mutate(frames = clip - snap) %>%
      select(gameId,playId,frames)
  ) %>%
  reduce(left_join,by = c("gameId","playId")) %>%
  filter(pff_passCoverage == "Quarters",between(pred1,0,.3),formation == formation1,frames >= 25) %>%
  slice_sample(n = 1) %>%
  print(width = Inf) %>%
  select(playId,gameId); play_animator(ds, play_selection$gameId, play_selection$playId, play_details, color_scheme)

### quarters? ####

play_selection <- hierarchy_preds %>%
  list(formations_by_play_id) %>%
  reduce(left_join,by = c("gameId","playId")) %>%
  rename(id = formation) %>%
  left_join(formation_names,by = "id") %>%
  mutate(across(formation,~ifelse(is.na(.),id,.))) %>%
  list(
    play_crop %>%
      mutate(frames = clip - snap) %>%
      select(gameId,playId,frames)
  ) %>%
  reduce(left_join,by = c("gameId","playId")) %>%
  filter(pff_passCoverage == "Quarters",between(pred1,.15,.35),formation == formation1,frames >= 25) %>%
  slice_sample(n = 1) %>%
  print(width = Inf) %>%
  select(playId,gameId); play_animator(ds, play_selection$gameId, play_selection$playId, play_details, color_scheme)


#### fangio? ####

hierarchy_preds %>%
  list(formations_by_play_id) %>%
  reduce(left_join,by = c("gameId","playId")) %>%
  rename(id = formation) %>%
  left_join(formation_names,by = "id") %>%
  mutate(across(formation,~ifelse(is.na(.),id,.))) %>%
  list(
    play_crop %>%
      mutate(frames = clip - snap) %>%
      select(gameId,playId,frames)
  ) %>%
  reduce(left_join,by = c("gameId","playId")) %>%
  left_join(
    plays_clean %>%
      select(gameId,playId,defensiveTeam),
    by = c("gameId","playId")
  ) %>%
  mutate(pred = ifelse(pred1 > .5,"1-High","2-High"),
         match = (actual == pred)) %>%
  filter(formation %in% c("empty open","3x1 open","2x2 open","trey")) %>%
  group_by(actual,formation,defensiveTeam) %>%
  summarise(
    count = n(),
    correct = sum(match),
    .groups = "drop"
  ) %>%
  mutate(pct = correct/count) %>%
  filter(formation == "3x1 open") %>%
  arrange(pct)
