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
  filter(between(pred0,.45,.55),formation == "2x2 open") %>%
  slice_sample(n = 1) %>%
  print(width = Inf);play_animator(ds, play_selection$gameId, play_selection$playId, play_details, color_scheme)

tidyr::crossing(
  spot_y = seq(80,30,by = -1),
  distance = seq(1,15,by = 1),
  down = seq(1,4,by = 1)
)

synthetic_preds <- read_csv("synthetic_preds.csv")

synthetic_preds %>%
  mutate(across(DOWN,scales::ordinal)) %>%
  filter(DOWN != "1st" | YARDSTOGO %in% c(5,10,15)) %>%
  ggplot(mapping = aes(x = SPOT_Y,y = pred1,color = YARDSTOGO,group = paste(DOWN,YARDSTOGO)))+
  facet_wrap(vars(DOWN))+
  geom_point(size = 3)+
  scale_x_reverse()+
  labs(x = "Spot",y = "1-High Odds",color = "Distance")+
  theme(
    axis.title.y = element_text(angle = 0,vjust = .5),
    strip.background = element_rect(fill = "dodgerblue4"),
    strip.text = element_text(size = 14,color = "white",face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )+
  scale_y_continuous(labels = scales::percent,limits = c(.5,.6))
