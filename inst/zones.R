depth_by_level <- depth_by_play %>% 
  group_by(gameId,playId) %>% 
  mutate(depth_rk = data.table::frankv(depth,order = 1,ties.method = "min")) %>% 
  ungroup %>% 
  filter(depth_rk <= 7) %>% 
  mutate(level = ifelse(depth < -15,2,1)) %>% 
  group_by(gameId,playId,level) %>% 
  mutate(width_rk = data.table::frankv(width,order = 1,ties.method = "min")) %>% 
  ungroup %>% 
  group_by(pff_passCoverage,level,width_rk) %>% 
  summarise(
    count = n(),
    across(c(depth,width),mean),
    .groups = "drop"
  )

depth_by_level %>% 
  group_by(pff_passCoverage) %>% 
  mutate(rk = data.table::frankv(count,order = -1,ties.method = "min")) %>% 
  ungroup %>% 
  filter(rk <= 11) %>% 
  group_by(pff_passCoverage) %>% 
  slice(1:11) %>% 
  ungroup %>% 
  ggplot(mapping = aes(x = width,y = depth))+
  geom_hline(yintercept = 0,lty = 3)+
  geom_vline(xintercept = 0,lty = 3)+
  geom_vline(xintercept = -3.081667,lty = 2)+
  geom_vline(xintercept = 3.081667,lty = 2)+
  geom_point(mapping = aes(size = count,color = factor(level)))+
  geom_text(mapping = aes(label = width_rk),color = "black")+
  facet_wrap(vars(pff_passCoverage))+
  scale_size_continuous(range = c(2,15))+
  scale_y_continuous(limits = c(-20,0))

#### zone grid ####

x_breaks <- c(-50,-10,10,50)
y_breaks <- c(-100, -20, -10, 0, 15)

zone_grid <- depth_by_play %>% 
  mutate(
    xzone = cut(width,breaks = x_breaks,labels = c("left_third","middle","right_third")),
    yzone = cut(depth,breaks = y_breaks,labels = c("real deep", "deep", "hooks", "blitz"))
  ) %>% 
  count(xzone,yzone) %>% 
  mutate(
    zone = c(
      "left third","left third","left flat","left corner blitz",
      "deep middle","deep middle","hooks","blitz",
      "right third","right third","right flat","right corner blitz"
    ),
    sym_zone = c(
      "third","third","flat","blitz",
      "middle","middle","hooks","blitz",
      "third","third","flat","blitz"
    )
  )

depth_by_zone <- depth_by_play %>% 
  mutate(
    xzone = cut(width,breaks = x_breaks,labels = c("left_third","middle","right_third")),
    yzone = cut(depth,breaks = y_breaks,labels = c("real deep", "deep", "hooks", "blitz"))
  ) %>% 
  left_join(
    zone_grid,
    by = c("xzone","yzone")
  ) %>% 
  group_by(gameId,playId) %>% 
  mutate(depth_rk = data.table::frankv(depth,order = 1,ties.method = "min")) %>% 
  ungroup %>% 
  filter(depth_rk <= 7) %>% 
  group_by(gameId,playId,zone,sym_zone) %>% 
  mutate(width_rk = data.table::frankv(width,order = 1,ties.method = "min")) %>% 
  ungroup %>% 
  group_by(pff_passCoverage,zone,sym_zone,width_rk) %>% 
  summarise(
    count = n(),
    across(c(depth,width),mean),
    .groups = "drop"
  )

zone_tiles <- zone_grid %>% 
  distinct(zone,sym_zone) %>% 
  mutate(
    x = c(-13.75,-13.75,-13.75,0,0,0,13.75,13.75,13.75),
    y = c(-15,-5,2.5,-15,-5,2.5,-15,-5,2.5),
    w = c(12.5,12.5,12.5,15,15,15,12.5,12.5,12.5),
    h = c(10,10,5,10,10,5,10,10,5)
  )

zone_tiles2 <- zone_grid %>% 
  distinct(zone,sym_zone) %>% 
  mutate(
    x = c(-15,-15,-15,0,0,0,15,15,15),
    y = c(-15,-5,2.5,-17.5,-7.5,2.5,-15,-5,2.5),
    w = c(10,10,10,20,20,20,10,10,10),
    h = c(10,10,5,5,15,5,10,10,5)
  )

zone_tiles_plot <- geom_tile(
    data = zone_tiles2,
    mapping = aes(x = x, y = y, width = w,height = h,fill = sym_zone),
    color = "black",
    alpha = .7
  )

depth_by_zone %>% 
  group_by(pff_passCoverage) %>% 
  mutate(rk = data.table::frankv(count,order = -1,ties.method = "min")) %>% 
  ungroup %>% 
  filter(rk <= 7) %>% 
  group_by(pff_passCoverage) %>% 
  slice(1:7) %>% 
  mutate(pct = count/sum(count)) %>% 
  ungroup %>% #filter(pff_passCoverage == "Cover-3")
  ggplot(mapping = aes(x = width,y = depth))+
  zone_tiles_plot + 
  geom_hline(yintercept = 0,lty = 3)+
  geom_vline(xintercept = 0,lty = 3)+
  geom_vline(xintercept = -3.081667,lty = 2)+
  geom_vline(xintercept = 3.081667,lty = 2)+
  geom_point(mapping = aes(size = pct,group = zone),
             show.legend = F,color = "black")+
  facet_wrap(vars(pff_passCoverage))+
  scale_size_continuous(range = c(7,10))+
  labs(fill = "")+
  theme(strip.text = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 10))

#### model ####

zone_counts_by_play <- depth_by_play %>% 
  mutate(
    xzone = cut(width,breaks = c(-50,-10,10,50),labels = c("left_third","middle","right_third")),
    yzone = cut(depth,breaks = c(-100, -20, -10, 0, 15),labels = c("real deep", "deep", "hooks", "blitz"))
  ) %>% 
  left_join(
    zone_grid,
    by = c("xzone","yzone")
  ) %>% 
  group_by(gameId,playId) %>% 
  mutate(depth_rk = data.table::frankv(depth,order = 1,ties.method = "min")) %>% 
  ungroup %>% 
  filter(depth_rk <= 7) %>% 
  group_by(gameId,playId,pff_passCoverage,zone,sym_zone) %>% 
  summarise(
    count = n(),
    .groups = "drop"
  )
  
zone_counts_by_play %>% 
  group_by(pff_passCoverage,sym_zone) %>% 
  summarise(
    plays = n_distinct(paste(gameId,playId)),
    count = mean(count),
    .groups = "drop"
  ) %>% 
  ggplot(mapping = aes(x = pff_passCoverage,y = count,fill = sym_zone))+
  geom_bar(stat = "identity",color = "black",position = "dodge")

model_df <- zone_counts_by_play %>% 
  pivot_wider(
    id_cols = c(gameId,playId,pff_passCoverage),
    names_from = sym_zone,
    values_from = count,
    values_fn = sum,
    values_fill = 0
  ) %>% 
  mutate(hierarchy = ifelse(pff_passCoverage %in% c("Cover-3","Cover-1"),1,0))

set.seed(15)
model_split <- initial_split(model_df,prop = .8)
model_train <- training(model_split)
train_split <- initial_split(model_train,prop = .75)
model_ana <- training(train_split)
model_assess <- testing(train_split)
model_test <- testing(model_split)

feature_cols <- c("blitz", "middle", "hooks", "flat")

possible_zone_players <- seq(1,7,by = 1)

grid <- crossing(
  blitz = possible_zone_players,
  middle = possible_zone_players,
  hooks = possible_zone_players,
  flat = possible_zone_players,
  third = possible_zone_players
) %>% 
  mutate(total = rowSums(.)) %>% 
  filter(total == 7) %>% 
  select(-total)

logistic_fit <- model_ana %>% 
  glm(formula = hierarchy ~ blitz + middle + hooks + flat,
      family = 'binomial')

logistic_preds <- list(
  model_ana,
  model_assess
) %>% 
  list(c("ana","assess")) %>% 
  pmap(function(x,y) mutate(x,set = y)) %>% 
  bind_rows %>% 
  mutate(pred = predict(logistic_fit,.,type = "response"),
         pred = ifelse(pred > .5,1,0))

logistic_preds %>% 
  group_by(set) %>% 
  summarise(
    count = n(),
    correct = sum(hierarchy == pred),
    .groups = "drop"
  ) %>% 
  mutate(accuracy = correct/count)

logistic_fit_full <- model_train %>% 
  glm(formula = hierarchy ~ blitz + middle + hooks + flat,
      family = 'binomial')

logistic_preds <- list(
  model_train,
  model_test
) %>% 
  list(c("train","test")) %>% 
  pmap(function(x,y) mutate(x,set = y)) %>% 
  bind_rows %>% 
  mutate(pred = predict(logistic_fit_full,.,type = "response"),
         pred = ifelse(pred > .5,1,0))

logistic_preds %>% 
  group_by(set) %>% 
  summarise(
    count = n(),
    correct = sum(hierarchy == pred),
    .groups = "drop"
  ) %>% 
  mutate(accuracy = correct/count)
