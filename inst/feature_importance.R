custom_theme <- ttheme_default(
  core = list(
    bg_params = list(
      # Alternating row colors
      fill = c("white", "lightgray")
    ),
    fg_params = list(fontface = "plain", fontsize = 12) # Regular text
  ),
  colhead = list(
    fg_params = list(fontface = "bold", col = "white"),  # White text
    bg_params = list(fill = "dodgerblue4")                     # Black background
  )
)

hierarchy_preds %>%
  mutate(pred = ifelse(pred1 > .5,"1-High","2-High"),match = (actual == pred),
         across(set,~ifelse(. %in% c("ana","assess"),"train",.)),
         across(set,~factor(.,levels = c("train","test")))) %>%
  group_by(set,actual) %>%
  summarise(count = n(),correct = sum(match),.groups = "drop") %>%
  mutate(accuracy = correct/count) %>%
  select(set,actual,count,accuracy) %>%
  mutate(
    across(count,~scales::number(.,big.mark = ",")),
    across(accuracy,~scales::percent(round(.,3)))
  ) %>%
  grid.table(
    theme = custom_theme,
    rows = NULL
  )

feature_importances <- "feature_importances.csv" %>%
  read_csv

dev.off();feature_importances %>%
  mutate(across(c(importance_mean,importance_sd),scales::scientific)) %>%
  mutate(across(importance_sd,~tidyr::replace_na(.,""))) %>%
  magrittr::set_names(c("category","importance mean","importance sd")) %>%
  grid.table(
    theme = custom_theme,
    rows = NULL
  )
