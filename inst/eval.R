
preds <- "preds/hierarchy_preds_20250302.csv" %>% 
  read_csv

preds %>% 
  mutate(pred_binary = ifelse(pred > .5,1,0)) %>% 
  list(
    crossing(
      pred_binary = c(1,0),
      actual = c(1,0)
    ) %>% 
      mutate(class = c("2 High","Wrong","Wrong","1 High"))
  ) %>% 
  reduce(left_join,by = c("pred_binary","actual")) %>% 
  ggplot(mapping = aes(x = pred, fill = class))+
  geom_density(alpha = .8)

play_selection <- preds %>% 
  filter(actual == 0,pred > .5) %>% 
  arrange(desc(pred)) %>% 
  slice(4) %>% 
  rename(gameId = GAMEID,playId = PLAYID) %>% 
  mutate(across(c(gameId,playId),as.integer))

play_selection

play_animator(ds, play_selection$gameId, play_selection$playId, play_details, color_scheme, field_lines)

#### preds comp #### 

preds <- list.files(pattern = "preds") %>% 
  .[str_detect(.,"hierarchy",negate = T)] %>% 
  map(function(x) {
    
    model_type <- str_extract(x,".+preds") %>% sub("_preds","",.)
    
    print(model_type)
    
    date_created <- str_extract(x,"\\d{8}") %>% as.Date("%Y%m%d")
    
    read_csv(x,col_types = cols("actual" = "c")) %>% 
      setNames(tolower(colnames(.))) %>% 
      pivot_longer(cols = matches("pred"),names_sep = "pred",names_to = c("blah","class"),values_to = "odds") %>% 
      mutate(
        model = model_type,
        date = date_created,
        across(class,~replace_na(.,1))
      ) %>% 
      select(model,date,set,clip_id,class,actual,odds)
    
  }) %>% 
  bind_rows %>% 
  group_by(model,clip_id) %>% 
  filter(date == max(date)) %>% 
  ungroup

class_mappings <- list.files(pattern = "mapping") %>% 
  map(function(x) {
    
    model_type <- str_extract(x,".+mapping") %>% sub("_mapping","",.)
    
    read_csv(x,col_types = cols("CLASS" = "c")) %>% 
      mutate(model = model_type)
    
  }) %>% 
  bind_rows

preds_mapped <- preds %>% 
  filter(set == "assess") %>% 
  group_by(model,clip_id) %>% 
  filter(odds == max(odds)) %>% 
  ungroup %>% 
  list(
    class_mappings %>% 
      select(class = CLASS,model,coverages)
  ) %>% 
  list(
    left_join(.[[1]],.[[2]],by = c("actual" = "class","model")) %>% 
      rename(actual_coverage = coverages),
    left_join(.[[1]],.[[2]],by = c("class","model")) %>% 
      rename(pred_coverage = coverages) %>% 
      select(model,date,clip_id,pred_coverage)
  ) %>% 
  .[-1] %>% 
  reduce(left_join,by = c("model","date","clip_id")) %>% 
  mutate(match = (actual_coverage == pred_coverage)) %>% 
  mutate(across(actual_coverage,~ifelse(. == "Cover-2; Quarters; Cover-6; 2-Man","2-High",.))) %>% 
  mutate(across(actual_coverage,~ifelse(. == "Cover-3; Cover-1","1-High",.)))
  
preds_mapped %>% 
  count(model,actual_coverage,pred_coverage) %>% 
  group_by(model,actual_coverage) %>% 
  mutate(pct = n/sum(n)) %>% 
  ungroup %>% 
  pivot_wider(id_cols = c(model,actual_coverage),names_from = pred_coverage,values_from = pct,values_fill = 0)
  
preds_mapped %>% 
  count(model,actual_coverage,pred_coverage) %>% 
  mutate(across(model,~ifelse(model != "all_class","hierarchical",.))) %>% 
  group_by(model,actual_coverage) %>% 
  mutate(pct = n/sum(n)) %>% 
  ungroup %>% 
  ggplot(mapping = aes(x = actual_coverage,y = pct,fill = pred_coverage))+
  facet_wrap(vars(model),nrow = 2)+
  geom_bar(position = "dodge",stat = "identity",color = "black")+
  theme(axis.text.x = element_text(size = 10))+
  scale_y_continuous(limits = c(0,1),labels = scales::percent)

preds_mapped %>% 
  count(model,actual_coverage,pred_coverage) %>% 
  mutate(across(model,~ifelse(model != "all_class","hierarchical",.))) %>% 
  group_by(model,actual_coverage) %>% 
  mutate(pct = n/sum(n)) %>% 
  ungroup %>% 
  ggplot(mapping = aes(x = actual_coverage,y = n,fill = pred_coverage))+
  facet_wrap(vars(model),nrow = 2)+
  geom_bar(position = "dodge",stat = "identity",color = "black")+
  theme(axis.text.x = element_text(size = 10))

preds_mapped %>% 
  mutate(across(model,~ifelse(model != "all_class","hierarchical",.))) %>% 
  #count(model,actual_coverage,pred_coverage) %>% 
  mutate(match = (actual_coverage == pred_coverage)) %>% 
  group_by(model,actual_coverage) %>% 
  summarise(
    count = n(),
    correct = sum(match),
    .groups = "drop"
  ) %>% 
  group_by(model) %>% 
  summarise(across(c(count,correct),sum)) %>% 
  mutate(pct = correct/count)
