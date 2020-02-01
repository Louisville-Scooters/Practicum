


# 
LV_rebal_user_only <- LV_rebal_sf %>% 
  filter(str_detect(reason, "user"))
