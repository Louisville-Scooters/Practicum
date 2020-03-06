##########################################################################
# This script is for aggregating scooter rides (currently open data only) by census tract
# It:
# 1. Counts the number of origins and destinations for trips in each census tract
##########################################################################

### Open Data ----
# Count origins for each census tract
KC_scooter_ct_origins <- KC_scooter_ct %>%
  mutate(month = month(KC_scooter_ct$start_time),
         year = year(KC_scooter_ct$start_time))

KC_cnt_origins <- KC_scooter_ct_origins %>%
  subset((month %in% c(7,8,9)) & (year==2019)) %>%
  st_intersection(KC_Census_geoinfo)
  
KC_open_origins_ct <- KC_cnt_origins %>% 
  group_by(Start.Census.Tract) %>%
  summarise(origins_cnt = n())

# Count dests for each census tract
KC_open_dests_ct <- KC_cnt_origins %>% 
  group_by(End.Census.Tract) %>%
  summarise(dests_cnt = n())

# Combine
KC_open_ct <- KC_open_origins_ct %>% 
  left_join(KC_open_dests_ct %>% 
              st_drop_geometry() %>%
              dplyr::select(End.Census.Tract, dests_cnt),
            by = c("Start.Census.Tract"="End.Census.Tract"))

