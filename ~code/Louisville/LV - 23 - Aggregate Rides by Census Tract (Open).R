##########################################################################
# This script is for aggregating scooter rides (currently open data only) by census tract
# It:
# 1. Counts the number of origins and destinations for trips in each census tract
##########################################################################

### Open Data ----
# Count origins for each census tract

LV_open_07to09_sf <- LV_open_origins %>%
  filter(StartDate >= '2019-07-01' & StartDate < '2019-10-01')

LV_open_origins_ct <- LV_Census_ct %>% 
  mutate(origins_cnt = (lengths(st_intersects(., LV_open_07to09_sf)))/3)

# Count dests for each census tract
#LV_open_dests_ct <- LV_Census_ct %>% 
#  mutate(dests_cnt = lengths(st_intersects(., LV_open_dests)))

# Combine
# LV_open_ct <- LV_open_origins_ct %>% 
#   left_join(LV_open_dests_ct %>% 
#               st_drop_geometry() %>%
#               dplyr::select(GEOID, dests_cnt),
#             by = "GEOID")

LV_open_ct <- LV_open_origins_ct
