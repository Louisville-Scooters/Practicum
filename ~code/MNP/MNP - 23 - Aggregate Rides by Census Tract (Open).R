##########################################################################
# This script is for aggregating scooter rides (currently open data only) by census tract
# It:
# 1. Counts the number of origins and destinations for trips in each census tract
##########################################################################

### Open Data ----
# Count origins for each census tract
MNP_open_origins_ct <- MNP_Census_ct %>% 
  mutate(origins_cnt = lengths(st_intersects(., MNP_open_origins)))

# Count dests for each census tract
MNP_open_dests_ct <- MNP_Census_ct %>% 
  mutate(dests_cnt = lengths(st_intersects(., MNP_open_dests)))

# Combine
MNP_open_ct <- MNP_open_origins_ct %>% 
  left_join(MNP_open_dests_ct %>% 
              st_drop_geometry() %>%
              dplyr::select(GEOID, dests_cnt),
            by = "GEOID")

