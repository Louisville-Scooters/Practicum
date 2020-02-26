##########################################################################
# This script is for aggregating scooter rides (currently open data only) by census tract
# It:
# 1. Counts the number of origins and destinations for trips in each census tract
##########################################################################

### Open Data ----
# Count origins for each census tract
CH_open_origins_ct <- CH_Census_ct %>% 
  mutate(origins_cnt = lengths(st_intersects(., CH_scooter_0619)))

# Count dests for each census tract ##not working for CH yet since not all trip dest can't be joined to street (some are trails)
#CH_open_dests_ct <- CH_Census_ct %>% 
#  mutate(dests_cnt = lengths(st_intersects(., CH_open_dests)))

CH_open_ct <- CH_open_origins_ct

# Combine
# CH_open_ct <- CH_open_origins_ct %>% 
#   left_join(CH_open_dests_ct %>% 
#               st_drop_geometry() %>%
#               dplyr::select(GEOID, dests_cnt),
#             by = "GEOID")

