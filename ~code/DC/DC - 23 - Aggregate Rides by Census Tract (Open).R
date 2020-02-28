##########################################################################
# This script is for aggregating scooter rides (currently open data only) by census tract
# It:
# 1. Counts the number of origins and destinations for trips in each census tract
##########################################################################

### Open Data ----
# Count origins for eaDC census tract
DC_open_origins_ct <- DC_Census_ct %>% 
  mutate(origins_cnt = (lengths(st_intersects(., DC_scooter_07to09_sf)))/3)

# Count dests for eaDC census tract ##not working for DC yet since not all trip dest can't be joined to street (some are trails)
#DC_open_dests_ct <- DC_Census_ct %>% 
#  mutate(dests_cnt = lengths(st_intersects(., DC_open_dests)))

DC_open_ct <- DC_open_origins_ct

# Combine
# DC_open_ct <- DC_open_origins_ct %>% 
#   left_join(DC_open_dests_ct %>% 
#               st_drop_geometry() %>%
#               dplyr::select(GEOID, dests_cnt),
#             by = "GEOID")

