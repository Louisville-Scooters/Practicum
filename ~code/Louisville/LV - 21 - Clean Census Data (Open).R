##########################################################################
# This script:
# 1. Cleans the raw LV census data and makes new columns
#
# If we standardize all the new census columns across cities at some later point,
# this should be deleted and merged into "LV - 20 - Collect Census Data"
##########################################################################

LV_Census <- LV_Census_raw %>% 
  mutate(Percent_White = White_Pop / Total_Pop,
         Mean_Commute_Time = Total_Travel_Time / Num_Commuters,
         Percent_Taking_Public_Trans = Total_Public_Trans / Means_of_Transport_pop,
         Percent_vehicle_available = 1 - No_vehicle / Vehicle_own_pop)