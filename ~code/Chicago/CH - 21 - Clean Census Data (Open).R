##########################################################################
# This script:
# 1. Cleans the raw CH census data and makes new columns
#
# If we standardize all the new census columns across cities at some later point,
# this should be deleted and merged into "LV - 20 - Collect Census Data"
##########################################################################

CH_Census <- CH_Census_raw %>% 
  mutate(pWhite = White_Pop / TotPop,
         Mean_Commute_Time = Total_Travel_Time / Num_Commuters,
         pTrans = Total_Public_Trans / Means_of_Transport_pop,
         pDrive = Total_cartruckvan/Means_of_Transport_pop,
         pFemale = TotFemale/TotPop,
         pCom30plus = (Travel_Time_3034 + Travel_Time_3539 + Travel_Time_4044 + Travel_Time_4559 +
                         Travel_Time_6089 + Travel_Time_90plus) / Total_Travel_Time,
         pOccupied = Occupied/Total_occupancy,
         pVehAvai = 1 - No_vehicle / Vehicle_own_pop)

names(CH_Census)

CH_Census <- CH_Census %>%
  dplyr::select(GEOID, TotPop, TotHseUni, MdHHInc, MdAge, MedValue, MedRent, pWhite, Mean_Commute_Time,
                pTrans, pDrive, pFemale, pCom30plus, pOccupied, pVehAvai)

CH_Census_ct <- CH_Census %>%
  filter(CH_Census$GEOID %in% CH_tract_list) %>%
  st_set_geometry(NULL)

# rejoin geometry infor from ct_LV
CH_Census_ct <- merge(CH_ct, CH_Census_ct, by.x = 'geoid10', by.y = 'GEOID')

names(CH_Census_ct)

CH_Census_ct <- CH_Census_ct %>%
  dplyr::select(-c(commarea, commarea_n, countyfp10, name10, namelsad10, notes, statefp10))
