##########################################################################
# This script:
# 1. Cleans the raw MNP census data and makes new columns
#
# If we standardize all the new census columns across cities at some later point,
# this should be deleted and merged into "LV - 20 - Collect Census Data"
##########################################################################

MNP_Census <- MNP_Census_raw %>% 
  mutate(pWhite = White_Pop / TotPop,
         Mean_Commute_Time = Total_Travel_Time / Num_Commuters,
         pTrans = Total_Public_Trans / Means_of_Transport_pop,
         pDrive = Total_cartruckvan/Means_of_Transport_pop,
         pFemale = TotFemale/TotPop,
         pCom30plus = (Travel_Time_3034 + Travel_Time_3539 + Travel_Time_4044 + Travel_Time_4559 +
                         Travel_Time_6089 + Travel_Time_90plus) / Total_Travel_Time,
         pOccupied = Occupied/Total_occupancy,
         pVehAvai = 1 - No_vehicle / Vehicle_own_pop)

names(MNP_Census)

MNP_Census <- MNP_Census %>%
  dplyr::select(GEOID, TotPop, TotHseUni, MdHHInc, MdAge, MedValue, MedRent, pWhite, Mean_Commute_Time,
                pTrans, pDrive, pFemale, pCom30plus, pOccupied, pVehAvai)

MNP_tract_list <- MNP_Census_geoinfo$GEOID

MNP_Census_ct <- MNP_Census %>%
  filter(MNP_Census$GEOID %in% MNP_tract_list) %>%
  st_set_geometry(NULL)

# rejoin geometry infor from ct_LV
MNP_Census_ct <- merge(MNP_Census_geoinfo, MNP_Census_ct, by = 'GEOID')
