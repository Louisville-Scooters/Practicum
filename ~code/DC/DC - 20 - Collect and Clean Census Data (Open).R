##########################################################################
# This script:
# 1. Installs a Census API key
# 2. Collects Census data and geometries for DC See "02 - Census Vars and Functions.R" for variable list.
# 3. Projects the geometry to DC_proj
# 4. Cleans the raw DC census data and makes new columns
##########################################################################

# Collect census data and geometries
DC_Census_raw <- get_acs(geography = "tract", 
                         variables = census_vars, 
                         year = 2018, 
                         state = "DC", 
                         geometry = TRUE, 
                         # county=c("Travis"),
                         output = "wide") %>%
  rename_census_cols %>%
  dplyr::select(GEOID, 
                geometry,
                census_colNames) %>% 
  st_transform(DC_proj)

DC_Census_geoinfo <- DC_Census_raw %>%
  dplyr::select(GEOID, geometry)

# extract centroid of each census tract
DC_Census_geoinfo <- DC_Census_geoinfo %>% 
  mutate(centroid_X = st_coordinates(st_centroid(DC_Census_geoinfo))[, 1],
         centroid_Y = st_coordinates(st_centroid(DC_Census_geoinfo))[, 2])

DC_Census <- DC_Census_raw %>% 
  mutate(pWhite = White_Pop / TotPop,
         Mean_Commute_Time = Total_Travel_Time / Num_Commuters,
         pTrans = Total_Public_Trans / Means_of_Transport_pop,
         pDrive = Total_cartruckvan/Means_of_Transport_pop,
         pFemale = TotFemale/TotPop,
         pCom30plus = (Travel_Time_3034 + Travel_Time_3539 + Travel_Time_4044 + Travel_Time_4559 +
                         Travel_Time_6089 + Travel_Time_90plus) / Total_Travel_Time,
         pOccupied = Occupied/Total_occupancy,
         pVehAvai = 1 - No_vehicle / Vehicle_own_pop)

# names(DC_Census)

DC_Census <- DC_Census %>%
  dplyr::select(GEOID, TotPop, TotHseUni, MdHHInc, MdAge, MedValue, MedRent, pWhite, Mean_Commute_Time,
                pTrans, pDrive, pFemale, pCom30plus, pOccupied, pVehAvai)

DC_tract_list <- DC_Census_geoinfo$GEOID

DC_Census_ct <- DC_Census %>%
  filter(DC_Census$GEOID %in% DC_tract_list) %>%
  st_set_geometry(NULL)

# rejoin geometry infor from ct_LV
DC_Census_ct <- merge(DC_Census_geoinfo, DC_Census_ct, by = 'GEOID')
