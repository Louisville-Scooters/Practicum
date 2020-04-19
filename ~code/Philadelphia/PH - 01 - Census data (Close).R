# collect the data for 8 cities in U.S.
## Philadelphia ####
# Read and input census key
census_key_RDS <- file.path(data_directory,
                            "~RData/Census/EC_census_key")
census_key <- readRDS(census_key_RDS)
tidycensus::census_api_key(census_key, install = TRUE, overwrite = TRUE)

# Collect census data and geometries
PH_Census_raw <- get_acs(geography = "tract", 
                         variables = census_vars, 
                         year = 2018, 
                         state = "PA", 
                         geometry = TRUE, 
                         county = c("Philadelphia"),
                         output = "wide") %>%
  rename_census_cols %>%
  dplyr::select(GEOID, 
                geometry,
                census_colNames) %>% 
  st_transform(2272)

PH_Census_geoinfo <- PH_Census_raw %>%
  dplyr::select(GEOID, geometry) %>%
  na.omit()

# extract centroid of each census tract
PH_Census_geoinfo <- PH_Census_geoinfo %>% 
  mutate(centroid_X = st_coordinates(st_centroid(PH_Census_geoinfo))[, 1],
         centroid_Y = st_coordinates(st_centroid(PH_Census_geoinfo))[, 2])

PH_Census <- PH_Census_raw %>% 
  mutate(pWhite = White_Pop / TotPop,
         Mean_Commute_Time = Total_Travel_Time / Num_Commuters,
         pTrans = Total_Public_Trans / Means_of_Transport_pop,
         pDrive = Total_cartruckvan/Means_of_Transport_pop,
         pFemale = TotFemale/TotPop,
         pCom30plus = (Travel_Time_3034 + Travel_Time_3539 + Travel_Time_4044 + Travel_Time_4559 +
                         Travel_Time_6089 + Travel_Time_90plus) / Total_Travel_Time,
         pOccupied = Occupied/Total_occupancy,
         pVehAvai = 1 - No_vehicle / Vehicle_own_pop)

# names(PH_Census)

PH_Census <- PH_Census %>%
  dplyr::select(GEOID, TotPop, TotHseUni, MdHHInc, MdAge, MedValue, MedRent, pWhite, Mean_Commute_Time,
                pTrans, pDrive, pFemale, pCom30plus, pOccupied, pVehAvai)

PH_tract_list <- PH_Census_geoinfo$GEOID

PH_Census_ct <- PH_Census %>%
  filter(PH_Census$GEOID %in% PH_tract_list) %>%
  st_set_geometry(NULL)

# rejoin geometry infor from ct_PH
PH_Census_ct <- merge(PH_Census_geoinfo, PH_Census_ct, by = 'GEOID')





