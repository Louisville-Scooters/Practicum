##########################################################################
# This script:
# 1. Installs a Census API key
# 2. Collects Census data and geometries for Minneapolis See "02 - Census Vars and Functions.R" for variable list.
# 3. Projects the geometry to MNP_proj
##########################################################################

# Read and input census key
census_key_RDS <- file.path(data_directory,
                            "~RData/Census/EC_census_key")
census_key <- readRDS(census_key_RDS)
tidycensus::census_api_key(census_key, install = TRUE, overwrite = TRUE)

# Collect census data and geometries
MNP_Census_raw <- get_acs(geography = "tract", 
                         variables = census_vars, 
                         year = 2018, 
                         state = "MN", 
                         geometry = TRUE, 
                         county = c("Hennepin"),
                         output = "wide") %>%
  rename_census_cols %>%
  dplyr::select(GEOID, 
                geometry,
                census_colNames) %>% 
  st_transform(MNP_proj)

MNP_Census_geoinfo <- MNP_Census_raw %>%
  dplyr::select(GEOID, geometry) %>%
  st_intersection(MNP_ct %>% dplyr::select(geometry))

# extract centroid of each census tract
MNP_Census_geoinfo <- MNP_Census_geoinfo %>% 
  mutate(centroid_X = st_coordinates(st_centroid(MNP_Census_geoinfo))[, 1],
         centroid_Y = st_coordinates(st_centroid(MNP_Census_geoinfo))[, 2])

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

# names(MNP_Census)

MNP_Census <- MNP_Census %>%
  dplyr::select(GEOID, TotPop, TotHseUni, MdHHInc, MdAge, MedValue, MedRent, pWhite, Mean_Commute_Time,
                pTrans, pDrive, pFemale, pCom30plus, pOccupied, pVehAvai)
