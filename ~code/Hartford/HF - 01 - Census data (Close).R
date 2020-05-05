# collect the data for 8 cities in U.S.
## Philadelphia ####
# Read and input census key
census_key_RDS <- file.path(data_directory,
                            "~RData/Census/EC_census_key")
census_key <- readRDS(census_key_RDS)
tidycensus::census_api_key(census_key, install = TRUE, overwrite = TRUE)

# Collect census data and geometries
HF_Census_raw <- get_acs(geography = "tract", 
                         variables = census_vars, 
                         year = 2018, 
                         state = "CT", 
                         geometry = TRUE, 
                         county = c("Hartford County"),
                         output = "wide") %>%
  rename_census_cols %>%
  dplyr::select(GEOID, 
                geometry,
                census_colNames) %>% 
  st_transform(2272)

# Hartford is different from other cities.
Hartford <- st_join( HF_Boundary, HF_Census_raw %>% dplyr::select(GEOID, geometry),st_intersects) 
Hartford <- st_join(HF_Census_raw %>% dplyr::select(GEOID, geometry), HF_Boundary, st_intersects) %>%
  na.omit()
HF_GEOID <- Hartford[Hartford$GEOID %in% unique(Hartford$GEOID),]
HF_GEOID <- HF_GEOID[!duplicated(HF_GEOID$GEOID), ]
HF_Boundary <- st_read('https://opendata.arcgis.com/datasets/21e2d33b658145349cfe4b17e03efef2_6.geojson') %>%
  st_transform(2272)

ggplot()+
  geom_sf(data=HF_Census_raw)

HF_Census_geoinfo <- HF_Census_raw %>%
  dplyr::select(GEOID, geometry) %>%
  na.omit()

# extract centroid of each census tract
HF_Census_geoinfo <- HF_Census_geoinfo %>% 
  mutate(centroid_X = st_coordinates(st_centroid(HF_Census_geoinfo))[, 1],
         centroid_Y = st_coordinates(st_centroid(HF_Census_geoinfo))[, 2])

HF_Census <- HF_Census_raw %>% 
  subset(HF_Census_raw$GEOID %in% HF_GEOID$GEOID) %>%
  mutate(pWhite = White_Pop / TotPop,
         Mean_Commute_Time = Total_Travel_Time / Num_Commuters,
         pTrans = Total_Public_Trans / Means_of_Transport_pop,
         pDrive = Total_cartruckvan/Means_of_Transport_pop,
         pFemale = TotFemale/TotPop,
         pCom30plus = (Travel_Time_3034 + Travel_Time_3539 + Travel_Time_4044 + Travel_Time_4559 +
                         Travel_Time_6089 + Travel_Time_90plus) / Total_Travel_Time,
         pOccupied = Occupied/Total_occupancy,
         pVehAvai = 1 - No_vehicle / Vehicle_own_pop)

# names(HF_Census)

HF_Census <- HF_Census %>%
  dplyr::select(GEOID, TotPop, TotHseUni, MdHHInc, MdAge, MedValue, MedRent, pWhite, Mean_Commute_Time,
                pTrans, pDrive, pFemale, pCom30plus, pOccupied, pVehAvai)

HF_tract_list <- HF_Census_geoinfo$GEOID

HF_Census_ct <- HF_Census %>%
  filter(HF_Census$GEOID %in% HF_tract_list) %>%
  st_set_geometry(NULL)

# rejoin geometry infor from ct_HS
HF_Census_ct <- merge(HF_Census_geoinfo, HF_Census_ct, by = 'GEOID')





