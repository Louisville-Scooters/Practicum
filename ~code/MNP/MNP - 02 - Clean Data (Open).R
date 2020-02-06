##########################################################################
# This script is for cleaning the MNP scooter data
# It:
# 1. Adds some helper columns: start and end dates and times
# 2. Links geographical information to the data based on streetcenterlineID
##########################################################################

# Add helper columns ----
MNP_scooter_data <- MNP_scooter_data_raw %>% 
  mutate(# this is the start time from the original data. Some data includes the date, but others only have the time
    start_date = date(starttime),
    start_time = hour(starttime),
    end_date = date(endtime),
    end_time = hour(endtime))

# Clean the MNP_street data
MNP_street_sf <- MNP_street %>%
  dplyr::select(NUM_WALKS, GBSID, BIKE_LANE, TRAFFIC_DI, SPEED_LIM, BUS_ROUTE, SNOW_EMERG, SEGMENT_LE, ROUTE_TYPE,
                OFT, STREET_TYP, geometry)

MNP_street_sf$GBSID <- as.character(MNP_street_sf$GBSID)

MNP_street_unique <- MNP_street_sf[!duplicated(MNP_street_sf$GBSID),]

MNP_street_unique <- MNP_street_unique %>%
  mutate(centroid_X = st_coordinates(st_centroid(MNP_street_unique))[,1],
         centroid_Y = st_coordinates(st_centroid(MNP_street_unique))[, 2])

MNP_street_centroid <- st_as_sf(MNP_street_unique %>% st_set_geometry(NULL), coords = c('centroid_X', 'centroid_Y'), crs = 26849)

# Keep 2019 June data only
MNP_scooter_0619 <- MNP_scooter_data %>%
  filter(dataset == "/Motorized_Foot_Scooter_Trips_June_2019")

# Join geographical information based on street centerline
MNP_scooter_0619_sf <- merge(MNP_scooter_0619, MNP_street_centroid, by.x = 'startcenterlineid', by.y = 'GBSID')

MNP_scooter_0619_sf <- st_as_sf(MNP_scooter_0619_sf, crs = MNP_proj)

#still cannot join endcenterline id
MNP_scooter_0619_final <- merge(MNP_scooter_0619_sf, MNP_street_centroid %>% dplyr::select(GBSID, geometry), by.x = 'endcenterlineid', by.y = 'GBSID')
names(MNP_scooter_0619_sf)
