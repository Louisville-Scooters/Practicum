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

# Keep 2019 June data only
MNP_scooter_0619 <- MNP_scooter_data %>%
  filter(dataset == "/Motorized_Foot_Scooter_Trips_June_2019")

# Join geographical information based on street centerline
MNP_scooter_0619_sf <- merge(MNP_scooter_0619, MNP_street_sf, by.x = 'startcenterlineid', by.y = 'GBSID', all.x = TRUE)

