##########################################################################
# This script is for calculating the opportunity index for Louisville:
# 1. Names a projection to use for LV
# 2. Re-projects the LV base map and service area in the new projection
# 3. Re-projects the rebal and open scooter data.
# 4. Filters out any rebal scooter data outside the service area
# 5. Defines a function for creating LV scooter origin and destination sf objects, 
#    which can be linked via the 'TripID' column to the rest of the dataset ('LV_open_raw')
#
# This script exports the following data:
# 1. 
##########################################################################

# June 2019 ----
# we want to know which census tract did these trips ended in.
LV_rebal_reb_only_0619_combined_rowPairs <- LV_rebal_reb_only_0619_combined_rowPairs %>% 
  mutate(week = week(end_time)) %>% 
  st_as_sf(sf_column_name = "trip_origin", crs = LV_proj) %>% 
  mutate(lon_s = st_coordinates(.)[1],
         lat_s = st_coordinates(.)[2]) %>% 
  as.data.frame() %>% 
  st_as_sf(sf_column_name = "trip_dest", crs = LV_proj) %>% 
  mutate(lon_d = st_coordinates(.)[1],
         lat_d = st_coordinates(.)[2]) %>% 
  as.data.frame()

LV_rebal_reb_only_0619_combined_rowPairs_sf_end <- st_as_sf(LV_rebal_reb_only_0619_combined_rowPairs, 
                                                        sf_column_name = "trip_dest", crs = LV_proj)

LV_rebal_reb_only_0619_combined_rowPairs_ct_end <- st_join(LV_rebal_reb_only_0619_combined_rowPairs_sf_end, 
                                                       LV_Census_geoinfo %>% 
                                                         dplyr::select(GEOID), st_within, left=T) %>% 
  rename(End.Census.Tract = GEOID)

LV_rebal_reb_only_0619_combined_rowPairs_sf_start <- st_as_sf(LV_rebal_reb_only_0619_combined_rowPairs, 
                                                            sf_column_name = "trip_origin", crs = LV_proj)

LV_rebal_reb_only_0619_combined_rowPairs_ct_start <- st_join(LV_rebal_reb_only_0619_combined_rowPairs_sf_start, 
                                                           LV_Census_geoinfo %>% 
                                                             dplyr::select(GEOID), st_within, left=T) %>% 
  rename(Start.Census.Tract = GEOID)

### calculating the opportunity index (rebalancing drop off and trips end) for each census tract by week
# first we focus on rebalancing drop off and pickup data
LV_reb_dropoff_ct <- LV_rebal_reb_only_0619_combined_rowPairs_ct_end %>%
  na.omit() %>%
  group_by(week, End.Census.Tract) %>%
  summarise(cnt_reb = n())

LV_reb_pickup_ct <- LV_rebal_reb_only_0619_combined_rowPairs_ct_start %>%
  na.omit() %>%
  group_by(week, Start.Census.Tract) %>%
  summarise(cnt_reb = n())

# users data ####
# LV_rebal_user_only_0619_combined_rowPairs could be obtained by running code LV - 03
# next, we turn to the user trips data
LV_rebal_user_only_0619_combined_rowPairs <- LV_rebal_user_only_0619_combined_rowPairs %>% 
  mutate(week = week(end_time)) %>% 
  st_as_sf(sf_column_name = "trip_origin", crs = LV_proj) %>% 
  mutate(lon_s = st_coordinates(.)[1],
         lat_s = st_coordinates(.)[2]) %>% 
  as.data.frame() %>% 
  st_as_sf(sf_column_name = "trip_dest", crs = LV_proj) %>% 
  mutate(lon_d = st_coordinates(.)[1],
         lat_d = st_coordinates(.)[2]) %>% 
  as.data.frame()

LV_rebal_user_only_0619_combined_rowPairs_sf_end <- st_as_sf(LV_rebal_user_only_0619_combined_rowPairs, 
                                                         sf_column_name = "trip_dest", crs = LV_proj)

LV_rebal_user_only_0619_combined_rowPairs_ct_end <- st_join(LV_rebal_user_only_0619_combined_rowPairs_sf_end, 
                                                        LV_Census_geoinfo %>% 
                                                          dplyr::select(GEOID), st_within, left=T) %>% 
  rename(End.Census.Tract = GEOID)

LV_rebal_user_only_0619_combined_rowPairs_sf_start <- st_as_sf(LV_rebal_user_only_0619_combined_rowPairs, 
                                                             sf_column_name = "trip_origin", crs = LV_proj)

LV_rebal_user_only_0619_combined_rowPairs_ct_start <- st_join(LV_rebal_user_only_0619_combined_rowPairs_sf_start, 
                                                            LV_Census_geoinfo %>% 
                                                              dplyr::select(GEOID), st_within, left=T) %>% 
  rename(Start.Census.Tract = GEOID)

# now the user dropoff and pickup data
LV_users_dropoff_ct <- LV_rebal_user_only_0619_combined_rowPairs_ct_end %>%
  na.omit() %>%
  group_by(week, End.Census.Tract) %>%
  summarise(cnt_user = n())

LV_users_pickup_ct <- LV_rebal_user_only_0619_combined_rowPairs_ct_start %>%
  na.omit() %>%
  group_by(week, Start.Census.Tract) %>%
  summarise(cnt_user = n())


### Create a panel ####
# rebalance_june contains all the trips (including rebalancing and users' etc.) happened in June, 2019
LV_rebal_sf_0619 <- LV_rebal_sf  %>%
  filter(year(occurredAt) == 2019, month(occurredAt) == 6) %>% 
  mutate(week = week(occurredAt))

LV_study.panel <- 
  expand.grid(week = unique(LV_rebal_sf_0619$week), 
              End.Census.Tract = unique(LV_Census_geoinfo$GEOID)) %>%
  mutate(End.Census.Tract = as.character(End.Census.Tract)) %>% 
  left_join(., LV_reb_dropoff_ct %>% st_set_geometry(NULL), how = 'left', by = c("week", "End.Census.Tract")) %>%
  left_join(., LV_users_dropoff_ct %>% st_set_geometry(NULL), how = 'left', by = c("week", "End.Census.Tract")) %>% 
  mutate(cnt_reb = replace_na(cnt_reb, 0),
         cnt_user = replace_na(cnt_user, 0),
         OI = cnt_reb + cnt_user)

# now lets focus on the trips started from each census tract
LV_users_pickup_ct <- LV_rebal_user_only_0619_combined_rowPairs_ct_start %>%
  na.omit() %>%
  group_by(week, Start.Census.Tract) %>%
  summarise(cnt_out = n()) %>%
  rename(End.Census.Tract = Start.Census.Tract)

LV_study.panel <- left_join(LV_study.panel, LV_users_pickup_ct %>% st_set_geometry(NULL), how='left', by = c("week", "End.Census.Tract")) %>% 
  mutate(cnt_out = replace_na(cnt_out, 0),
         diff = OI / cnt_out, ratio = mean_out/mean_OI)

### weekly opportunity index in June
LV_OI_bymonth <- LV_study.panel %>%
  group_by(End.Census.Tract) %>%
  summarise(mean_OI = mean(OI), mean_out = mean(cnt_out)) %>% 
  mutate(diff =  mean_OI - mean_out, ratio = mean_out/mean_OI) %>% 
  left_join(LV_Census_geoinfo, by = c('End.Census.Tract' = 'GEOID'))

ggplot() +
  geom_sf(data = LV_OI_bymonth %>% st_as_sf(), aes(fill = diff)) +
  scale_fill_viridis() +
  mapTheme() +
  labs(title = "Opportunity Index by Census Tract in Louisville")

ggplot() +
  geom_sf(data = LV_OI_bymonth %>% st_as_sf(), aes(fill = ratio)) +
  scale_fill_viridis() +
  mapTheme() +
  labs(title = "Ratio of out-flow and Opportunity Index \nby Census Tract in Louisville")

# For all data ----
# we want to know which census tract did these trips ended in.
LV_rebal_reb_only_combined_rowPairs <- LV_rebal_reb_only_combined_rowPairs %>% 
  mutate(week = week(end_time)) %>% 
  st_as_sf(sf_column_name = "trip_origin", crs = LV_proj) %>% 
  mutate(lon_s = st_coordinates(.)[1],
         lat_s = st_coordinates(.)[2]) %>% 
  as.data.frame() %>% 
  st_as_sf(sf_column_name = "trip_dest", crs = LV_proj) %>% 
  mutate(lon_d = st_coordinates(.)[1],
         lat_d = st_coordinates(.)[2]) %>% 
  as.data.frame()

LV_rebal_reb_only_combined_rowPairs_sf_end <- st_as_sf(LV_rebal_reb_only_combined_rowPairs, 
                                                            sf_column_name = "trip_dest", crs = LV_proj)

LV_rebal_reb_only_combined_rowPairs_ct_end <- st_join(LV_rebal_reb_only_combined_rowPairs_sf_end, 
                                                           LV_Census_geoinfo %>% 
                                                             dplyr::select(GEOID), st_within, left=T) %>% 
  rename(End.Census.Tract = GEOID)

LV_rebal_reb_only_combined_rowPairs_sf_start <- st_as_sf(LV_rebal_reb_only_0619_combined_rowPairs, 
                                                              sf_column_name = "trip_origin", crs = LV_proj)

LV_rebal_reb_only_combined_rowPairs_ct_start <- st_join(LV_rebal_reb_only_combined_rowPairs_sf_start, 
                                                             LV_Census_geoinfo %>% 
                                                               dplyr::select(GEOID), st_within, left=T) %>% 
  rename(Start.Census.Tract = GEOID)

# users data ####
LV_rebal_user_only_combined_rowPairs <- LV_rebal_user_only_combined_rowPairs %>% 
  mutate(week = week(end_time)) %>% 
  st_as_sf(sf_column_name = "trip_origin", crs = LV_proj) %>% 
  mutate(lon_s = st_coordinates(.)[1],
         lat_s = st_coordinates(.)[2]) %>% 
  as.data.frame() %>% 
  st_as_sf(sf_column_name = "trip_dest", crs = LV_proj) %>% 
  mutate(lon_d = st_coordinates(.)[1],
         lat_d = st_coordinates(.)[2]) %>% 
  as.data.frame()

LV_rebal_user_only_combined_rowPairs_sf_end <- st_as_sf(LV_rebal_user_only_combined_rowPairs, 
                                                             sf_column_name = "trip_dest", crs = LV_proj)

LV_rebal_user_only_combined_rowPairs_ct_end <- st_join(LV_rebal_user_only_combined_rowPairs_sf_end, 
                                                            LV_Census_geoinfo %>% 
                                                              dplyr::select(GEOID), st_within, left=T) %>% 
  rename(End.Census.Tract = GEOID)

LV_rebal_user_only_combined_rowPairs_sf_start <- st_as_sf(LV_rebal_user_only_combined_rowPairs, 
                                                               sf_column_name = "trip_origin", crs = LV_proj)

LV_rebal_user_only_combined_rowPairs_ct_start <- st_join(LV_rebal_user_only_combined_rowPairs_sf_start, 
                                                              LV_Census_geoinfo %>% 
                                                                dplyr::select(GEOID), st_within, left=T) %>% 
  rename(Start.Census.Tract = GEOID)
