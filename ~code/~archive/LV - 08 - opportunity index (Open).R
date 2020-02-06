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

### extraction  (could be deleted if it is included in other files) ####
# rebalancing data####
# structured rebalance data (i.e. LV_rebal_reb_only_0619_combined_rowPairs here) could be obtained by running code LV - 04.
# obtain fields about longitude and latitude.
LV_rebal_reb_only_0619_combined_rowPairs[c("lon_s", "lat_s")] <- do.call(rbind, 
                                           lapply(strsplit(LV_rebal_reb_only_0619_combined_rowPairs$trip_origin, "[()]"), 
                                                  function(col) {   
                                                    (parts <- unlist(strsplit(col[2], " ")))
                                                  }
                                           )
)

LV_rebal_reb_only_0619_combined_rowPairs[c("lon_d", "lat_d")] <- do.call(rbind, 
                                         lapply(strsplit(LV_rebal_reb_only_0619_combined_rowPairs$trip_dest, "[()]"), 
                                                function(col) {   
                                                  (parts <- unlist(strsplit(col[2], " ")))
                                                }
                                         )
)

LV_rebal_reb_only_0619_combined_rowPairs$lon_s <- as.numeric(LV_rebal_reb_only_0619_combined_rowPairs$lon_s)
LV_rebal_reb_only_0619_combined_rowPairs$lat_s <- as.numeric(LV_rebal_reb_only_0619_combined_rowPairs$lat_s)
LV_rebal_reb_only_0619_combined_rowPairs$lon_d <- as.numeric(LV_rebal_reb_only_0619_combined_rowPairs$lon_d)
LV_rebal_reb_only_0619_combined_rowPairs$lat_d <- as.numeric(LV_rebal_reb_only_0619_combined_rowPairs$lat_d)
# then, we want to know which census tract did these trips ended in.
LV_rebal_reb_only_0619_combined_rowPairs_sf <- st_as_sf(LV_rebal_reb_only_0619_combined_rowPairs, coords = c('lon_d','lat_d'),crs=4326) %>%
  mutate(lon_d = unlist(map(geometry, 1)),
         lat_d = unlist(map(geometry, 2)))
LV_rebal_reb_only_0619_combined_rowPairs_ct <- st_join(LV_rebal_reb_only_0619_combined_rowPairs_sf, LV_Census_geoinfo %>% select(GEOID), st_within, left=T)
LV_rebal_reb_only_0619_combined_rowPairs_ct <- rename(LV_rebal_reb_only_0619_combined_rowPairs_ct, End.Census.Tract=GEOID)

### calculating the opportunity index (rebalancing drop off and trips end) for each census tract by week
# first we focus on rebalancing drop off data
LV_reb_dropoff_ct <- LV_rebal_reb_only_0619_combined_rowPairs_ct %>%
  na.omit() %>%
  group_by(week, End.Census.Tract) %>%
  summarise(cnt_reb = n())
LV_reb_dropoff_ct$End.Census.Tract <- as.character(LV_reb_dropoff_ct$End.Census.Tract)

# next, we turn to the trips end data
LV_rebal_user_only_0619_combined_rowPairs_ct$week <- week(LV_rebal_user_only_0619_combined_rowPairs_ct$end_time)
LV_users_dropoff_ct <- LV_rebal_user_only_0619_combined_rowPairs_ct %>%
  na.omit() %>%
  group_by(week, End.Census.Tract) %>%
  summarise(cnt_user = n())
LV_users_dropoff_ct$End.Census.Tract <- as.character(LV_users_dropoff_ct$End.Census.Tract)

# users data ####
# LV_rebal_user_only_0619_combined_rowPairs could be obtained by running code LV - 03
LV_rebal_user_only_0619_combined_rowPairs[c("lon_s", "lat_s")] <- do.call(rbind, 
                                           lapply(strsplit(LV_rebal_user_only_0619_combined_rowPairs$trip_origin, "[()]"), 
                                                  function(col) {   
                                                    (parts <- unlist(strsplit(col[2], " ")))
                                                  }
                                           )
)

LV_rebal_user_only_0619_combined_rowPairs[c("lon_d", "lat_d")] <- do.call(rbind, 
                                            lapply(strsplit(LV_rebal_user_only_0619_combined_rowPairs$trip_dest, "[()]"), 
                                                   function(col) {   
                                                     (parts <- unlist(strsplit(col[2], " ")))
                                                   }
                                            )
)

LV_rebal_user_only_0619_combined_rowPairs$lon_s <- as.numeric(LV_rebal_user_only_0619_combined_rowPairs$lon_s)
LV_rebal_user_only_0619_combined_rowPairs$lat_s <- as.numeric(LV_rebal_user_only_0619_combined_rowPairs$lat_s)
LV_rebal_user_only_0619_combined_rowPairs$lon_d <- as.numeric(LV_rebal_user_only_0619_combined_rowPairs$lon_d)
LV_rebal_user_only_0619_combined_rowPairs$lat_d <- as.numeric(LV_rebal_user_only_0619_combined_rowPairs$lat_d)

LV_rebal_user_only_0619_combined_rowPairs_sf <- st_as_sf(LV_rebal_user_only_0619_combined_rowPairs, coords = c('lon_s','lat_s'),crs=4326) %>%
  mutate(lon_s = unlist(map(geometry, 1)),
         lat_s = unlist(map(geometry, 2)))
LV_rebal_user_only_0619_combined_rowPairs_ct <- st_join(LV_rebal_user_only_0619_combined_rowPairs_sf, LV_Census_geoinfo %>% select(GEOID), st_within, left=T) %>%
  rename(Start.Census.Tract=GEOID)

LV_rebal_user_only_0619_combined_rowPairs_ct <- st_set_geometry(LV_rebal_user_only_0619_combined_rowPairs_ct, NULL)
LV_rebal_user_only_0619_combined_rowPairs_ct <- st_as_sf(LV_rebal_user_only_0619_combined_rowPairs_ct, coords = c('lon_d','lat_d'),crs=4326) %>%
  mutate(lon_d = unlist(map(geometry, 1)),
         lat_d = unlist(map(geometry, 2)))
LV_rebal_user_only_0619_combined_rowPairs_ct <- st_join(LV_rebal_user_only_0619_combined_rowPairs_ct, LV_Census_geoinfo %>% select(GEOID), st_within, left=T) %>%
  rename(End.Census.Tract=GEOID)

### Create a panel ####
# rebalance_june contains all the trips (including rebalancing and users' etc.) happened in June, 2019
rebalance_june$week <- week(rebalance_june$occurredAt)
study.panel <- 
  expand.grid(week=unique(rebalance_june$week), 
              End.Census.Tract = unique(LV_Census_geoinfo$GEOID)) %>%
  left_join(., LV_reb_dropoff_ct %>% st_set_geometry(NULL), how='left', on=c(week,End.Census.Tract)) %>%
  left_join(., LV_users_dropoff_ct %>% st_set_geometry(NULL), how='left', on=c(week,End.Census.Tract))

study.panel$cnt_reb <- study.panel$cnt_reb %>% replace_na(0)
study.panel$cnt_user <- study.panel$cnt_user %>% replace_na(0)

study.panel$OI <- study.panel$cnt_reb + study.panel$cnt_user

# now lets focus on the trips started from each census tract
LV_users_pickup_ct <- LV_rebal_user_only_0619_combined_rowPairs_ct %>%
  na.omit() %>%
  group_by(week, Start.Census.Tract) %>%
  summarise(cnt_out = n()) %>%
  rename(End.Census.Tract=Start.Census.Tract)

study.panel <- left_join(study.panel, LV_users_pickup_ct %>% st_set_geometry(NULL), how='left', on=c(week,End.Census.Tract))
study.panel$cnt_out <- study.panel$cnt_out %>% replace_na(0)

study.panel$diff <- study.panel$OI - study.panel$cnt_out

### weekly opportunity index in June
LV_OI_bymonth <- study.panel %>%
  group_by(End.Census.Tract) %>%
  summarise(mean_OI = mean(OI), mean_out = mean(cnt_out))

LV_OI_bymonth$diff <- LV_OI_bymonth$mean_OI - LV_OI_bymonth$mean_out
LV_OI_bymonth <- left_join(LV_OI_bymonth, LV_Census_geoinfo, by=c('End.Census.Tract'='GEOID'))
ggplot() +
  geom_sf(data = LV_OI_bymonth %>% st_as_sf(), aes(fill=diff)) +
  scale_fill_viridis() +
  mapTheme()
