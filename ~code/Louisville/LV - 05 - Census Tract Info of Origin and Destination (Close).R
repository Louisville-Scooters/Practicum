##########################################################################
# This script:
# 1. Add location field for data obtained from LV - 03/04, including longitude and latitude of origin and destination 
#
# This script exports the following data:
# 1. LV_rebal_reb_only_combined_rowPairs_ct - origin-destination rows for all rebalance records in the rebalance data with the census tract info of the origin and destination
# 2. LV_rebal_user_only_combined_rowPairs_ct - origin-destination rows for all rebalance records in the rebalance data with the census tract info of the origin and destination
#
##########################################################################

### users events####
# Read the structured rebalance data - users events
LV_rebal_user_only_combined_rowPairs_RDS <- file.path(data_directory, 
                                                      "~RData/Louisville/LV_rebal_user_only_combined_rowPairs")

LV_rebal_user_only_combined_rowPairs <- readRDS(LV_rebal_user_only_combined_rowPairs_RDS)

# For exploratory analysis, we first focus on trips in June 2019
LV_rebal_user_only_combined_rowPairs <- LV_rebal_user_only_combined_rowPairs %>%
  filter(year(start_time)==2019, month(start_time)==6)

# extract x y of geometry
LV_rebal_user_only_combined_rowPairs[c("lon_s", "lat_s")] <- do.call(rbind, 
                                                                     lapply(strsplit(as.character(LV_rebal_user_only_combined_rowPairs$trip_origin), "[()]"), 
                                                                            function(col) {   
                                                                              (parts <- unlist(strsplit(col[2], ",")))
                                                                            }
                                                                     )
)

LV_rebal_user_only_combined_rowPairs[c("lon_d", "lat_d")] <- do.call(rbind, 
                                                                     lapply(strsplit(as.character(LV_rebal_user_only_combined_rowPairs$trip_dest), "[()]"), 
                                                                            function(col) {   
                                                                              (parts <- unlist(strsplit(col[2], ",")))
                                                                            }
                                                                     )
)

LV_rebal_user_only_combined_rowPairs$lon_s <- as.numeric(LV_rebal_user_only_combined_rowPairs$lon_s)
LV_rebal_user_only_combined_rowPairs$lat_s <- as.numeric(LV_rebal_user_only_combined_rowPairs$lat_s)
LV_rebal_user_only_combined_rowPairs$lon_d <- as.numeric(LV_rebal_user_only_combined_rowPairs$lon_d)
LV_rebal_user_only_combined_rowPairs$lat_d <- as.numeric(LV_rebal_user_only_combined_rowPairs$lat_d)

LV_rebal_user_only_combined_rowPairs_sf <- st_as_sf(LV_rebal_user_only_combined_rowPairs, coords = c('lon_s','lat_s'),crs=LV_proj) %>%
  mutate(lon_s = unlist(map(geometry, 1)),
         lat_s = unlist(map(geometry, 2)))
LV_rebal_user_only_combined_rowPairs_ct <- st_join(LV_rebal_user_only_combined_rowPairs_sf, LV_Census_geoinfo %>% dplyr::select(GEOID), st_within, left=T) %>%
  rename(Start.Census.Tract=GEOID)

LV_rebal_user_only_combined_rowPairs_ct <- st_as_sf(as.data.frame(LV_rebal_user_only_combined_rowPairs_ct) %>% dplyr::select(-geometry), coords = c('lon_d','lat_d'),crs=LV_proj) %>%
  mutate(lon_d = unlist(map(geometry, 1)),
         lat_d = unlist(map(geometry, 2)))
LV_rebal_user_only_combined_rowPairs_ct <- st_join(LV_rebal_user_only_combined_rowPairs_ct, LV_Census_geoinfo %>% dplyr::select(GEOID), st_within, left=T) %>%
  rename(End.Census.Tract=GEOID)


### rebalance events####
# Read the structured rebalance data - rebalance events
LV_rebal_reb_only_combined_rowPairs_RDS <- file.path(data_directory, 
                                                      "~RData/Louisville/LV_rebal_user_only_combined_rowPairs")

LV_rebal_reb_only_combined_rowPairs <- readRDS(LV_rebal_reb_only_combined_rowPairs_RDS)


# extract x y of geometry
LV_rebal_reb_only_combined_rowPairs[c("lon_s", "lat_s")] <- do.call(rbind, 
                                                                    lapply(strsplit(as.character(LV_rebal_reb_only_combined_rowPairs$trip_origin), "[()]"), 
                                                                           function(col) {   
                                                                             (parts <- unlist(strsplit(col[2], ",")))
                                                                           }
                                                                    )
)

LV_rebal_reb_only_combined_rowPairs[c("lon_d", "lat_d")] <- do.call(rbind, 
                                                                    lapply(strsplit(as.character(LV_rebal_reb_only_combined_rowPairs$trip_dest), "[()]"), 
                                                                           function(col) {   
                                                                             (parts <- unlist(strsplit(col[2], ",")))
                                                                           }
                                                                    )
)

LV_rebal_reb_only_combined_rowPairs$lon_s <- as.numeric(LV_rebal_reb_only_combined_rowPairs$lon_s)
LV_rebal_reb_only_combined_rowPairs$lat_s <- as.numeric(LV_rebal_reb_only_combined_rowPairs$lat_s)
LV_rebal_reb_only_combined_rowPairs$lon_d <- as.numeric(LV_rebal_reb_only_combined_rowPairs$lon_d)
LV_rebal_reb_only_combined_rowPairs$lat_d <- as.numeric(LV_rebal_reb_only_combined_rowPairs$lat_d)

LV_rebal_reb_only_combined_rowPairs_sf <- st_as_sf(LV_rebal_reb_only_combined_rowPairs, coords = c('lon_s','lat_s'),crs=LV_proj) %>%
  mutate(lon_s = unlist(map(geometry, 1)),
         lat_s = unlist(map(geometry, 2)))
LV_rebal_reb_only_combined_rowPairs_ct <- st_join(LV_rebal_reb_only_combined_rowPairs_sf, LV_Census_geoinfo %>% dplyr::select(GEOID), st_within, left=T) %>%
  rename(Start.Census.Tract=GEOID)

LV_rebal_reb_only_combined_rowPairs_ct <- st_as_sf(as.data.frame(LV_rebal_reb_only_combined_rowPairs_ct) %>% dplyr::select(-geometry), coords = c('lon_d','lat_d'),crs=LV_proj) %>%
  mutate(lon_d = unlist(map(geometry, 1)),
         lat_d = unlist(map(geometry, 2)))
LV_rebal_reb_only_combined_rowPairs_ct <- st_join(LV_rebal_reb_only_combined_rowPairs_ct, LV_Census_geoinfo %>% dplyr::select(GEOID), st_within, left=T) %>%
  rename(End.Census.Tract=GEOID)



# save the data ####
LV_rebal_reb_only_combined_rowPairs_ct_RDS <- file.path(data_directory, 
                                          "~RData/Louisville/LV_rebal_reb_only_combined_rowPairs_ct")

saveRDS(LV_rebal_reb_only_combined_rowPairs_ct,
        file = LV_rebal_reb_only_combined_rowPairs_ct_RDS)

LV_rebal_user_only_combined_rowPairs_ct_RDS <- file.path(data_directory, 
                                                        "~RData/Louisville/LV_rebal_user_only_combined_rowPairs_ct")

saveRDS(LV_rebal_user_only_combined_rowPairs_ct,
        file = LV_rebal_user_only_combined_rowPairs_ct_RDS)
