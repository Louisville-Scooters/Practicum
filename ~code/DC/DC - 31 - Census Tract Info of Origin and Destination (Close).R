##########################################################################
# This script:
# 1. Add location field for KC trips data
#
# This script exports the following data:
# 1. KC_scooter_ct
#
##########################################################################

### users events####
# Read the structured rebalance data - users events
DC_scooter_sf <- st_as_sf(DC_scooter_data %>% na.omit(), coords = c('start_lon','start_lat'),crs=4326) %>%
  st_transform(DC_proj) %>%
  mutate(start_longitude = unlist(map(geometry, 1)),
         start_latitude = unlist(map(geometry, 2)))
DC_scooter_ct <- st_join(DC_scooter_sf, DC_Census_geoinfo %>% dplyr::select(GEOID), st_within, left=T) %>%
  rename(Start.Census.Tract=GEOID)

DC_scooter_ct <- st_as_sf(as.data.frame(DC_scooter_ct) %>% dplyr::select(-geometry), coords = c('end_lon','end_lat'),crs=4326) %>%
  st_transform(DC_proj) %>%
  mutate(end_longitude = unlist(map(geometry, 1)),
         end_longitude = unlist(map(geometry, 2)))


DC_scooter_ct <- st_join(DC_scooter_ct, DC_Census_geoinfo %>% dplyr::select(GEOID), st_within, left=T) %>%
  rename(End.Census.Tract=GEOID)

DC_scooter_ct_RDS <- file.path(data_directory, 
                               "~RData/Kansas City/DC_scooter_ct")

# saveRDS(DC_scooter_ct,
#         file = DC_scooter_ct_RDS)

