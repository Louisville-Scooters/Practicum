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
KC_scooter_sf <- st_as_sf(KC_scooter %>% na.omit(), coords = c('start_longitude','start_latitude'),crs=4326) %>%
  mutate(start_longitude = unlist(map(geometry, 1)),
         start_latitude = unlist(map(geometry, 2))) %>%
  st_transform(KC_proj)
KC_scooter_ct <- st_join(KC_scooter_sf, KC_Census_geoinfo %>% dplyr::select(GEOID), st_within, left=T) %>%
  rename(Start.Census.Tract=GEOID)

KC_scooter_ct <- st_as_sf(as.data.frame(KC_scooter_ct) %>% dplyr::select(-geometry), coords = c('end_longitude','end_latitude'),crs=4326) %>%
  mutate(lon_d = unlist(map(geometry, 1)),
         lat_d = unlist(map(geometry, 2))) %>%
  st_transform(KC_proj)

KC_scooter_ct <- st_join(KC_scooter_ct, KC_Census_geoinfo %>% dplyr::select(GEOID), st_within, left=T) %>%
  rename(End.Census.Tract=GEOID)

KC_scooter_ct_RDS <- file.path(data_directory, 
                               "~RData/Kansas City/KC_scooter_ct")

# saveRDS(KC_scooter_ct,
#         file = KC_scooter_ct_RDS)

