##########################################################################
# This script is for calculating density of spatial features (count/area) in each census
# tract in LV (University/college is not calculated, since there are only 37)

# This script export the file: 
# 1. LV_spatial_panel which has both KNN and density info (can decide later to use which one)
# 2. LV_spatial_census which has both spatial and census vars 

##########################################################################

LV_Census_geoinfo$area <- as.numeric(st_area(LV_Census_geoinfo))*9.29e-8
# retail ####
LV_retail_ct <- st_join(LV_Census_geoinfo %>% st_intersection(LV_SA), LV_retail) %>%
  group_by(GEOID,area) %>%
  summarise(count= n())

LV_retail_ct$retail_density <- LV_retail_ct$count/LV_retail_ct$area

ggplot() +
  geom_sf(data=LV_retail_ct,aes(fill=retail_density))+
  scale_fill_viridis()

# office ####
LV_office_ct <- st_join(LV_Census_geoinfo %>% st_intersection(LV_SA), LV_office) %>%
  group_by(GEOID,area) %>%
  summarise(count= n())

LV_office_ct$office_density <- LV_office_ct$count/LV_office_ct$area

ggplot() +
  geom_sf(data=LV_office_ct,aes(fill=office_density))+
  scale_fill_viridis()

# restaurant ####
LV_restaurant_ct <- st_join(LV_Census_geoinfo %>% st_intersection(LV_SA),LV_restaurant) %>%
  group_by(GEOID,area) %>%
  summarise(count= n())
# count of certain facility in a census tract
LV_restaurant_ct$restaurant_density <- LV_restaurant_ct$count/LV_restaurant_ct$area
#LV_Census_panel$restaurant_density <- LV_restaurant_ct$restaurant_density

ggplot() +
  geom_sf(data=LV_restaurant_ct,aes(fill=restaurant_density))+
  scale_fill_viridis()

# public transport ####
LV_public_transport_ct <- st_join(LV_Census_geoinfo %>% st_intersection(LV_SA),LV_public_transport) %>%
  group_by(GEOID,area) %>%
  summarise(count= n())

LV_public_transport_ct$public_transport_density <- LV_restaurant_ct$count/LV_restaurant_ct$area
#LV_Census_panel$public_transport_density <- LV_public_transport_ct$public_transport_density

ggplot() +
  geom_sf(data=LV_public_transport_ct,aes(fill=public_transport_density))+
  scale_fill_viridis()

# cycleway ####
LV_cycleway_ct_len <- st_intersection(LV_cycleway,LV_Census_geoinfo) %>%
  mutate(length = as.numeric(st_length(.))*0.000189394) %>%
  group_by(GEOID) %>%
  summarise(total_length = sum(length)) %>%
  st_set_geometry(NULL) %>%
  merge(LV_Census_geoinfo, on='GEOID', all.y=T) %>%
  st_as_sf()
LV_cycleway_ct_len$total_length <- replace_na(LV_cycleway_ct_len$total_length,0) %>% st_intersection(LV_SA)

ggplot() +
  geom_sf(data=LV_cycleway_ct_len %>% st_intersection(LV_SA),aes(fill=total_length))+
  scale_fill_viridis()

# leisure ####
LV_leisure_ct <- st_join(LV_Census_geoinfo %>% st_intersection(LV_SA), LV_leisure) %>%
  group_by(GEOID,area) %>%
  summarise(count= n())

LV_leisure_ct$leisure_density <- LV_leisure_ct$count/LV_leisure_ct$area

ggplot() +
  geom_sf(data=LV_leisure_ct,aes(fill=leisure_density))+
  scale_fill_viridis()

# tourism ####
LV_tourism_ct <- st_join(LV_Census_geoinfo %>% st_intersection(LV_SA), LV_tourism) %>%
  group_by(GEOID,area) %>%
  summarise(count= n())

LV_tourism_ct$tourism_density <- LV_tourism_ct$count/LV_tourism_ct$area

ggplot() +
  geom_sf(data=LV_tourism_ct,aes(fill=tourism_density))+
  scale_fill_viridis()

LV_spatial_panel <- left_join(LV_Census_panel, LV_retail_ct%>%st_set_geometry(NULL)%>%dplyr::select(GEOID, retail_density), by = 'GEOID') %>%
  left_join(LV_office_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, office_density), by = 'GEOID') %>%
  left_join(LV_leisure_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, leisure_density), by = 'GEOID') %>%
  left_join(LV_tourism_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, tourism_density), by = 'GEOID') %>%
  left_join(LV_public_transport_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, public_transport_density), by = 'GEOID') %>%
  left_join(LV_restaurant_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, restaurant_density), by = 'GEOID') %>%
  left_join(LV_cycleway_ct_len %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, total_length), by = 'GEOID')

LV_spatial_panel[is.na(LV_spatial_panel)] <- 0

LV_spatial_panel_RDS <- file.path(data_directory, "~RData/Louisville/LV_spatial_panel")
# saveRDS(LV_spatial_panel,
#        file = LV_spatial_panel_RDS)
LV_spatial_panel <- readRDS(LV_spatial_panel_RDS)

LV_spatial_census <- left_join(LV_spatial_panel, LV_open_ct%>%st_set_geometry(NULL)%>%dplyr::select(-centroid_X, -centroid_Y), by = 'GEOID')

LV_spatial_census_RDS <- file.path(data_directory, "~RData/Louisville/LV_spatial_census")
#saveRDS(LV_spatial_census,
#        file = LV_spatial_census_RDS)
LV_spatial_census <- readRDS(LV_spatial_census_RDS)