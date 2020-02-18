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
  summarise(count_retail= n())

LV_retail_ct$density_retail <- LV_retail_ct$count_retail/LV_retail_ct$area

ggplot() +
  geom_sf(data=LV_retail_ct,aes(fill=density_retail))+
  scale_fill_viridis()

# office ####
LV_office_ct <- st_join(LV_Census_geoinfo %>% st_intersection(LV_SA), LV_office) %>%
  group_by(GEOID,area) %>%
  summarise(count_office= n())

LV_office_ct$density_office <- LV_office_ct$count_office/LV_office_ct$area

ggplot() +
  geom_sf(data=LV_office_ct,aes(fill=density_office))+
  scale_fill_viridis()

# restaurant ####
LV_restaurant_ct <- st_join(LV_Census_geoinfo %>% st_intersection(LV_SA),LV_restaurant) %>%
  group_by(GEOID,area) %>%
  summarise(count_restaurant= n())
LV_restaurant_ct$density_restaurant <- LV_restaurant_ct$count_restaurant/LV_restaurant_ct$area

ggplot() +
  geom_sf(data=LV_restaurant_ct,aes(fill=density_restaurant))+
  scale_fill_viridis()

# public transport ####
LV_public_transport_ct <- st_join(LV_Census_geoinfo %>% st_intersection(LV_SA),LV_public_transport) %>%
  group_by(GEOID,area) %>%
  summarise(count_pubtran= n())

LV_public_transport_ct$density_pubtran <- LV_public_transport_ct$count_pubtran/LV_public_transport_ct$area
#LV_Census_panel$density_pubtran <- LV_public_transport_ct$density_pubtran

ggplot() +
  geom_sf(data=LV_public_transport_ct,aes(fill=density_pubtran))+
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
  summarise(count_leisure= n())

LV_leisure_ct$density_leisure <- LV_leisure_ct$count_leisure/LV_leisure_ct$area

ggplot() +
  geom_sf(data=LV_leisure_ct,aes(fill=density_leisure))+
  scale_fill_viridis()

# tourism ####
LV_tourism_ct <- st_join(LV_Census_geoinfo %>% st_intersection(LV_SA), LV_tourism) %>%
  group_by(GEOID,area) %>%
  summarise(count_tourism= n())

LV_tourism_ct$density_tourism <- LV_tourism_ct$count_tourism/LV_tourism_ct$area

# college ####
LV_college_ct <- st_join(LV_Census_geoinfo %>% st_intersection(LV_SA), LV_college) %>%
  group_by(GEOID,area) %>%
  summarise(count_college= n())

LV_college_ct$density_college <- LV_college_ct$count_college/LV_college_ct$area

ggplot() +
  geom_sf(data=LV_college_ct,aes(fill=density_college))+
  scale_fill_viridis()

LV_spatial_panel <- left_join(LV_Census_panel, LV_retail_ct%>%st_set_geometry(NULL)%>%dplyr::select(GEOID, count_retail, density_retail), by = 'GEOID') %>%
  left_join(LV_office_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, count_office, density_office), by = 'GEOID') %>%
  left_join(LV_leisure_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, count_leisure, density_leisure), by = 'GEOID') %>%
  left_join(LV_tourism_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, count_tourism, density_tourism), by = 'GEOID') %>%
  left_join(LV_public_transport_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, count_pubtran,density_pubtran), by = 'GEOID') %>%
  left_join(LV_restaurant_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, count_restaurant, density_restaurant), by = 'GEOID') %>%
  left_join(LV_college_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, count_college, density_college), by = 'GEOID') %>%
  left_join(LV_cycleway_ct_len %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, total_length), by = 'GEOID')

LV_spatial_panel[is.na(LV_spatial_panel)] <- 0

LV_spatial_panel_RDS <- file.path(data_directory, "~RData/Louisville/LV_spatial_panel")
# saveRDS(LV_spatial_panel,
#        file = LV_spatial_panel_RDS)
LV_spatial_panel <- readRDS(LV_spatial_panel_RDS)

LV_spatial_census <- left_join(LV_spatial_panel, LV_open_ct%>%st_set_geometry(NULL)%>%dplyr::select(-centroid_X, -centroid_Y), by = 'GEOID')

LV_spatial_census_RDS <- file.path(data_directory, "~RData/Louisville/LV_spatial_census")
# saveRDS(LV_spatial_census,
#        file = LV_spatial_census_RDS)
LV_spatial_census <- readRDS(LV_spatial_census_RDS)
