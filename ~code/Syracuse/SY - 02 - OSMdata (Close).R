##########################################################################
# This script is for quering data from OpenStreetMap for LV
# List of spatial features:
# University (KNN, 1)
# Restaurant (density)
# Retail (KNN, 5)
# Office (KNN, 5)
# Tourism (density)
# Leisure (density)
# Cycleway (density)
# Public_transport station (KNN, 5)

#********************* the variables******************####
city_name = "Syracuse"                              ####
proj = 2272 # city projection                         ####
boundary = SY_Census_geoinfo # service area boundary              ####
census_ct = SY_Census_ct # census tract ecosocia data ####
#origin_ct = SY_open_ct #census tract with count of trip origins
census_geoinfo = SY_Census_geoinfo                    ####
#*****************************************************####

### using osm to grab data####
college <- opq (city_name) %>%
  add_osm_feature(key = 'amenity', value = c("university", "college")) %>%
  osmdata_sf(.)
# 
college <- st_geometry(college$osm_polygons) %>%
  st_transform(proj) %>%
  st_sf() %>%
  st_intersection(boundary) %>%
  st_centroid() %>%
  mutate(Legend = 'College',
         City = city_name) %>%
  dplyr::select(Legend, City, geometry)

#Get_OSM <- function ()

# restaurant ####
restaurant <- opq (city_name) %>%
  add_osm_feature(key = 'amenity', value = c("restaurant", "fast_food")) %>%
  osmdata_sf(.)

restaurant <- st_geometry(restaurant$osm_points) %>%
  st_transform(proj) %>%
  st_sf() %>%
  st_intersection(boundary) %>%
  mutate(Legend = 'Restaurant',
         City = city_name) %>%
  dplyr::select(Legend, City, geometry)

# public transport ####
public_transport <- opq (city_name) %>%
  add_osm_feature(key = 'public_transport', value = c("stop_position", "station")) %>%
  osmdata_sf(.)

public_transport <- st_geometry(public_transport$osm_points) %>%
  st_transform(proj) %>%
  st_sf() %>%
  st_intersection(boundary) %>%
  mutate(Legend = 'Public.Transport',
         City = city_name) %>%
  dplyr::select(Legend, City, geometry)

# retail ####
retail <- opq (city_name) %>%
  add_osm_feature(key = 'shop') %>%
  osmdata_sf(.)

retail <- st_geometry(retail$osm_points) %>%
  st_transform(proj) %>%
  st_sf() %>%
  st_intersection(boundary) %>%
  mutate(Legend = 'Retails',
         City = city_name) %>%
  dplyr::select(Legend, City, geometry)

# office ####
office <- opq (city_name) %>%
  add_osm_feature(key = 'office') %>%
  osmdata_sf(.)

office <- st_geometry(office$osm_points) %>%
  st_transform(proj) %>%
  st_sf() %>%
  st_intersection(boundary) %>%
  mutate(Legend = 'Office',
         City = city_name) %>%
  dplyr::select(Legend, City, geometry)

# cycleway ####
cycleway <- opq (city_name) %>%
  add_osm_feature(key = 'cycleway') %>%
  osmdata_sf(.)

cycleway <- st_geometry(cycleway$osm_lines) %>%
  st_transform(proj) %>%
  st_sf() %>%
  st_intersection(boundary) %>%
  mutate(Legend = 'Cycleway',
         City = city_name) %>%
  dplyr::select(Legend, City, geometry)

cycleway %>% st_join(census_geoinfo %>% st_intersection(boundary))

# leisure  ####
leisure <- opq (city_name) %>%
  add_osm_feature(key = 'leisure', value = c('adult_gaming_center','amusement_arcade','common','fitness_center','hackerspace','park',
                                             'pitch','stadium')) %>%
  osmdata_sf(.)

leisure <- st_geometry(leisure$osm_points) %>%
  st_transform(proj) %>%
  st_sf() %>%
  st_intersection(boundary) %>%
  mutate(Legend = 'Leisure',
         City = city_name) %>%
  dplyr::select(Legend, City, geometry)

# tourism  ####
tourism <- opq (city_name) %>%
  add_osm_feature(key = 'tourism', value = c("aquarium", "artwork", "attraction", "gallery", "museumm", "theme_park", 'viewpoint', 'zoo')) %>%
  osmdata_sf(.)

tourism <- st_geometry(tourism$osm_points) %>%
  st_transform(proj) %>%
  st_sf() %>%
  st_intersection(boundary) %>%
  mutate(Legend = 'Tourism',
         City = city_name) %>%
  dplyr::select(Legend, City, geometry)

## code to plot and check the OSM data
grid.arrange(
  ggplot()+
    geom_sf(data = census_ct, fill = "white")+
    geom_sf(data = cycleway, color = "chartreuse3", size = 1.5, alpha = 0.6)+
    geom_sf(data = leisure, color = "lightsalmon",alpha = 0.6)+
    geom_sf(data = boundary,fill='transparent')+
    labs(title = paste("Location of cycleway and leisure places in", city_name),
         subtitle = "Green lines as cycleway and light pink dots as leisure places") +
    mapTheme(),
  
  ggplot()+
    geom_sf(data = census_ct, fill = "white")+
    geom_sf(data = restaurant, color = "turquoise",alpha = 0.6)+
    geom_sf(data = tourism, color = "hotpink", alpha = 0.6)+
    geom_sf(data = boundary,fill='transparent')+
    labs(title = paste("Location of restaurant and tourism spots in", city_name),
         subtitle = "Turqoise dots as restaurants and pink dots as tourism spots") +
    mapTheme(),
  
  ggplot()+
    geom_sf(data = census_ct, fill = "white")+
    geom_sf(data = office, color = "indianred2", alpha = 0.6, size = 2)+
    geom_sf(data = retail, color = "orange", alpha = 0.6, size = 2)+
    geom_sf(data = college, shape = 23, fill = "cornflowerblue", size = 2)+
    geom_sf(data = boundary,fill='transparent')+
    labs(title = paste("Location of offices, retails, and colleges in", city_name),
         subtitle = "Red dots as office, orange dots as retails, and blue dots as colleges") +
    mapTheme(),
  ncol = 3)

# street
SY_street_path <- file.path(data_directory,
                            "Syracuse/9fbc61c5-24ea-4201-8019-8f1726d849bf202045-1-1w3aekp.undc.shp")
SY_street <- st_read(SY_street_path) %>%
  st_transform(2272)

# ggplot() +
#   geom_sf(data = SY_Census_geoinfo) +
#   geom_sf(data = st_intersection(SY_street,SY_Census_geoinfo))


SY_street_ct_len <- st_intersection(SY_street,SY_Census_geoinfo) %>%
  mutate(length = as.numeric(st_length(.))*0.000189394) %>%
  group_by(GEOID) %>%
  summarise(street_length = sum(length)) %>%
  st_set_geometry(NULL) %>%
  merge(SY_Census_geoinfo, on='GEOID', all.y=T) %>%
  st_as_sf()

SY_street_ct_len$street_length <- replace_na(SY_street_ct_len$street_length,0)

census_panel <- census_geoinfo

## count ####
census_geoinfo$area <- as.numeric(st_area(census_geoinfo))*9.29e-8
# retail 
retail_ct <- st_join(census_geoinfo %>% st_intersection(boundary), retail) %>%
  group_by(GEOID,area) %>%
  summarise(count_retail= n())
retail_ct$density_retail <- retail_ct$count_retail/retail_ct$area

# office 
office_ct <- st_join(census_geoinfo %>% st_intersection(boundary), office) %>%
  group_by(GEOID,area) %>%
  summarise(count_office= n())
office_ct$density_office <- office_ct$count_office/office_ct$area

# restaurant 
restaurant_ct <- st_join(census_geoinfo %>% st_intersection(boundary),restaurant) %>%
  group_by(GEOID,area) %>%
  summarise(count_restaurant= n())
restaurant_ct$density_restaurant <- restaurant_ct$count_restaurant/restaurant_ct$area

# public transport
public_transport_ct <- st_join(census_geoinfo %>% st_intersection(boundary),public_transport) %>%
  group_by(GEOID,area) %>%
  summarise(count_pubtran= n())
public_transport_ct$density_pubtran <- public_transport_ct$count_pubtran/public_transport_ct$area

# cycleway
cycleway_ct_len <- st_intersection(cycleway, boundary) %>%
  mutate(length = as.numeric(st_length(.))*0.000189394) %>%
  group_by(GEOID) %>%
  summarise(total_length = sum(length)) %>%
  st_set_geometry(NULL) %>%
  merge(census_geoinfo, on='GEOID', all.y=T) %>%
  st_as_sf()
cycleway_ct_len$total_length <- replace_na(cycleway_ct_len$total_length,0) 

# leisure
leisure_ct <- st_join(census_geoinfo %>% st_intersection(boundary), leisure) %>%
  group_by(GEOID,area) %>%
  summarise(count_leisure= n())
leisure_ct$density_leisure <- leisure_ct$count_leisure/leisure_ct$area

# tourism
tourism_ct <- st_join(census_geoinfo %>% st_intersection(boundary), tourism) %>%
  group_by(GEOID,area) %>%
  summarise(count_tourism= n())
tourism_ct$density_tourism <- tourism_ct$count_tourism/tourism_ct$area

# college
college_ct <- st_join(census_geoinfo %>% st_intersection(boundary), college) %>%
  group_by(GEOID,area) %>%
  summarise(count_college= n())
college_ct$density_college <- college_ct$count_college/college_ct$area

spatial_census <- left_join(census_panel, retail_ct%>%st_set_geometry(NULL)%>%dplyr::select(GEOID, count_retail, density_retail), by = 'GEOID') %>%
  left_join(office_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, count_office, density_office), by = 'GEOID') %>%
  left_join(leisure_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, count_leisure, density_leisure), by = 'GEOID') %>%
  left_join(tourism_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, count_tourism, density_tourism), by = 'GEOID') %>%
  left_join(public_transport_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, count_pubtran,density_pubtran), by = 'GEOID') %>%
  left_join(restaurant_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, count_restaurant, density_restaurant), by = 'GEOID') %>%
  left_join(college_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, count_college, density_college), by = 'GEOID') %>%
  left_join(cycleway_ct_len %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, total_length), by = 'GEOID')

SY_spatial_census <- spatial_census
SY_spatial_census <- merge(SY_spatial_census, as.data.frame(SY_street_ct_len) %>% dplyr::select(GEOID, street_length), by='GEOID')
# ratio
SY_spatial_census$ratio_retail <- SY_spatial_census$count_retail/length(SY_spatial_census$count_retail)[1]
SY_spatial_census$ratio_office <- SY_spatial_census$count_office/length(SY_spatial_census$count_office)[1]
SY_spatial_census$ratio_restaurant <- SY_spatial_census$count_restaurant/length(SY_spatial_census$count_office)[1]
SY_spatial_census$ratio_public_transport <- SY_spatial_census$count_pubtran/length(SY_spatial_census$count_pubtran)[1]
SY_spatial_census$ratio_leisure <- SY_spatial_census$count_leisure/length(SY_spatial_census$count_leisure)[1]
SY_spatial_census$ratio_tourism <- SY_spatial_census$count_tourism/length(SY_spatial_census$count_tourism)[1]
SY_spatial_census$ratio_college <- SY_spatial_census$count_college/length(SY_spatial_census$count_college)[1]
SY_spatial_census$ratio_cycleway <- SY_spatial_census$total_length/sum(SY_spatial_census$total_length)
SY_spatial_census$ratio_street <- SY_spatial_census$street_length %>% replace_na(0)/sum(SY_spatial_census$street_length %>% replace_na(0))


SY_spatial_census <- merge(SY_spatial_census, as.data.frame(SY_Census) %>% dplyr::select(-geometry), by='GEOID')
SY_Census_q5 <- apply(SY_Census %>% st_set_geometry(NULL) %>% dplyr::select(-GEOID), 2, q5)
SY_Census_q5 <- cbind(values = as.data.frame(SY_Census), q5 = SY_Census_q5)

# SY_spatial_census_RDS <- file.path(data_directory, "~RData/Syracuse/SY_spatial_census")
# saveRDS(SY_spatial_census,
#         file = SY_spatial_census_RDS)
SY_spatial_census <- readRDS(SY_spatial_census_RDS)

# SY_Census_q5_RDS <- file.path(data_directory, "~RData/Syracuse/SY_Census_q5")
# saveRDS(SY_Census_q5,
#         file = SY_Census_q5_RDS)
SY_Census_q5 <- readRDS(SY_Census_q5_RDS)