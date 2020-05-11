##########################################################################
# This script is for quering data from OpenStreetMap for Austin
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
city_name = "Kansas City"                              ####
proj = KC_proj # city projection                         ####
boundary = KC_Census_geoinfo # service area boundary              ####
census_ct = KC_Census_ct # census tract ecosocia data ####
origin_ct = KC_open_ct #census tract with count of trip origins
census_geoinfo = KC_Census_geoinfo                    ####
#*****************************************************####
KC_Census_geoinfo <- KC_Census_geoinfo%>%
  st_transform(KC_proj)

crs(KC_Census_geoinfo)

# crs(college)

### using osm to grab data####
college <- opq (city_name) %>%
  add_osm_feature(key = 'amenity', value = c("university", "college")) %>%
  osmdata_sf(.)
# 
college <- st_geometry(college$osm_polygons) %>%
  st_transform(proj) %>%
  st_sf()%>%
  st_intersection(boundary) %>%
  st_centroid() %>%
  mutate(Legend = 'College',
         City = city_name) %>%
  dplyr::select(Legend, City, geometry)

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

# cycleway %>% st_join(census_geoinfo %>% st_intersection(boundary))

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
ggplot()+
  geom_sf(data = census_ct, fill = "white")+
  #geom_sf(data = LV_college, shape = 23, fill = "cornflowerblue", size = 2)+
  geom_sf(data = college, color = "red", size = 1.5)+
  geom_sf(data = boundary, fill='transparent')+
  labs(title = paste("Location of offices, retails, and colleges in",city_name),
       subtitle = "Red dots as office, orange dots as retails, and blue dots as colleges") +
  mapTheme()

grid.arrange(
  ggplot()+
    geom_sf(data = census_ct, fill = "white")+
    geom_sf(data = leisure, color = "lightsalmon",alpha = 0.6)+
    geom_sf(data = cycleway, color = "chartreuse3", size = 1.5, alpha = 0.6)+
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


##########################################################################
# This script is for calculating the shortest distance from each city's census tract to 
# the nearest (or 3 or 5 nearest) spatial features

# all features are calculated except for cycleway

##########################################################################

## create a panal to store spatial effects ####
census_panel <- census_geoinfo %>% st_set_geometry(NULL)

#### KNN ####
# nn function ####
nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <-
    as.matrix(measureFrom)
  measureTo_Matrix <-
    as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}
# knn for each spatial effects ####
census_panel$KNN_college <- nn_function(coordinates(as.data.frame(census_panel)[,2:3]),
                                        coordinates(college %>% st_coordinates()),
                                        1)

census_panel$KNN_restaurant <- nn_function(coordinates(as.data.frame(census_panel)[,2:3]),
                                           coordinates(restaurant %>% st_coordinates()),
                                           5)

census_panel$KNN_public_transport <- nn_function(coordinates(as.data.frame(census_panel)[,2:3]),
                                                 coordinates(public_transport %>% st_coordinates()),
                                                 5)

census_panel$KNN_office <- nn_function(coordinates(as.data.frame(census_panel)[,2:3]),
                                       coordinates(office %>% st_coordinates()),
                                       5)

census_panel$KNN_retail <- nn_function(coordinates(as.data.frame(census_panel)[,2:3]),
                                       coordinates(retail %>% st_coordinates()),
                                       5)


census_panel$KNN_tourism <- nn_function(coordinates(as.data.frame(census_panel)[,2:3]),
                                        coordinates(tourism %>% st_coordinates()),
                                        5)

census_panel$KNN_leisure <- nn_function(coordinates(as.data.frame(census_panel)[,2:3]),
                                        coordinates(leisure %>% st_coordinates()),
                                        5)

## count and density ####
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

spatial_panel <- left_join(census_panel, retail_ct%>%st_set_geometry(NULL)%>%dplyr::select(GEOID, count_retail, density_retail), by = 'GEOID') %>%
  left_join(office_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, count_office, density_office), by = 'GEOID') %>%
  left_join(leisure_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, count_leisure, density_leisure), by = 'GEOID') %>%
  left_join(tourism_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, count_tourism, density_tourism), by = 'GEOID') %>%
  left_join(public_transport_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, count_pubtran,density_pubtran), by = 'GEOID') %>%
  left_join(restaurant_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, count_restaurant, density_restaurant), by = 'GEOID') %>%
  left_join(college_ct %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, count_college, density_college), by = 'GEOID') %>%
  left_join(cycleway_ct_len %>% st_set_geometry(NULL) %>% dplyr::select(GEOID, total_length), by = 'GEOID')

spatial_panel <- replace_na(spatial_panel, 0)
spatial_census <- left_join(spatial_panel, origin_ct%>%st_set_geometry(NULL)%>%dplyr::select(-centroid_X, -centroid_Y), by = 'GEOID')
spatial_census <- spatial_census %>%
  mutate(city = city_name)

KC_spatial_panel <- spatial_panel
KC_spatial_census <- spatial_census

KC_spatial_panel_RDS <- file.path(data_directory, "~RData/Kansas City/KC_spatial_panel")
saveRDS(KC_spatial_panel,
        file = KC_spatial_panel_RDS)
KC_spatial_panel <- readRDS(KC_spatial_panel_RDS)


KC_spatial_census_RDS <- file.path(data_directory, "~RData/Kansas City/KC_spatial_census")
saveRDS(KC_spatial_census,
        file = KC_spatial_census_RDS)
KC_spatial_census <- readRDS(KC_spatial_census_RDS)
