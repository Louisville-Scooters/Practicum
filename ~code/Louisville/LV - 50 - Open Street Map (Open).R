##########################################################################
# This script is for analyzing spatial features for LV census tract
# It:
# 1. 
# This script UPDATES the following files
# 1. LV_open_ct: which has origin/dest count for each census tract, can be used for variable scatter plots,
                #and has newly added info: no. of recreation areas in each tract
##########################################################################
# college from shapefile ####
LV_area_of_interest <- st_read(file.path(data_directory,
                                         "Louisville_Metro_Areas_of_Interest/Louisville_Metro_Areas_of_Interest.shp"))
LV_area_of_interest <- LV_area_of_interest %>%
  filter(COUNTY == 'JEFFERSON')

LV_college <- st_read(file.path(data_directory,
                                         "Ky_Colleges_Universities/Ky_Colleges_Universities.shp"))
LV_college <- LV_college %>%
  filter(City == 'Louisville') %>%
  st_transform(LV_proj)

# recreation from shapefile ####
LV_recreation <- st_read(file.path(data_directory,
                                "Jefferson_County_KY_Recreation_Areas__2016/Jefferson_County_KY_Recreation_Areas__2016.shp"))
unique(LV_recreation$RL_TYPE)
#[1] In-Ground Pool                   Pavilion, Bleacher, Batting Cage Tennis/Recreation Court         
#[4] Sports Track                     Ball Field                       Football/Soccer Field           
#[7] Playground                       Above-Ground Pool                Golf Course   

LV_recreation <- LV_recreation%>% 
  st_transform(LV_proj)

LV_recreation_tract <- LV_open_ct %>% 
  mutate(Recreation_Count = lengths(st_intersects(., LV_recreation)))

LV_college_coord <- LV_college %>% st_coordinates()
LV_census_centroid <- st_coordinates(st_centroid(LV_Census_ct))

LV_college_dist <- get.knnx(coordinates(LV_college_coord), coordinates(LV_census_centroid),k=1)
LV_college_dist <- as.data.frame(lapply(LV_college_dist, unlist))

test <- merge(LV_open_ct, LV_college_dist)
remove(test)

### XINYI'S CODE ####
### using osm to grab data####
# restaurant ####
LV_restaurant <- opq ("Louisville USA") %>%
  add_osm_feature(key = 'amenity', value = c("restaurant", "fast_food")) %>%
  osmdata_sf(.)

LV_restaurant <- st_geometry(LV_restaurant$osm_points) %>%
  st_transform(LV_proj) %>%
  st_sf() %>%
  st_intersection(LV_SA) %>%
  mutate(Legend = 'Restaurant',
         City = 'Louisville') %>%
  dplyr::select(Legend, City, geometry)

# public transport ####
LV_public_transport <- opq ("Louisville USA") %>%
  add_osm_feature(key = 'public_transport', value = c("stop_position", "station")) %>%
  osmdata_sf(.)

LV_public_transport <- st_geometry(LV_public_transport$osm_points) %>%
  st_transform(LV_proj) %>%
  st_sf() %>%
  st_intersection(LV_SA) %>%
  mutate(Legend = 'Public.Transport',
         City = 'Louisville') %>%
  dplyr::select(Legend, City, geometry)

# retail and mall ####
# landuse = commercial & retail has include all offices, malls, shops.
LV_retail <- opq ("Louisville USA") %>%
  add_osm_feature(key = 'landuse', value = c("commercial", "retail")) %>%
  osmdata_sf(.)

LV_retail <- st_geometry(LV_retail$osm_points) %>%
  st_transform(LV_proj) %>%
  st_sf() %>%
  st_intersection(LV_SA) %>%
  mutate(Legend = 'Retails',
         City = 'Louisville') %>%
  dplyr::select(Legend, City, geometry)

# cycleway ####
LV_cycleway <- opq ("Louisville USA") %>%
  add_osm_feature(key = 'cycleway') %>%
  osmdata_sf(.)

LV_cycleway <- st_geometry(LV_cycleway$osm_lines) %>%
  st_transform(LV_proj) %>%
  st_sf() %>%
  st_intersection(LV_SA) %>%
  mutate(Legend = 'Cycleway',
         City = 'Louisville') %>%
  dplyr::select(Legend, City, geometry)

LV_cycleway %>% st_join(LV_Census_geoinfo %>% st_intersection(LV_SA))

## code to plot and check the OSM data
# ggplot()+
#   geom_sf(data = LV_office)+
#   geom_sf(data = LV_SA,fill='transparent')
## create a panal to store spatial effects ####
LV_Census_panel <- LV_Census_geoinfo %>% st_intersection(LV_SA %>% dplyr::select(geometry))

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
LV_Census_panel$KNN_university <- nn_function(coordinates(as.data.frame(LV_Census_panel)[,2:3]),
                                              coordinates(LV_college %>% st_coordinates()),
                                              1)
ggplot() +
  geom_sf(data=LV_Census_panel ,aes(fill=KNN_university))+
  scale_fill_viridis(direction = -1)

LV_Census_panel$KNN_restaurant <- nn_function(coordinates(as.data.frame(LV_Census_panel)[,2:3]),
                                              coordinates(LV_restaurant %>% st_coordinates()),
                                              5)
ggplot() +
  geom_sf(data=LV_Census_panel ,aes(fill=KNN_restaurant))+
  scale_fill_viridis(direction = -1)

LV_Census_panel$KNN_public_transport <- nn_function(coordinates(as.data.frame(LV_Census_panel)[,2:3]),
                                              coordinates(LV_public_transport %>% st_coordinates()),
                                              10)
ggplot() +
  geom_sf(data=LV_Census_panel ,aes(fill=KNN_public_transport))+
  scale_fill_viridis(direction = -1)

#### COUNT DENSITY ####
LV_Census_geoinfo$area <- as.numeric(st_area(LV_Census_geoinfo))*9.29e-8
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

