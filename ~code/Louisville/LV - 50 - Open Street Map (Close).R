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

# !!!! Focus on XINYI's part !!!!
##########################################################################
# # Ophelia's part ####
# LV_area_of_interest <- st_read(file.path(data_directory,
#                                          "Louisville_Metro_Areas_of_Interest/Louisville_Metro_Areas_of_Interest.shp"))
# LV_area_of_interest <- LV_area_of_interest %>%
#   filter(COUNTY == 'JEFFERSON')
# 
# # recreation from shapefile
# LV_recreation <- st_read(file.path(data_directory,
#                                 "Jefferson_County_KY_Recreation_Areas__2016/Jefferson_County_KY_Recreation_Areas__2016.shp"))
# unique(LV_recreation$RL_TYPE)
# #[1] In-Ground Pool                   Pavilion, Bleacher, Batting Cage Tennis/Recreation Court         
# #[4] Sports Track                     Ball Field                       Football/Soccer Field           
# #[7] Playground                       Above-Ground Pool                Golf Course   
# 
# LV_recreation <- LV_recreation%>% 
#   st_transform(LV_proj)
# 
# LV_recreation_tract <- LV_open_ct %>% 
#   mutate(Recreation_Count = lengths(st_intersects(., LV_recreation)))
# 
# LV_college_coord <- LV_college %>% st_coordinates()
# LV_census_centroid <- st_coordinates(st_centroid(LV_Census_ct))
# 
# LV_college_dist <- get.knnx(coordinates(LV_college_coord), coordinates(LV_census_centroid),k=1)
# LV_college_dist <- as.data.frame(lapply(LV_college_dist, unlist))

LV_college <- st_read(file.path(data_directory,
                                        "Ky_Colleges_Universities/Ky_Colleges_Universities.shp"))
LV_college <- LV_college %>%
 filter(City == 'Louisville') %>%
  st_intersection(LV_SA)%>%
  st_transform(LV_proj)

### XINYI'S CODE ####
### using osm to grab data####
 LV_college2 <- opq ("Louisville USA") %>%
   add_osm_feature(key = 'amenity', value = c("university", "college")) %>%
   osmdata_sf(.)
# 
LV_college2 <- st_geometry(LV_college2$osm_points) %>%
   st_transform(LV_proj) %>%
   st_sf() %>%
   st_intersection(LV_SA) %>%
   mutate(Legend = 'College',
          City = 'Louisville') %>%
   dplyr::select(Legend, City, geometry)

ggplot()+
  geom_sf(data = LV_Census_ct, fill = "white")+
  geom_sf(data = LV_college, shape = 23, fill = "cornflowerblue", size = 2)+
  geom_sf(data = LV_college2, color = "red", size = 1.5)+
  geom_sf(data = LV_SA, fill='transparent')+
  labs(title = "Location of offices, retails, and colleges in Louisville",
       subtitle = "Red dots as office, orange dots as retails, and blue dots as colleges") +
  mapTheme()

Get_OSM <- function ()

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

# retail ####
LV_retail <- opq ("Louisville USA") %>%
  add_osm_feature(key = 'shop') %>%
  osmdata_sf(.)

LV_retail <- st_geometry(LV_retail$osm_points) %>%
  st_transform(LV_proj) %>%
  st_sf() %>%
  st_intersection(LV_SA) %>%
  mutate(Legend = 'Retails',
         City = 'Louisville') %>%
  dplyr::select(Legend, City, geometry)

# office ####
LV_office <- opq ("Louisville USA") %>%
  add_osm_feature(key = 'office') %>%
  osmdata_sf(.)

LV_office <- st_geometry(LV_office$osm_points) %>%
  st_transform(LV_proj) %>%
  st_sf() %>%
  st_intersection(LV_SA) %>%
  mutate(Legend = 'Office',
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

# leisure  ####
LV_leisure <- opq ("Louisville USA") %>%
  add_osm_feature(key = 'leisure') %>%
  osmdata_sf(.)

LV_leisure <- st_geometry(LV_leisure$osm_points) %>%
  st_transform(LV_proj) %>%
  st_sf() %>%
  st_intersection(LV_SA) %>%
  mutate(Legend = 'Leisure',
         City = 'Louisville') %>%
  dplyr::select(Legend, City, geometry)

# tourism  ####
LV_tourism <- opq ("Louisville USA") %>%
  add_osm_feature(key = 'tourism', value = c("aquarium", "artwork", "attraction", "gallery", "museumm", "theme_park", 'viewpoint', 'zoo')) %>%
  osmdata_sf(.)

LV_tourism <- st_geometry(LV_tourism$osm_points) %>%
  st_transform(LV_proj) %>%
  st_sf() %>%
  st_intersection(LV_SA) %>%
  mutate(Legend = 'Tourism',
         City = 'Louisville') %>%
  dplyr::select(Legend, City, geometry)

# street  ####
LV_street <- st_read('https://opendata.arcgis.com/datasets/f36b2c8164714b258840dce66909ba9a_1.geojson') %>%
  st_transform(LV_proj)

LV_street <- LV_street %>% st_join(LV_Census_geoinfo %>% st_intersection(LV_SA))

## code to plot and check the OSM data
grid.arrange(
ggplot()+
  geom_sf(data = LV_Census_ct, fill = "white")+
  geom_sf(data = LV_cycleway, color = "chartreuse3", size = 1.5, alpha = 0.6)+
  geom_sf(data = LV_leisure, color = "lightsalmon",alpha = 0.6)+
  geom_sf(data = LV_street, color = "lightblue",alpha = 0.6)+
  geom_sf(data = LV_SA,fill='transparent')+
  labs(title = "Location of cycleway and leisure places in Louisville",
       subtitle = "Green lines as cycleway and light pink dots as leisure places") +
  mapTheme(),

ggplot()+
  geom_sf(data = LV_Census_ct, fill = "white")+
  geom_sf(data = LV_restaurant, color = "turquoise",alpha = 0.6)+
  geom_sf(data = LV_tourism, color = "hotpink", alpha = 0.6)+
  geom_sf(data = LV_SA,fill='transparent')+
  labs(title = "Location of restaurant and tourism spots in Louisville",
       subtitle = "Turqoise dots as restaurants and pink dots as tourism spots") +
  mapTheme(),

ggplot()+
  geom_sf(data = LV_Census_ct, fill = "white")+
  geom_sf(data = LV_office, color = "indianred2", alpha = 0.6, size = 2)+
  geom_sf(data = LV_retail, color = "orange", alpha = 0.6, size = 2)+
  geom_sf(data = LV_college%>%st_intersection(LV_SA), shape = 23, fill = "cornflowerblue", size = 2)+
  geom_sf(data = LV_SA,fill='transparent')+
  labs(title = "Location of offices, retails, and colleges in Louisville",
       subtitle = "Red dots as office, orange dots as retails, and blue dots as colleges") +
  mapTheme(),
ncol = 3)
