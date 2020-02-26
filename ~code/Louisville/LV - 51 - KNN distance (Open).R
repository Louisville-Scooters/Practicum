##########################################################################
# This script is for calculating the shortest distance from LV's census tract to 
# the nearest (or 3 or 5 nearest) spatial features

# all features are calculated except for cycleway

##########################################################################

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
LV_Census_panel$KNN_college <- nn_function(coordinates(as.data.frame(LV_Census_panel)[,2:3]),
                                              coordinates(LV_college %>% st_coordinates()),
                                              1)
ggplot() +
  geom_sf(data=LV_Census_panel ,aes(fill=KNN_college))+
  scale_fill_viridis(direction = -1)

LV_Census_panel$KNN_restaurant <- nn_function(coordinates(as.data.frame(LV_Census_panel)[,2:3]),
                                              coordinates(LV_restaurant %>% st_coordinates()),
                                              5)
ggplot() +
  geom_sf(data=LV_Census_panel ,aes(fill=KNN_restaurant))+
  scale_fill_viridis(direction = -1)

LV_Census_panel$KNN_public_transport <- nn_function(coordinates(as.data.frame(LV_Census_panel)[,2:3]),
                                                    coordinates(LV_public_transport %>% st_coordinates()),
                                                    5)
ggplot() +
  geom_sf(data=LV_Census_panel ,aes(fill=KNN_public_transport))+
  scale_fill_viridis(direction = -1)

LV_Census_panel$KNN_office <- nn_function(coordinates(as.data.frame(LV_Census_panel)[,2:3]),
                                          coordinates(LV_office %>% st_coordinates()),
                                          5)

LV_Census_panel$KNN_retail <- nn_function(coordinates(as.data.frame(LV_Census_panel)[,2:3]),
                                          coordinates(LV_retail %>% st_coordinates()),
                                          5)


LV_Census_panel$KNN_tourism <- nn_function(coordinates(as.data.frame(LV_Census_panel)[,2:3]),
                                           coordinates(LV_tourism %>% st_coordinates()),
                                           5)

LV_Census_panel$KNN_leisure <- nn_function(coordinates(as.data.frame(LV_Census_panel)[,2:3]),
                                           coordinates(LV_leisure %>% st_coordinates()),
                                           5)
