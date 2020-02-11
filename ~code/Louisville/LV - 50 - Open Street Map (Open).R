##########################################################################
# This script is for analyzing spatial features for LV census tract
# It:
# 1. 
# This script UPDATES the following files
# 1. LV_open_ct: which has origin/dest count for each census tract, can be used for variable scatter plots,
                #and has newly added info: no. of recreation areas in each tract
##########################################################################

LV_area_of_interest <- st_read(file.path(data_directory,
                                         "Louisville_Metro_Areas_of_Interest/Louisville_Metro_Areas_of_Interest.shp"))
LV_area_of_interest <- LV_area_of_interest %>%
  filter(COUNTY == 'JEFFERSON')

LV_college <- st_read(file.path(data_directory,
                                         "Ky_Colleges_Universities/Ky_Colleges_Universities.shp"))
LV_college <- LV_college %>%
  filter(City == 'Louisville') %>%
  st_transform(LV_proj)

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
