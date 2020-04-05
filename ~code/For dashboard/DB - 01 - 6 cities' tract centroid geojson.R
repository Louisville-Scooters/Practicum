##########################################################################
# This script is for exporting geojsons for 6 cities' census tract with census var info. 
# It reads in:
# 1. City_model (x6)

# This script exports the following data:
# 1. 
##########################################################################

## Read in all City_model file ####
LV_model_RDS <- file.path(data_directory, "~RData/Louisville/LV_model")
LV_model <- readRDS(LV_model_RDS)

MNP_model_RDS <- file.path(data_directory, "~RData/Minneapolis/MNP_model")
MNP_model <- readRDS(MNP_model_RDS)

CH_model_RDS <- file.path(data_directory, "~RData/Chicago/CH_model")
CH_model <- readRDS(CH_model_RDS)

AU_model_RDS <- file.path(data_directory, "~RData/Austin/AU_model")
AU_model <- readRDS(AU_model_RDS)

DC_model_RDS <- file.path(data_directory, "~RData/DC/DC_model")
DC_model <- readRDS(DC_model_RDS)

KC_model_RDS <- file.path(data_directory, "~RData/Kansas City/KC_model")
KC_model <- readRDS(KC_model_RDS)

## Eugene rbind to City_model_2 ####
LV_model_2 <- LV_model %>% 
  rename_all(toupper) 

LV_model_2 <- LV_model_2%>% 
  dplyr::select(GEOID, names(LV_model_2), everything())

MNP_model_2 <- MNP_model %>% 
  rename_all(toupper) %>% 
  dplyr::select(GEOID, names(LV_model_2))

CH_model_2 <- CH_model %>% 
  rename_all(toupper) %>% 
  rename(GEOID = GEOID10) %>%
  dplyr::select(GEOID, names(LV_model_2))

AU_model_2 <- AU_model %>% 
  rename_all(toupper) %>% 
  rename(ORIGINS_CNT = ORIGINS_CT) %>% 
  dplyr::select(GEOID, names(LV_model_2))

DC_model_2 <- DC_model %>% 
  rename_all(toupper) %>% 
  dplyr::select(GEOID, names(LV_model_2))

KC_model_2 <- KC_model %>% 
  mutate(CITY = "KANSAS CITY") %>% 
  rename_all(toupper) %>% 
  dplyr::select(GEOID, names(LV_model_2))
## st_to_sf conversion
AU_model_centorid <- AU_model_2 %>%
  st_as_sf(coords = c("CENTROID_X", "CENTROID_Y"), 
         crs = AU_proj) %>% 
  st_transform(4326)

geojsonio::geojson_write(dat, file = "dat.GeoJSON")

st_as_sf(coords = c("EndLongitude", "EndLatitude"), 
         crs = 4326) %>% 
  st_transform(proj)