##########################################################################
# This script is for exporting geojsons for 6 cities' census tract geojson with census var info. 
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

MNP_model_RDS <- file.path(data_directory, "~RData/MNPstin/MNP_model")
MNP_model <- readRDS(MNP_model_RDS)

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

MNP_model_2 <- MNP_model %>% 
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

## join back census tract geometry ####

## run MNP - 21 till line 31 to get MNP_Census_geoinfo
MNP_model_tract <- merge(MNP_Census_geoinfo, MNP_model_2, by = "GEOID") %>% st_transform(4326)
MNP_model_tract$ORIGINS_CNT[is.na(MNP_model_tract$ORIGINS_CNT)] <- 0
MNP_model_tract$ORIGINS_CNT <- as.numeric(MNP_model_tract$ORIGINS_CNT)

names(MNP_model_tract)
MNP_model_tract[is.na(MNP_model_tract)] <- 0
MNP_model_tract[, 2:40] <- as.numeric(unlist(MNP_model_tract[,2:40]))
MNP_model_tract[, 42:53] <- as.numeric(unlist(MNP_model_tract[,42:53]))
MNP_model_tract <- MNP_model_tract %>%
  mutate(PWHITE = PWHITE*100,
         PTRANS = PTRANS*100,
         PDRIVE = PDRIVE*100,
         PFEMALE = PFEMALE*100,
         PCOM30PLUS = PCOM30PLUS*100,
         POCCUPIED = POCCUPIED*100,
         PVEHAVAI = PVEHAVAI*100)

glimpse(MNP_model_tract)

MNP_model_tract_RDS <- file.path(data_directory, "~RData/Minneapolis/MNP_model_tract.GeoJSON")
geojsonio::geojson_write(MNP_model_tract, file = MNP_model_tract_RDS)

## run CH - 20 till line 31 to get CH_Census_geoinfo
CH_model_tract <- merge(CH_Census_geoinfo, CH_model_2, by = "GEOID") %>% st_transform(4326)
CH_model_tract$ORIGINS_CNT[is.na(CH_model_tract$ORIGINS_CNT)] <- 0
CH_model_tract$ORIGINS_CNT <- as.numeric(CH_model_tract$ORIGINS_CNT)
CH_model_tract_RDS <- file.path(data_directory, "~RData/Chicago/CH_model_tract.GeoJSON")
geojsonio::geojson_write(CH_model_tract, file = CH_model_tract_RDS)

## run DC - 20 till line 24 to get DC_Census_geoinfo
DC_model_tract <- merge(DC_Census_geoinfo, DC_model_2, by = "GEOID") %>% st_transform(4326)
DC_model_tract$ORIGINS_CNT[is.na(DC_model_tract$ORIGINS_CNT)] <- 0
DC_model_tract$ORIGINS_CNT <- as.numeric(DC_model_tract$ORIGINS_CNT)
DC_model_tract_RDS <- file.path(data_directory, "~RData/DC/DC_model_tract.GeoJSON")
geojsonio::geojson_write(DC_model_tract, file = DC_model_tract_RDS)

## run KC - 21 till line 45 to get KC_Census_geoinfo
KC_model_tract <- merge(KC_Census_geoinfo, KC_model_2, by = "GEOID") %>% st_transform(4326)
KC_model_tract$ORIGINS_CNT[is.na(KC_model_tract$ORIGINS_CNT)] <- 0
KC_model_tract$ORIGINS_CNT <- as.numeric(KC_model_tract$ORIGINS_CNT)
KC_model_tract_RDS <- file.path(data_directory, "~RData/Kansas City/KC_model_tract.GeoJSON")
geojsonio::geojson_write(KC_model_tract, file = KC_model_tract_RDS)

## run LV - 21 till line 30 to get LV_Census_geoinfo
LV_model_tract <- merge(LV_Census_geoinfo, LV_model_2, by = "GEOID") %>% st_transform(4326)
LV_model_tract$ORIGINS_CNT[is.na(LV_model_tract$ORIGINS_CNT)] <- 0
LV_model_tract$ORIGINS_CNT <- as.numeric(LV_model_tract$ORIGINS_CNT)
LV_model_tract_RDS <- file.path(data_directory, "~RData/Louisville/LV_model_tract.GeoJSON")
geojsonio::geojson_write(LV_model_tract, file = LV_model_tract_RDS)

## run MNP - 20 till line 30 to get MNP_Census_geoinfo
MNP_model_tract <- merge(MNP_Census_geoinfo, MNP_model_2, by = "GEOID") %>% st_transform(4326)
MNP_model_tract$ORIGINS_CNT[is.na(MNP_model_tract$ORIGINS_CNT)] <- 0
MNP_model_tract$ORIGINS_CNT <- as.numeric(MNP_model_tract$ORIGINS_CNT)
MNP_model_tract_RDS <- file.path(data_directory, "~RData/Minneapolis/MNP_model_tract.GeoJSON")
geojsonio::geojson_write(MNP_model_tract, file = MNP_model_tract_RDS)


