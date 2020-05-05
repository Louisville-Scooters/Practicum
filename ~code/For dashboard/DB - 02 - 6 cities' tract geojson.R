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

## run AU - 21 till line 31 to get AU_Census_geoinfo
AU_model_tract <- merge(AU_Census_geoinfo, AU_model_2, by = "GEOID") %>% st_transform(4326)
AU_model_tract$ORIGINS_CNT[is.na(AU_model_tract$ORIGINS_CNT)] <- 0
AU_model_tract$ORIGINS_CNT <- as.numeric(AU_model_tract$ORIGINS_CNT)

AU_model_tract[is.na(AU_model_tract)] <- 0
AU_model_tract <- AU_model_tract %>%
  mutate(PWHITE = PWHITE*100,
         PTRANS = PTRANS*100,
         PDRIVE = PDRIVE*100,
         PFEMALE = PFEMALE*100,
         PCOM30PLUS = PCOM30PLUS*100,
         POCCUPIED = POCCUPIED*100,
         PVEHAVAI = PVEHAVAI*100)

AU_model_tract_RDS <- file.path(data_directory, "~RData/Austin/AU_model_tract.GeoJSON")
geojsonio::geojson_write(AU_model_tract, file = AU_model_tract_RDS)

## run CH - 20 till line 31 to get CH_Census_geoinfo
CH_model_tract <- merge(CH_Census_geoinfo, CH_model_2, by = "GEOID") %>% st_transform(4326)
CH_model_tract$ORIGINS_CNT[is.na(CH_model_tract$ORIGINS_CNT)] <- 0
CH_model_tract$ORIGINS_CNT <- as.numeric(CH_model_tract$ORIGINS_CNT)

CH_model_tract[is.na(CH_model_tract)] <- 0
CH_model_tract <- CH_model_tract %>%
  mutate(PWHITE = PWHITE*100,
         PTRANS = PTRANS*100,
         PDRIVE = PDRIVE*100,
         PFEMALE = PFEMALE*100,
         PCOM30PLUS = PCOM30PLUS*100,
         POCCUPIED = POCCUPIED*100,
         PVEHAVAI = PVEHAVAI*100)
CH_model_tract_RDS <- file.path(data_directory, "~RData/Chicago/CH_model_tract.GeoJSON")
geojsonio::geojson_write(CH_model_tract, file = CH_model_tract_RDS)

## run DC - 20 till line 24 to get DC_Census_geoinfo
DC_model_tract <- merge(DC_Census_geoinfo, DC_model_2, by = "GEOID") %>% st_transform(4326)
DC_model_tract$ORIGINS_CNT[is.na(DC_model_tract$ORIGINS_CNT)] <- 0
DC_model_tract$ORIGINS_CNT <- as.numeric(DC_model_tract$ORIGINS_CNT)

DC_model_tract[is.na(DC_model_tract)] <- 0
DC_model_tract <- DC_model_tract %>%
  mutate(PWHITE = PWHITE*100,
         PTRANS = PTRANS*100,
         PDRIVE = PDRIVE*100,
         PFEMALE = PFEMALE*100,
         PCOM30PLUS = PCOM30PLUS*100,
         POCCUPIED = POCCUPIED*100,
         PVEHAVAI = PVEHAVAI*100)

DC_model_tract_RDS <- file.path(data_directory, "~RData/DC/DC_model_tract.GeoJSON")
geojsonio::geojson_write(DC_model_tract, file = DC_model_tract_RDS)

## run KC - 21 till line 45 to get KC_Census_geoinfo
KC_model_tract <- merge(KC_Census_geoinfo, KC_model_2, by = "GEOID") %>% st_transform(4326)
KC_model_tract$ORIGINS_CNT[is.na(KC_model_tract$ORIGINS_CNT)] <- 0
KC_model_tract$ORIGINS_CNT <- as.numeric(KC_model_tract$ORIGINS_CNT)

KC_model_tract[is.na(KC_model_tract)] <- 0
KC_model_tract <- KC_model_tract %>%
  mutate(PWHITE = PWHITE*100,
         PTRANS = PTRANS*100,
         PDRIVE = PDRIVE*100,
         PFEMALE = PFEMALE*100,
         PCOM30PLUS = PCOM30PLUS*100,
         POCCUPIED = POCCUPIED*100,
         PVEHAVAI = PVEHAVAI*100)

KC_model_tract_RDS <- file.path(data_directory, "~RData/Kansas City/KC_model_tract.GeoJSON")
geojsonio::geojson_write(KC_model_tract, file = KC_model_tract_RDS)

## run LV - 20 till line 30 to get LV_Census_geoinfo
LV_model_tract <- merge(LV_Census_geoinfo, LV_model_2, by = "GEOID") %>% st_transform(4326)
names(LV_model_tract)

LV_model_tract <- LV_model_tract %>%
  dplyr::select(-centroid_X, -centroid_Y)

LV_model_tract$ORIGINS_CNT[is.na(LV_model_tract$ORIGINS_CNT)] <- 0
LV_model_tract$ORIGINS_CNT <- as.numeric(LV_model_tract$ORIGINS_CNT)

LV_model_tract[is.na(LV_model_tract)] <- 0
LV_model_tract <- LV_model_tract %>%
  mutate(PWHITE = PWHITE*100,
         PTRANS = PTRANS*100,
         PDRIVE = PDRIVE*100,
         PFEMALE = PFEMALE*100,
         PCOM30PLUS = PCOM30PLUS*100,
         POCCUPIED = POCCUPIED*100,
         PVEHAVAI = PVEHAVAI*100)

LV_model_tract_RDS <- file.path(data_directory, "~RData/Louisville/LV_model_tract.GeoJSON")
geojsonio::geojson_write(LV_model_tract, file = LV_model_tract_RDS)

## run MNP - 20 till line 30 to get MNP_Census_geoinfo
MNP_model_tract <- merge(MNP_Census_geoinfo, MNP_model_2, by = "GEOID") %>% st_transform(4326)
MNP_model_tract$ORIGINS_CNT[is.na(MNP_model_tract$ORIGINS_CNT)] <- 0
MNP_model_tract$ORIGINS_CNT <- as.numeric(MNP_model_tract$ORIGINS_CNT)

MNP_model_tract[is.na(MNP_model_tract)] <- 0
MNP_model_tract <- MNP_model_tract %>%
  mutate(PWHITE = PWHITE*100,
         PTRANS = PTRANS*100,
         PDRIVE = PDRIVE*100,
         PFEMALE = PFEMALE*100,
         PCOM30PLUS = PCOM30PLUS*100,
         POCCUPIED = POCCUPIED*100,
         PVEHAVAI = PVEHAVAI*100)

MNP_model_tract_RDS <- file.path(data_directory, "~RData/Minneapolis/MNP_model_tract.GeoJSON")
geojsonio::geojson_write(MNP_model_tract, file = MNP_model_tract_RDS)

## Philadelphia 
PH_result_RDS <- file.path(data_directory, "~RData/Philadelphia/PH_result")
PH_result <- readRDS(PH_result_RDS)

PH_model_tract <- merge(PH_result %>%dplyr::select(-centroid_X, -centroid_Y, -pWhite), PH_model, by = "GEOID")

PH_model_tract <- PH_model_tract %>% 
  rename_all(toupper) 

PH_model_tract <- PH_model_tract %>% st_transform(4326)

PH_model_tract$PREDICTED.CNT[is.na(PH_model_tract$PREDICTED.CNT)] <- 0
PH_model_tract$PREDICTED.CNT <- as.numeric(PH_model_tract$PREDICTED.CNT)

PH_model_tract[is.na(PH_model_tract)] <- 0
PH_model_tract <- PH_model_tract %>%
  mutate(PWHITE = PWHITE*100,
         PTRANS = PTRANS*100,
         PDRIVE = PDRIVE*100,
         PFEMALE = PFEMALE*100,
         PCOM30PLUS = PCOM30PLUS*100,
         POCCUPIED = POCCUPIED*100,
         PVEHAVAI = PVEHAVAI*100)

PH_model_tract_RDS <- file.path(data_directory, "~RData/Philadelphia/PH_model_tract.GeoJSON")
geojsonio::geojson_write(PH_model_tract, file = PH_model_tract_RDS)

## Madison 
MD_result_RDS <- file.path(data_directory, "~RData/Madison/MD_result")
MD_result <- readRDS(MD_result_RDS)

MD_model_tract <- merge(MD_result %>%dplyr::select(-centroid_X, -centroid_Y, -pWhite), MD_model, by = "GEOID")

MD_model_tract <- MD_model_tract %>% 
  rename_all(toupper) 

MD_model_tract <- MD_model_tract %>% st_transform(4326)

MD_model_tract$PREDICTED.CNT[is.na(MD_model_tract$PREDICTED.CNT)] <- 0
MD_model_tract$PREDICTED.CNT <- as.numeric(MD_model_tract$PREDICTED.CNT)

MD_model_tract[is.na(MD_model_tract)] <- 0
MD_model_tract <- MD_model_tract %>%
  mutate(PWHITE = PWHITE*100,
         PTRANS = PTRANS*100,
         PDRIVE = PDRIVE*100,
         PFEMALE = PFEMALE*100,
         PCOM30PLUS = PCOM30PLUS*100,
         POCCUPIED = POCCUPIED*100,
         PVEHAVAI = PVEHAVAI*100)

MD_model_tract_RDS <- file.path(data_directory, "~RData/Madison/MD_model_tract.GeoJSON")
geojsonio::geojson_write(MD_model_tract, file = MD_model_tract_RDS)


AU_model_tract <- st_read(AU_model_tract_RDS)
CH_model_tract <- st_read(CH_model_tract_RDS)
DC_model_tract <- st_read(DC_model_tract_RDS)
KC_model_tract <- st_read(KC_model_tract_RDS)
LV_model_tract <- st_read(LV_model_tract_RDS)
PH_model_tract <- st_read(PH_model_tract_RDS)
MD_model_tract <- st_read(MD_model_tract_RDS)

sum(MD_model_tract$JOBS_IN_TRACT)
sum(MD_model_tract$PREDICTED.CNT)

sum(PH_model_tract$JOBS_IN_TRACT)
sum(PH_model_tract$PREDICTED.CNT)

PH_model_tract <- st_read(PH_model_tract_RDS)
names(PH_model_tract)
