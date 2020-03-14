##########################################################################
# This script reads in:
# 1. LV_spatial_census_RDS
# 2. LV_spatial_census_RDS
# 3. LV_spatial_census_RDS
# 4. LV_spatial_census_RDS
# 5. LV_spatial_census_RDS
# 6. LV_spatial_census_RDS
#
# Please update this whenever you create a function that everyone will need to use
# or load a new package
##########################################################################
#Louisville
LV_spatial_census_RDS <- file.path(data_directory, "~RData/Louisville/LV_spatial_census")
LV_spatial_census <- readRDS(LV_spatial_census_RDS)
LV_LODES_RDS <- file.path(data_directory, "~RData/Louisville/LV_LODES")
LV_LODES <- readRDS(LV_LODES_RDS)
LV_model <- merge(LV_spatial_census, LV_LODES, by.x = 'GEOID', by.y = 'geocode')
LV_model <- LV_model%>%st_set_geometry(NULL)
LV_model_RDS <- file.path(data_directory, "~RData/Louisville/LV_model")
saveRDS(LV_model,
        file = LV_model_RDS)
LV_model <- readRDS(LV_model_RDS)


MNP_spatial_census_RDS <- file.path(data_directory, "~RData/MNP/MNP_spatial_census")
MNP_spatial_census <- readRDS(MNP_spatial_census_RDS)
MNP_LODES_RDS <- file.path(data_directory, "~RData/Minneapolis/MNP_LODES")
MNP_LODES <- readRDS(MNP_LODES_RDS)
MNP_model <- merge(MNP_spatial_census, MNP_LODES, by.x = 'GEOID', by.y = 'geocode')

MNP_model_RDS <- file.path(data_directory, "~RData/Minneapolis/MNP_model")
saveRDS(MNP_model,
        file = MNP_model_RDS)
MNP_model <- readRDS(MNP_model_RDS)

CH_spatial_census_RDS <- file.path(data_directory, "~RData/Chicago/CH_spatial_census")
CH_spatial_census <- readRDS(CH_spatial_census_RDS)
CH_LODES_RDS <- file.path(data_directory, "~RData/Chicago/CH_LODES")
CH_LODES <- readRDS(CH_LODES_RDS)
CH_model <- merge(CH_spatial_census, CH_LODES, by.x = 'geoid10', by.y = 'geocode')
CH_model <- CH_model%>%st_set_geometry(NULL)
CH_model_RDS <- file.path(data_directory, "~RData/Chicago/CH_model")
saveRDS(CH_model,
        file = CH_model_RDS)
CH_model <- readRDS(CH_model_RDS)

AU_spatial_census_RDS <- file.path(data_directory, "~RData/Austin/AU_spatial_census")
AU_spatial_census <- readRDS(AU_spatial_census_RDS)
AU_LODES_RDS <- file.path(data_directory, "~RData/Austin/AU_LODES")
AU_LODES <- readRDS(AU_LODES_RDS)
AU_model <- merge(AU_spatial_census, AU_LODES, by.x = 'GEOID', by.y = 'geocode')
AU_model <- AU_model %>%
  dplyr::select(-street_length.x, -street_length.y)
AU_model_RDS <- file.path(data_directory, "~RData/Austin/AU_model")
saveRDS(AU_model,
        file = AU_model_RDS)
AU_model <- readRDS(AU_model_RDS)

DC_spatial_census_RDS <- file.path(data_directory, "~RData/DC/DC_spatial_census")
DC_spatial_census <- readRDS(DC_spatial_census_RDS)
DC_LODES_RDS <- file.path(data_directory, "~RData/DC/DC_LODES")
DC_LODES <- readRDS(DC_LODES_RDS)
DC_model <- merge(DC_spatial_census, DC_LODES, by.x = 'GEOID', by.y = 'geocode')
DC_model_RDS <- file.path(data_directory, "~RData/DC/DC_model")
saveRDS(DC_model,
        file = DC_model_RDS)
DC_model <- readRDS(DC_model_RDS)

KC_spatial_census_RDS <- file.path(data_directory, "~RData/Kansas City/KC_spatial_census")
KC_spatial_census <- readRDS(KC_spatial_census_RDS)
KC_LODES_RDS <- file.path(data_directory, "~RData/Kansas City/KC_LODES")
KC_LODES <- readRDS(KC_LODES_RDS)
KC_model <- merge(KC_spatial_census, KC_LODES, by.x = 'GEOID', by.y = 'geocode')

KC_model_RDS <- file.path(data_directory, "~RData/Kansas City/KC_model")
saveRDS(KC_model,
        file = KC_model_RDS)
KC_model <- readRDS(KC_model_RDS)

### rbind together 
Model_panel <- rbind(LV_model, MNP_model, CH_model, DC_model)
Model_panel <- rbind(Model_panel, AU_model)
Model_panel_RDS <- file.path(data_directory, "~RData/Model_panel")
saveRDS(Model_panel,
        file = Model_panel_RDS)
Model_panel <- readRDS(Model_panel_RDS)


glimpse(Model_panel)
glimpse(AU_model)
names(KC_model)

### Eugene rbind
LV_model_2 <- LV_model %>% 
  rename_all(toupper) %>% 
  dplyr::select(GEOID, names(LV_model_2), everything())

MNP_model_2 <- MNP_model %>% 
  rename_all(toupper) %>% 
  dplyr::select(GEOID, names(LV_model_2))

CH_model_2 <- CH_model %>% 
  rename_all(toupper) %>% 
  rename(GEOID = GEOID10) %>% 
  dplyr::select(names(LV_model_2))

AU_model_2 <- AU_model %>% 
  rename_all(toupper) %>% 
  rename(ORIGINS_CNT = ORIGINS_CT) %>% 
  dplyr::select(GEOID, names(LV_model_2))

DC_model_2 <- DC_model %>% 
  rename_all(toupper) %>% 
  dplyr::select(GEOID, names(LV_model_2))
  
KC_model_2 <- KC_model %>% 
  st_drop_geometry() %>% 
  mutate(CITY = "KANSAS CITY") %>% 
  rename_all(toupper) %>% 
  dplyr::select(GEOID, names(LV_model_2))
  
Model_panel_2 <- rbind(LV_model_2,
                       MNP_model_2,
                       CH_model_2,
                       AU_model_2,
                       DC_model_2,
                       KC_model_2)

Model_panel_2_RDS <- file.path(data_directory, "~RData/Model_panel_2")
saveRDS(Model_panel_2,
        file = Model_panel_2_RDS)
