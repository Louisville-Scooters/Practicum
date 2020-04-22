PH_spatial_census_RDS <- file.path(data_directory, "~RData/Philadelphia/PH_spatial_census")
PH_spatial_census <- readRDS(PH_spatial_census_RDS)
PH_LODES_RDS <- file.path(data_directory, "~RData/Philadelphia/PH_LODES")
PH_LODES <- readRDS(PH_LODES_RDS)
PH_model <- merge(PH_spatial_census, PH_LODES, by.x = 'GEOID', by.y = 'geocode')
PH_model <- PH_model %>%
  st_set_geometry(NULL)
PH_model <- PH_model %>% 
  rename_all(toupper) 
PH_model <- PH_model %>% dplyr::select(-c(AREA, MEAN_COMMUTE_TIME, CENTROID_X, CENTROID_Y),-starts_with('DENSITY'), -starts_with('COUNT'), -ends_with('LENGTH'))


# PH_model_RDS <- file.path(data_directory, "~RData/Kansas City/PH_model")
# saveRDS(PH_model,
#        file = PH_model_RDS)
# PH_model <- readRDS(PH_model_RDS)

model1 <- randomForest(ORIGINS_CNT ~ ., data = Model_clean_census %>% dplyr::select(-CITY),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)

model2 <- randomForest(ORIGINS_CNT ~ ., data = Model_clean_osm %>% dplyr::select(-CITY),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)

# Predicting on train set
Model_rf <- Model_clean %>%
  dplyr::select(GEOID, ORIGINS_CNT) %>%
  mutate(Predicted.CNT.CENSUS = round(predict(model1, Model_clean_census, type = "class"),0))

Model_rf <- Model_rf %>%
  mutate(Predicted.CNT.OSM = round(predict(model2, Model_clean_osm, type = "class"),0))

model3 <- randomForest(ORIGINS_CNT ~ ., data = Model_rf %>% dplyr::select(-GEOID),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)

Model_rf <- Model_rf %>%
  mutate(Predicted.CNT = round(predict(model3, Model_rf, type = "class"),0))

mean(abs(Model_rf$Predicted.CNT - Model_rf$ORIGINS_CNT))
mean(Model_rf$ORIGINS_CNT)

# predict on PH data
census_var <- colnames(PH_model)[c(1,11:23)]
osm_var <- colnames(PH_model)[c(1:10, 24:25)]

PH_model_census <- PH_model %>% dplyr::select(census_var)
PH_model_osm <- PH_model %>% dplyr::select(osm_var)
Model_PH_rf <- PH_model_census %>%
  mutate(Predicted.CNT.CENSUS = round(predict(model1, PH_model_census, type = "class"),0))

Model_PH_rf <- Model_PH_rf %>%
  mutate(Predicted.CNT.OSM = round(predict(model2, PH_model_osm, type = "class"),0))

Model_PH_rf <- Model_PH_rf %>%
  mutate(Predicted.CNT = round(predict(model3, Model_PH_rf, type = "class"),0))

PH_result <- merge(PH_Census_geoinfo, Model_PH_rf %>% dplyr::select(GEOID, Predicted.CNT), on='GEOID')

library(viridis)
palette5 <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
ggplot() +
  geom_sf(data = PH_result %>% na.omit(), aes(fill=q5(Predicted.CNT))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(PH_result, "Predicted.CNT"),
                    name="Quintile\nBreaks") +
  labs(title = 'Prediced Trip Count in Philadelphia, PA', size=18) +
  mapTheme()
