MD_spatial_census_RDS <- file.path(data_directory, "~RData/Madison/MD_spatial_census")
MD_spatial_census <- readRDS(MD_spatial_census_RDS)
MD_LODES_RDS <- file.path(data_directory, "~RData/Madison/MD_LODES")
MD_LODES <- readRDS(MD_LODES_RDS)
MD_model <- merge(MD_spatial_census, MD_LODES, by.x = 'GEOID', by.y = 'geocode')
MD_model <- MD_model %>%
  st_set_geometry(NULL)
MD_model <- MD_model %>% 
  rename_all(toupper) 
MD_model <- MD_model %>% dplyr::select(-c(MEAN_COMMUTE_TIME, CENTROID_X, CENTROID_Y),-starts_with('DENSITY'), -starts_with('COUNT'), -ends_with('LENGTH'))


# MD_model_RDS <- file.path(data_directory, "~RData/Kansas City/MD_model")
# saveRDS(MD_model,
#        file = MD_model_RDS)
# MD_model <- readRDS(MD_model_RDS)

# model1 <- randomForest(ORIGINS_CNT ~ ., data = Model_clean_census %>% dplyr::select(-CITY),
#                        ntree = 1000, 
#                        mtry = 2, engine = 'ranger', importance = TRUE)
# 
# model2 <- randomForest(ORIGINS_CNT ~ ., data = Model_clean_osm %>% dplyr::select(-CITY),
#                        ntree = 1000, 
#                        mtry = 2, engine = 'ranger', importance = TRUE)
# 
# # Predicting on train set
# Model_rf <- Model_clean %>%
#   dplyr::select(GEOID, ORIGINS_CNT) %>%
#   mutate(Predicted.CNT.CENSUS = round(predict(model1, Model_clean_census, type = "class"),0))
# 
# Model_rf <- Model_rf %>%
#   mutate(Predicted.CNT.OSM = round(predict(model2, Model_clean_osm, type = "class"),0))
# 
# model3 <- randomForest(ORIGINS_CNT ~ ., data = Model_rf %>% dplyr::select(-GEOID),
#                        ntree = 1000, 
#                        mtry = 2, engine = 'ranger', importance = TRUE)
# 
# Model_rf <- Model_rf %>%
#   mutate(Predicted.CNT = round(predict(model3, Model_rf, type = "class"),0))
# 
# mean(abs(Model_rf$Predicted.CNT - Model_rf$ORIGINS_CNT))
# mean(Model_rf$ORIGINS_CNT)

# predict on MD data
census_var <- colnames(MD_model)[c(1,11:23)]
osm_var <- colnames(MD_model)[c(1:10, 24:25)]

MD_model_census <- MD_model %>% dplyr::select(census_var)
MD_model_osm <- MD_model %>% dplyr::select(osm_var)
Model_MD_rf <- MD_model_census %>%
  mutate(Predicted.CNT.CENSUS = round(predict(model1, MD_model_census, type = "class"),0))

Model_MD_rf <- Model_MD_rf %>%
  mutate(Predicted.CNT.OSM = round(predict(model2, MD_model_osm, type = "class"),0))

Model_MD_rf <- Model_MD_rf %>%
  mutate(Predicted.CNT = round(predict(model3, Model_MD_rf, type = "class"),0))

MD_result <- merge(MD_Census_geoinfo, Model_MD_rf %>% dplyr::select(GEOID, Predicted.CNT), on='GEOID')

library(viridis)
palette5 <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
ggplot() +
  geom_sf(data = MD_result %>% na.omit(), aes(fill=q5(Predicted.CNT))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(MD_result, "Predicted.CNT"),
                    name="Quintile\nBreaks") +
  labs(title = 'Prediced Trip Count in Madison, WI', size=18) +
  mapTheme()
