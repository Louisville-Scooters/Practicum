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
model1 <- randomForest(ORIGINS_CNT ~ ., data = Model_clean %>% dplyr::select(-CITY, -race),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)


MD_model <- MD_model %>%
  mutate(Predicted.CNT = round(predict(model1, MD_model, type = "class"),0))

MD_result <- merge(MD_Census_geoinfo, MD_model %>% dplyr::select(GEOID, Predicted.CNT), on='GEOID')

###### race content ##########
MD_result <- merge(MD_result, as.data.frame(MD_Census) %>% dplyr::select(GEOID, pWhite), on='GEOID')
MD_result <- mutate(MD_result, race = ifelse(pWhite > .5, "Majority_White", "Majority_Non_White"))

MD_result %>%
  na.omit() %>%
  group_by(race) %>%
  summarise(mean_trip_count=mean(Predicted.CNT))

MD_result <- MD_result %>% na.omit()
mean(MD_result$Predicted.CNT)

library(viridis)
palette5 <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
ggplot() +
  geom_sf(data = MD_result %>% na.omit(), aes(fill=q5(Predicted.CNT))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(MD_result, "Predicted.CNT"),
                    name="Quintile\nBreaks") +
  labs(title = 'Prediced Trip Count in Madison, WI', size=18) +
  mapTheme()


# MD_result_RDS <- file.path(data_directory, "~RData/Madison/MD_result")
# saveRDS(MD_result,
#         file = MD_result_RDS)
MD_result <- readRDS(MD_result_RDS)

predict_MD <- ggplot()+
  geom_sf(data = MD_trimmed_result %>% na.omit(), aes(fill=q5(Predicted.CNT))) +
  scale_fill_viridis_d(labels=qBr(MD_trimmed_result,"Predicted.CNT"),name="Quintile\nBreaks")+
  labs(title = 'Predicted Trip Count for Madison, WI') +
  mapTheme()

ggsave(file.path(plot_directory,
                 "5.3 predict_MD.png"),
       plot = predict_MD,
       width = 6,
       units = "in")

Dist_MD <- ggplot()+
  geom_histogram(data = as.data.frame(MD_trimmed_result) %>% na.omit(), aes(round(Predicted.CNT)), fill='#453781FF', bins=30) +
  labs(title='Distribution of Trip Count in Madison', x='Predicted trip count') +
  plotTheme

ggsave(file.path(plot_directory,
                 "5.3 Dist_MD.png"),
       plot = Dist_MD,
       width = 6,
       units = "in")
