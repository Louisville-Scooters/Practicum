AV_spatial_census_RDS <- file.path(data_directory, "~RData/Asheville/AV_spatial_census")
AV_spatial_census <- readRDS(AV_spatial_census_RDS)
AV_LODES_RDS <- file.path(data_directory, "~RData/Asheville/AV_LODES")
AV_LODES <- readRDS(AV_LODES_RDS)

AV_model <- merge(AV_spatial_census, AV_LODES, by.x = 'GEOID', by.y = 'geocode')
AV_model <- AV_model %>%
  st_set_geometry(NULL)
AV_model <- AV_model %>% 
  rename_all(toupper) 
AV_model <- AV_model %>% dplyr::select(-c(MEAN_COMMUTE_TIME, CENTROID_X, CENTROID_Y),-starts_with('DENSITY'), -starts_with('COUNT'), -ends_with('LENGTH'))


# AV_model_RDS <- file.path(data_directory, "~RData/Asheville/AV_model")
# saveRDS(AV_model,
#        file = AV_model_RDS)
# AV_model <- readRDS(AV_model_RDS)

model1 <- randomForest(ORIGINS_CNT ~ ., data = Model_clean %>% dplyr::select(-CITY, -race),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)


AV_model <- AV_model %>%
  mutate(Predicted.CNT = round(predict(model1, AV_model, type = "class"),0))

AV_result <- merge(AV_Census_geoinfo, AV_model %>% dplyr::select(GEOID, Predicted.CNT), on='GEOID')

###### race content ##########
AV_result <- merge(AV_result, as.data.frame(MD_Census) %>% dplyr::select(GEOID, pWhite), on='GEOID')
AV_result <- mutate(AV_result, race = ifelse(pWhite > .5, "Majority_White", "Majority_Non_White"))

AV_result %>%
  na.omit() %>%
  group_by(race) %>%
  summarise(mean_trip_count=mean(Predicted.CNT))

AV_result <- AV_result %>% na.omit()
mean(AV_result$Predicted.CNT)

library(viridis)
palette5 <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
ggplot() +
  geom_sf(data = AV_result %>% na.omit(), aes(fill=q5(Predicted.CNT))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(AV_result, "Predicted.CNT"),
                    name="Quintile\nBreaks") +
  labs(title = 'Prediced Trip Count in Asheville, NY', size=18) +
  mapTheme()


# AV_result_RDS <- file.path(data_directory, "~RData/Asheville/AV_result")
# saveRDS(AV_result,
#         file = AV_result_RDS)
AV_result <- readRDS(AV_result_RDS)