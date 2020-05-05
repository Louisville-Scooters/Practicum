SA_spatial_census_RDS <- file.path(data_directory, "~RData/Raleigh/SA_spatial_census")
SA_spatial_census <- readRDS(SA_spatial_census_RDS)
SA_LODES_RDS <- file.path(data_directory, "~RData/Raleigh/SA_LODES")
SA_LODES <- readRDS(SA_LODES_RDS)

SA_model <- merge(SA_spatial_census, SA_LODES, by.x = 'GEOID', by.y = 'geocode')
SA_model <- SA_model %>%
  st_set_geometry(NULL)
SA_model <- SA_model %>% 
  rename_all(toupper) 
SA_model <- SA_model %>% dplyr::select(-c(MEAN_COMMUTE_TIME, CENTROID_X, CENTROID_Y),-starts_with('DENSITY'), -starts_with('COUNT'), -ends_with('LENGTH'))


# SA_model_RDS <- file.path(data_directory, "~RData/Houston/SA_model")
# saveRDS(SA_model,
#        file = SA_model_RDS)
# SA_model <- readRDS(SA_model_RDS)
library(randomForest)
model1 <- randomForest(ORIGINS_CNT ~ ., data = Model_clean %>% dplyr::select(-CITY, -race),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)


SA_model <- SA_model %>%
  mutate(Predicted.CNT = round(predict(model1, SA_model, type = "class"),0))

SA_result <- merge(SA_Census_geoinfo, SA_model %>% dplyr::select(GEOID, Predicted.CNT), on='GEOID')

###### race content ##########
SA_result <- merge(SA_result, as.data.frame(MD_Census) %>% dplyr::select(GEOID, pWhite), on='GEOID')
SA_result <- mutate(SA_result, race = ifelse(pWhite > .5, "Majority_White", "Majority_Non_White"))

SA_result %>%
  na.omit() %>%
  group_by(race) %>%
  summarise(mean_trip_count=mean(Predicted.CNT))

SA_result <- SA_result %>% na.omit()
mean(SA_result$Predicted.CNT)

library(viridis)
palette5 <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
ggplot() +
  geom_sf(data = SA_result %>% na.omit(), aes(fill=q5(Predicted.CNT))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(SA_result, "Predicted.CNT"),
                    name="Quintile\nBreaks") +
  labs(title = 'Prediced Trip Count in San Antonio, TX', size=18) +
  mapTheme()


SA_result_RDS <- file.path(data_directory, "~RData/Houston/SA_result")
saveRDS(SA_result,
        file = SA_result_RDS)
SA_result <- readRDS(SA_result_RDS)