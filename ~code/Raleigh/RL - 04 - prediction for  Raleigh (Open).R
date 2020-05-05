RL_spatial_census_RDS <- file.path(data_directory, "~RData/Raleigh/RL_spatial_census")
RL_spatial_census <- readRDS(RL_spatial_census_RDS)
RL_LODES_RDS <- file.path(data_directory, "~RData/Raleigh/RL_LODES")
RL_LODES <- readRDS(RL_LODES_RDS)
RL_model <- merge(RL_spatial_census, RL_LODES, by.x = 'GEOID', by.y = 'geocode')
RL_model <- RL_model %>%
  st_set_geometry(NULL)
RL_model <- RL_model %>% 
  rename_all(toupper) 
RL_model <- RL_model %>% dplyr::select(-c(MEAN_COMMUTE_TIME, CENTROID_X, CENTROID_Y),-starts_with('DENSITY'), -starts_with('COUNT'), -ends_with('LENGTH'))


# RL_model_RDS <- file.path(data_directory, "~RData/Kansas City/RL_model")
# saveRDS(RL_model,
#        file = RL_model_RDS)
# RL_model <- readRDS(RL_model_RDS)

model1 <- randomForest(ORIGINS_CNT ~ ., data = Model_clean %>% dplyr::select(-CITY, -race),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)


RL_model <- RL_model %>%
  mutate(Predicted.CNT = round(predict(model1, RL_model, type = "class"),0))

RL_result <- merge(RL_Census_geoinfo, RL_model %>% dplyr::select(GEOID, Predicted.CNT), on='GEOID')

###### race content ##########
RL_result <- merge(RL_result, as.data.frame(MD_Census) %>% dplyr::select(GEOID, pWhite), on='GEOID')
RL_result <- mutate(RL_result, race = ifelse(pWhite > .5, "Majority_White", "Majority_Non_White"))

RL_result %>%
  na.omit() %>%
  group_by(race) %>%
  summarise(mean_trip_count=mean(Predicted.CNT))

RL_result <- RL_result %>% na.omit()
mean(RL_result$Predicted.CNT)

library(viridis)
palette5 <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
ggplot() +
  geom_sf(data = RL_result %>% na.omit(), aes(fill=q5(Predicted.CNT))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(RL_result, "Predicted.CNT"),
                    name="Quintile\nBreaks") +
  labs(title = 'Prediced Trip Count in Philadelphia, PA', size=18) +
  mapTheme()


RL_result_RDS <- file.path(data_directory, "~RData/Raleigh/RL_result")
saveRDS(RL_result,
        file = RL_result_RDS)
RL_result <- readRDS(RL_result_RDS)