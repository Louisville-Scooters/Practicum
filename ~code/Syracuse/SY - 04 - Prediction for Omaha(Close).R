SY_spatial_census_RDS <- file.path(data_directory, "~RData/Syracuse/SY_spatial_census")
SY_spatial_census <- readRDS(SY_spatial_census_RDS)
SY_LODES_RDS <- file.path(data_directory, "~RData/Syracuse/SY_LODES")
SY_LODES <- readRDS(SY_LODES_RDS)

SY_model <- merge(SY_spatial_census, SY_LODES, by.x = 'GEOID', by.y = 'geocode')
SY_model <- SY_model %>%
  st_set_geometry(NULL)
SY_model <- SY_model %>% 
  rename_all(toupper) 
SY_model <- SY_model %>% dplyr::select(-c(MEAN_COMMUTE_TIME, CENTROID_X, CENTROID_Y),-starts_with('DENSITY'), -starts_with('COUNT'), -ends_with('LENGTH'))


# SY_model_RDS <- file.path(data_directory, "~RData/Syracuse/SY_model")
# saveRDS(SY_model,
#        file = SY_model_RDS)
# SY_model <- readRDS(SY_model_RDS)

model1 <- randomForest(ORIGINS_CNT ~ ., data = Model_clean %>% dplyr::select(-CITY, -race),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)


SY_model <- SY_model %>%
  mutate(Predicted.CNT = round(predict(model1, SY_model, type = "class"),0))

SY_result <- merge(SY_Census_geoinfo, SY_model %>% dplyr::select(GEOID, Predicted.CNT), on='GEOID')

###### race content ##########
SY_result <- merge(SY_result, as.data.frame(SY_Census) %>% dplyr::select(GEOID, pWhite), on='GEOID')
SY_result <- mutate(SY_result, race = ifelse(pWhite > .5, "Majority_White", "Majority_Non_White"))

SY_result %>%
  na.omit() %>%
  group_by(race) %>%
  summarise(mean_trip_count=mean(Predicted.CNT))

SY_result <- SY_result %>% na.omit()
mean(SY_result$Predicted.CNT)

library(viridis)
palette5 <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
ggplot() +
  geom_sf(data = SY_result %>% na.omit(), aes(fill=q5(Predicted.CNT))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(SY_result, "Predicted.CNT"),
                    name="Quintile\nBreaks") +
  labs(title = 'Prediced Trip Count in Syracuse, NY', size=18) +
  mapTheme()


# SY_result_RDS <- file.path(data_directory, "~RData/Syracuse/SY_result")
# saveRDS(SY_result,
#         file = SY_result_RDS)
SY_result <- readRDS(SY_result_RDS)

predict_SY <- ggplot()+
  geom_sf(data = SY_trimmed_result %>% na.omit(), aes(fill=Predicted.CNT)) +
  scale_fill_viridis()+
  labs(title = 'Predicted Trip Count for Syracuse, NY') +
  mapTheme()

ggsave(file.path(plot_directory,
                 "5.3 predict_SY.png"),
       plot = predict_SY,
       width = 6,
       units = "in")
