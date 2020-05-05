JC_spatial_census_RDS <- file.path(data_directory, "~RData/Jersey City/JC_spatial_census")
JC_spatial_census <- readRDS(JC_spatial_census_RDS)
JC_LODES_RDS <- file.path(data_directory, "~RData/Jersey City/JC_LODES")
JC_LODES <- readRDS(JC_LODES_RDS)
JC_model <- merge(JC_spatial_census, JC_LODES, by.x = 'GEOID', by.y = 'geocode')
JC_model <- JC_model %>%
  st_set_geometry(NULL)
JC_model <- JC_model %>% 
  rename_all(toupper) 
JC_model <- JC_model %>% dplyr::select(-c(MEAN_COMMUTE_TIME, CENTROID_X, CENTROID_Y),-starts_with('DENSITY'), -starts_with('COUNT'), -ends_with('LENGTH'))


# JC_model_RDS <- file.path(data_directory, "~RData/Kansas City/JC_model")
# saveRDS(JC_model,
#        file = JC_model_RDS)
# JC_model <- readRDS(JC_model_RDS)

model1 <- randomForest(ORIGINS_CNT ~ ., data = Model_clean %>% dplyr::select(-CITY, -race),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)


JC_model <- JC_model %>%
  mutate(Predicted.CNT = round(predict(model1, JC_model, type = "class"),0))


JC_result <- merge(JC_Census_geoinfo, JC_model %>% dplyr::select(GEOID, Predicted.CNT), on='GEOID')

###### race content ##########
JC_result <- merge(JC_result, as.data.frame(MD_Census) %>% dplyr::select(GEOID, pWhite), on='GEOID')
JC_result <- mutate(JC_result, race = ifelse(pWhite > .5, "Majority_White", "Majority_Non_White"))

JC_result %>%
  na.omit() %>%
  group_by(race) %>%
  summarise(mean_trip_count=mean(Predicted.CNT))

JC_result <- JC_result %>% na.omit()
mean(JC_result$Predicted.CNT)

library(viridis)
palette5 <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
ggplot() +
  geom_sf(data = JC_result %>% na.omit(), aes(fill=q5(Predicted.CNT))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(JC_result, "Predicted.CNT"),
                    name="Quintile\nBreaks") +
  labs(title = 'Prediced Trip Count in Philadelphia, PA', size=18) +
  mapTheme()


JC_result_RDS <- file.path(data_directory, "~RData/Jersey City/JC_result")
saveRDS(JC_result,
        file = JC_result_RDS)
JC_result <- readRDS(JC_result_RDS)