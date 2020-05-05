JV_spatial_census_RDS <- file.path(data_directory, "~RData/Jacksonville/JV_spatial_census")
JV_spatial_census <- readRDS(JV_spatial_census_RDS)
JV_LODES_RDS <- file.path(data_directory, "~RData/Jacksonville/JV_LODES")
JV_LODES <- readRDS(JV_LODES_RDS)

JV_model <- merge(JV_spatial_census, JV_LODES, by.x = 'GEOID', by.y = 'geocode')
JV_model <- JV_model %>%
  st_set_geometry(NULL)
JV_model <- JV_model %>% 
  rename_all(toupper) 
JV_model <- JV_model %>% dplyr::select(-c(MEAN_COMMUTE_TIME, CENTROID_X, CENTROID_Y),-starts_with('DENSITY'), -starts_with('COUNT'), -ends_with('LENGTH'))


# JV_model_RDS <- file.path(data_directory, "~RData/Jacksonville/JV_model")
# saveRDS(JV_model,
#        file = JV_model_RDS)
# JV_model <- readRDS(JV_model_RDS)

model1 <- randomForest(ORIGINS_CNT ~ ., data = Model_clean %>% dplyr::select(-CITY, -race),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)


JV_model <- JV_model %>%
  mutate(Predicted.CNT = round(predict(model1, JV_model, type = "class"),0))

JV_result <- merge(JV_Census_geoinfo, JV_model %>% dplyr::select(GEOID, Predicted.CNT), on='GEOID')

###### race content ##########
JV_result <- merge(JV_result, as.data.frame(MD_Census) %>% dplyr::select(GEOID, pWhite), on='GEOID')
JV_result <- mutate(JV_result, race = ifelse(pWhite > .5, "Majority_White", "Majority_Non_White"))

JV_result %>%
  na.omit() %>%
  group_by(race) %>%
  summarise(mean_trip_count=mean(Predicted.CNT))

JV_result <- JV_result %>% na.omit()
mean(JV_result$Predicted.CNT)

library(viridis)
palette5 <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
ggplot() +
  geom_sf(data = JV_result %>% na.omit(), aes(fill=q5(Predicted.CNT))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(JV_result, "Predicted.CNT"),
                    name="Quintile\nBreaks") +
  labs(title = 'Prediced Trip Count in Jacksonville, FL', size=18) +
  mapTheme()


# JV_result_RDS <- file.path(data_directory, "~RData/Jacksonville/JV_result")
# saveRDS(JV_result,
#         file = JV_result_RDS)
JV_result <- readRDS(JV_result_RDS)