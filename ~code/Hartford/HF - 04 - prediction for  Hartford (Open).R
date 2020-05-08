HF_spatial_census_RDS <- file.path(data_directory, "~RData/Hartford/HF_spatial_census")
HF_spatial_census <- readRDS(HF_spatial_census_RDS)
HF_LODES_RDS <- file.path(data_directory, "~RData/Hartford/HF_LODES")
HF_LODES <- readRDS(HF_LODES_RDS)

HF_model <- merge(HF_spatial_census, HF_LODES, by.x = 'GEOID', by.y = 'geocode')
HF_model <- HF_model %>%
  st_set_geometry(NULL)
HF_model <- HF_model %>% 
  rename_all(toupper) 
HF_model <- HF_model %>% dplyr::select(-c(MEAN_COMMUTE_TIME, CENTROID_X, CENTROID_Y),-starts_with('DENSITY'), -starts_with('COUNT'), -ends_with('LENGTH'))


# HF_model_RDS <- file.path(data_directory, "~RData/Hartford/HF_model")
# saveRDS(HF_model,
#        file = HF_model_RDS)
# HF_model <- readRDS(HF_model_RDS)
library(randomForest)
model1 <- randomForest(ORIGINS_CNT ~ ., data = Model_clean %>% dplyr::select(-CITY, -race),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)


HF_model <- HF_model %>%
  mutate(Predicted.CNT = round(predict(model1, HF_model, type = "class"),0))

HF_result <- merge(HF_Census_geoinfo, HF_model %>% dplyr::select(GEOID, Predicted.CNT), on='GEOID')

###### race content ##########
HF_result <- merge(HF_result, as.data.frame(MD_Census) %>% dplyr::select(GEOID, pWhite), on='GEOID')
HF_result <- mutate(HF_result, race = ifelse(pWhite > .5, "Majority_White", "Majority_Non_White"))

HF_result %>%
  na.omit() %>%
  group_by(race) %>%
  summarise(mean_trip_count=mean(Predicted.CNT))

HF_result <- HF_result %>% na.omit()
mean(HF_result$Predicted.CNT)

library(viridis)
palette5 <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
ggplot() +
  geom_sf(data = HF_result %>% na.omit(), aes(fill=q5(Predicted.CNT))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(HF_result, "Predicted.CNT"),
                    name="Quintile\nBreaks") +
  labs(title = 'Prediced Trip Count in Hartford, CT', size=18) +
  mapTheme()


HF_result_RDS <- file.path(data_directory, "~RData/Hartford/HF_result")
# saveRDS(HF_result,
#         file = HF_result_RDS)
HF_result <- readRDS(HF_result_RDS)
