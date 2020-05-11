HS_spatial_census_RDS <- file.path(data_directory, "~RData/Houston/HS_spatial_census")
HS_spatial_census <- readRDS(HS_spatial_census_RDS)
HS_LODES_RDS <- file.path(data_directory, "~RData/Houston/HS_LODES")
HS_LODES <- readRDS(HS_LODES_RDS)

HS_model <- merge(HS_spatial_census, HS_LODES, by.x = 'GEOID', by.y = 'geocode')
HS_model <- HS_model %>%
  st_set_geometry(NULL)
HS_model <- HS_model %>% 
  rename_all(toupper) 
HS_model <- HS_model %>% dplyr::select(-c(MEAN_COMMUTE_TIME, CENTROID_X, CENTROID_Y),-starts_with('DENSITY'), -starts_with('COUNT'), -ends_with('LENGTH'))


# HS_model_RDS <- file.path(data_directory, "~RData/Houston/HS_model")
# saveRDS(HS_model,
#        file = HS_model_RDS)
# HS_model <- readRDS(HS_model_RDS)
library(randomForest)
model1 <- randomForest(ORIGINS_CNT ~ ., data = Model_clean %>% dplyr::select(-CITY, -race),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)


HS_model <- HS_model %>%
  mutate(Predicted.CNT = round(predict(model1, HS_model, type = "class"),0))

HS_result <- merge(HS_Census_geoinfo, HS_model %>% dplyr::select(GEOID, Predicted.CNT), on='GEOID')

###### race content ##########
HS_result <- merge(HS_result, as.data.frame(MD_Census) %>% dplyr::select(GEOID, pWhite), on='GEOID')
HS_result <- mutate(HS_result, race = ifelse(pWhite > .5, "Majority_White", "Majority_Non_White"))

HS_result %>%
  na.omit() %>%
  group_by(race) %>%
  summarise(mean_trip_count=mean(Predicted.CNT))

HS_result <- HS_result %>% na.omit()
mean(HS_result$Predicted.CNT)

library(viridis)
palette5 <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
ggplot() +
  geom_sf(data = HS_result %>% na.omit(), aes(fill=q5(Predicted.CNT))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(HS_result, "Predicted.CNT"),
                    name="Quintile\nBreaks") +
  labs(title = 'Prediced Trip Count in Houston, TX', size=18) +
  mapTheme()


HS_result_RDS <- file.path(data_directory, "~RData/Houston/HS_result")
saveRDS(HS_result,
        file = HS_result_RDS)
HS_result <- readRDS(HS_result_RDS)

predict_HS <- ggplot()+
  geom_sf(data = HS_trimmed_result %>% na.omit(), aes(fill=Predicted.CNT)) +
  scale_fill_viridis()+
  labs(title = 'Predicted Trip Count for Houston, TX') +
  mapTheme()

ggsave(file.path(plot_directory,
                 "5.3 predict_HS.png"),
       plot = predict_HS,
       width = 6,
       units = "in")
