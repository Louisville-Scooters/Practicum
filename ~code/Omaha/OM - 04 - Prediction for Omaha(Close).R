OM_spatial_census_RDS <- file.path(data_directory, "~RData/Omaha/OM_spatial_census")
OM_spatial_census <- readRDS(OM_spatial_census_RDS)
OM_LODES_RDS <- file.path(data_directory, "~RData/Omaha/OM_LODES")
OM_LODES <- readRDS(OM_LODES_RDS)

OM_model <- merge(OM_spatial_census, OM_LODES, by.x = 'GEOID', by.y = 'geocode')
OM_model <- OM_model %>%
  st_set_geometry(NULL)
OM_model <- OM_model %>% 
  rename_all(toupper) 
OM_model <- OM_model %>% dplyr::select(-c(MEAN_COMMUTE_TIME, CENTROID_X, CENTROID_Y),-starts_with('DENSITY'), -starts_with('COUNT'), -ends_with('LENGTH'))


# OM_model_RDS <- file.path(data_directory, "~RData/Omaha/OM_model")
# saveRDS(OM_model,
#        file = OM_model_RDS)
# OM_model <- readRDS(OM_model_RDS)

model1 <- randomForest(ORIGINS_CNT ~ ., data = Model_clean %>% dplyr::select(-CITY, -race),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)


OM_model <- OM_model %>%
  mutate(Predicted.CNT = round(predict(model1, OM_model, type = "class"),0))

OM_result <- merge(OM_Census_geoinfo, OM_model %>% dplyr::select(GEOID, Predicted.CNT), on='GEOID')

###### race content ##########
OM_result <- merge(OM_result, as.data.frame(OM_Census) %>% dplyr::select(GEOID, pWhite), on='GEOID')
OM_result <- mutate(OM_result, race = ifelse(pWhite > .5, "Majority_White", "Majority_Non_White"))

OM_result %>%
  na.omit() %>%
  group_by(race) %>%
  summarise(mean_trip_count=mean(Predicted.CNT))

OM_result <- OM_result %>% na.omit()
mean(OM_result$Predicted.CNT)

library(viridis)
palette5 <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
ggplot() +
  geom_sf(data = OM_result %>% na.omit(), aes(fill=q5(Predicted.CNT))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(OM_result, "Predicted.CNT"),
                    name="Quintile\nBreaks") +
  labs(title = 'Prediced Trip Count in Omaha, FL', size=18) +
  mapTheme()


OM_result_RDS <- file.path(data_directory, "~RData/Omaha/OM_result")
# saveRDS(OM_result,
#         file = OM_result_RDS)
OM_result <- readRDS(OM_result_RDS)

predict_OM <- ggplot()+
  geom_sf(data = OM_trimmed_result %>% na.omit(), aes(fill=q5(Predicted.CNT))) +
  scale_fill_viridis_d(labels=qBr(OM_trimmed_result,"Predicted.CNT"),name="Quintile\nBreaks") +
  labs(title = 'Predicted Trip Count for Omaha, NE') +
  mapTheme()

ggsave(file.path(plot_directory,
                 "5.3 predict_OM.png"),
       plot = predict_OM,
       width = 6,
       units = "in")

Dist_OM <- ggplot()+
  geom_histogram(data = as.data.frame(OM_trimmed_result) %>% na.omit(), aes(round(Predicted.CNT)), fill='#453781FF', bins=30) +
  labs(title='Distribution of Trip Count in Omaha', x='Predicted trip count') +
  plotTheme

ggsave(file.path(plot_directory,
                 "5.3 Dist_OM.png"),
       plot = Dist_OM,
       width = 6,
       units = "in")
