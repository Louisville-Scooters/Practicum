##########################################################################
# This script is for setting up data files for ML model building. 
# It reads in:
# 1. Model_panel

# This script exports the following data:
# 1. 
##########################################################################
#install.packages('ggcorrplot')
library(ggcorrplot)

Model_clean_RDS <- file.path(data_directory, "~RData/Model_clean")
#saveRDS(Model_clean,
#        file = Model_clean_RDS)
Model_clean_corr <- readRDS(Model_clean_RDS) #this one has all 48 variables

ggcorrplot(cor(Model_clean_corr, use = "pairwise.complete.obs"),
           colors = c("#9F71C7","white", "#D16BA5"), 
           title="Correlation Matrix (Correlogram) of all variables") + 
  theme(axis.text.x = element_text(size=10, angle= 90, vjust=1, hjust=1, 
                                   margin=margin(-3,0,0,0)),
        axis.text.y = element_text(size=10, margin=margin(0,-3,0,0)),
        panel.grid.minor = element_line(size=10))

### correlation plots between census variables & ORIGINs_CNT ####
Model_corr_census <- Model_clean_corr %>%
  dplyr::select(-starts_with('DENSITY'), -starts_with('KNN'), -starts_with('COUNT'),-starts_with('RATIO'), 
                -ends_with('LENGTH'), -JOBS_IN_TRACT, -WORKERS_IN_TRACT)

# scatter plots
Model_corr_census.long <- Model_corr_census %>%
  gather(Variable, Value, -ORIGINS_CNT )

Model_corr_census.cor <-
  Model_corr_census.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, ORIGINS_CNT, use = "complete.obs"))

ggplot(Model_corr_census.long, aes(Value, ORIGINS_CNT)) +
  geom_point(size = 0.1) +
  geom_text(data = Model_corr_census.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "gold") +
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  labs(title = "Origin count as a function of census factors")


### correlation plots between OSM variables (COUNT) & ORIGINs_CNT ####
Model_corr_osm_cnt <- Model_clean_corr %>%
  dplyr::select(starts_with('COUNT'), ends_with('LENGTH'), ORIGINS_CNT)

# scatter plots
Model_corr_osm_cnt.long <- Model_corr_osm_cnt %>%
  gather(Variable, Value, -ORIGINS_CNT )

Model_corr_osm_cnt.cor <-
  Model_corr_osm_cnt.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, ORIGINS_CNT, use = "complete.obs"))

ggplot(Model_corr_osm_cnt.long, aes(Value, ORIGINS_CNT)) +
  geom_point(size = 0.1) +
  geom_text(data = Model_corr_osm_cnt.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "#FFAF6D") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  labs(title = "Origin count as a function of COUNT of OSM features")

### correlation plots between OSM variables (KNN) & ORIGINs_CNT ####
Model_corr_osm_knn <- Model_clean_corr %>%
  dplyr::select(starts_with('KNN'), ORIGINS_CNT)

# scatter plots
Model_corr_osm_knn.long <- Model_corr_osm_knn %>%
  gather(Variable, Value, -ORIGINS_CNT )

Model_corr_osm_knn.cor <-
  Model_corr_osm_knn.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, ORIGINS_CNT, use = "complete.obs"))

ggplot(Model_corr_osm_knn.long, aes(Value, ORIGINS_CNT)) +
  geom_point(size = 0.1) +
  geom_text(data = Model_corr_osm_knn.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "#FFAF6D") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  labs(title = "Origin count as a function of K Nearest OSM features")

### correlation plots between OSM variables (RATIO) & ORIGINs_CNT ####
#density: all zero
Model_corr_osm_ratio <- Model_clean_corr %>%
  dplyr::select(starts_with('RATIO'), ORIGINS_CNT)

# scatter plots
Model_corr_osm_ratio.long <- Model_corr_osm_ratio %>%
  gather(Variable, Value, -ORIGINS_CNT )

Model_corr_osm_ratio.cor <-
  Model_corr_osm_ratio.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, ORIGINS_CNT, use = "complete.obs"))

ggplot(Model_corr_osm_ratio.long, aes(Value, ORIGINS_CNT)) +
  geom_point(size = 0.1) +
  geom_text(data = Model_corr_osm_ratio.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "#FFAF6D") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  labs(title = "Origin count as a function of ratio OSM features",
       subtitle = "Ratio: number of OSM features in each tract/total number of features across the city")
