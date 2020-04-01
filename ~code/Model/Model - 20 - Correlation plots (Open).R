##########################################################################
# This script is for setting up data files for ML model building. 
# It reads in:
# 1. Model_panel

# This script exports the following data:
# 1. 
##########################################################################
install.packages('ggcorrplot')
library(ggcorrplot)

Model_clean_RDS <- file.path(data_directory, "~RData/Model_clean")
#saveRDS(Model_clean,
#        file = Model_clean_RDS)
Model_clean_corr <- readRDS(Model_clean_RDS) #this one has all 48 variables

# correlation plots between census variables & ORIGINs_CNT
Model_corr_census <- Model_clean_corr %>%
  dplyr::select(-starts_with('DENSITY'), -starts_with('KNN'), -starts_with('COUNT'),-starts_with('RATIO'), 
                -ends_with('LENGTH'), -JOBS_IN_TRACT, -WORKERS_IN_TRACT)


ggcorrplot(cor(Model_corr_census, use = "pairwise.complete.obs"),
           colors = c("#9F71C7","white", "#D16BA5"), 
           title="Correlation Matrix (Correlogram) of Census Variables",
           ggtheme=mapTheme)

# Ophelia's Feb 10 update
glimpse(MNP_open_ct)
MNP_ori_correlation.long <-
  st_set_geometry(MNP_open_ct, NULL) %>%
  dplyr::select(-GEOID, -centroid_X, -centroid_Y, -dests_cnt, -Mean_Commute_Time) %>%
  gather(Variable, Value, -origins_cnt )

MNP_ori_correlation.cor <-
  MNP_ori_correlation.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, origins_cnt, use = "complete.obs"))

ggplot(MNP_ori_correlation.long, aes(Value, origins_cnt)) +
  geom_point(size = 0.1) +
  geom_text(data = MNP_ori_correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "gold") +
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  labs(title = "Origin count as a function of census factors")