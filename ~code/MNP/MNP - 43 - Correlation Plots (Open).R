##########################################################################
# This script contains our correlation plots:
# 1. Open Data origins and destinations and census variables
##########################################################################

# Open Data Origins and Destinations and Census Variables
MNP_corr_mat <- cor(MNP_open_ct %>% 
                     st_drop_geometry() %>% 
                     na.omit() %>% 
                     dplyr::select(-centroid_X, -centroid_Y) %>% 
                     dplyr::select_if(is.numeric))

MNP_open_ct_corrplot <- corrplot(MNP_corr_mat,
                                method = "color")

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
