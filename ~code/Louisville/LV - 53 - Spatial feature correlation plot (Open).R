##########################################################################
# This script contains our correlation plots:
# 1. Spatial features VS count of origins

# University (KNN, 1)
# Restaurant (density)
# Retail (KNN, 5)
# Office (KNN, 5)
# Tourism (density)
# Leisure (density)
# Cycleway (density)
# Public_transport station (KNN, 5)
##########################################################################

glimpse(LV_spatial_census)

LV_ori_spatial_correlation.long <-
  st_set_geometry(LV_spatial_census, NULL) %>%
  dplyr::select(origins_cnt, KNN_university, restaurant_density, KNN_retail, KNN_office, tourism_density,
                leisure_density, total_length, KNN_public_transport) %>%
  gather(Variable, Value, -origins_cnt )

LV_ori_spatial_correlation.cor <-
  LV_ori_spatial_correlation.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, origins_cnt, use = "complete.obs"))

ggplot(LV_ori_spatial_correlation.long, aes(Value, origins_cnt)) +
  geom_point(size = 0.1) +
  geom_text(data = LV_ori_spatial_correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "gold") +
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  labs(title = "Origin count as a function of spatial factors")
