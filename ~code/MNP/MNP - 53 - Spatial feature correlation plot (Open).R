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

glimpse(MNP_spatial_census)

MNP_ori_spatial_correlation.long <-
  st_set_geometry(MNP_spatial_census, NULL) %>%
  dplyr::select(origins_cnt, KNN_college, KNN_restaurant, KNN_public_transport, 
                KNN_retail, KNN_office, KNN_tourism, KNN_leisure, count_retail, 
                density_retail, count_office, density_office, count_leisure,
                density_leisure, count_tourism, density_tourism,count_pubtran,
                density_pubtran, count_restaurant, density_restaurant,
                count_college, density_college, total_length) %>%
  gather(Variable, Value, -origins_cnt )

MNP_ori_spatial_correlation.cor <-
  MNP_ori_spatial_correlation.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, origins_cnt, use = "complete.obs"))

ggplot(MNP_ori_spatial_correlation.long, aes(Value, origins_cnt)) +
  geom_point(size = 0.1) +
  geom_text(data = MNP_ori_spatial_correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "gold") +
  facet_wrap(~Variable, ncol = 5, scales = "free") +
  labs(title = "Origin count as a function of spatial factors")
