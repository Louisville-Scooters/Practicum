##########################################################################
# This script:
# 1. Join CH scooter origin (0619) into census tract and calculate total number of user pick-ups for each census tract
#
# This script exports the following data:
# 1. LV_rebal_reb_only_combined_rowPairs_ct - origin-destination rows for all rebalance records in the rebalance data with the census tract info of the origin and destination
# 2. LV_rebal_user_only_combined_rowPairs_ct - origin-destination rows for all rebalance records in the rebalance data with the census tract info of the origin and destination
#
##########################################################################

ggplot() + 
  geom_sf(data = CH_open_ct, aes(fill = origins_cnt), color = "white", alpha = 0.8) +
  scale_fill_viridis()+
  labs(title = "Count of trip origin by census tract, June 2019, Minneapolis") +
  mapTheme()

ggplot() + 
  geom_sf(data = CH_scooter_clean_ori, color = "blue", alpha = 0.8) +
  geom_sf(data = CH_open_ct) +
  scale_fill_viridis()+
  labs(title = "Count of trip origin by census tract, June 2019, Minneapolis") +
  mapTheme()

#giant correlation plot
CH_ori_correlation.long <-
  st_set_geometry(CH_open_ct, NULL) %>%
  dplyr::select(-geoid10, -centroid_X, -centroid_Y, -Mean_Commute_Time, - tractce10) %>%
  gather(Variable, Value, -origins_cnt )

CH_ori_correlation.cor <-
  CH_ori_correlation.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, origins_cnt, use = "complete.obs"))

ggplot(CH_ori_correlation.long, aes(Value, origins_cnt)) +
  geom_point(size = 0.1) +
  geom_text(data = CH_ori_correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "gold") +
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  labs(title = "Origin count as a function of census factors")
