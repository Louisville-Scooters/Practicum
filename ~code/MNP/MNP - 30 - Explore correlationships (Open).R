##########################################################################
# This script:
# 1. Join MNP scooter origin (0619) into census tract and calculate total number of user pick-ups for each census tract
#
# This script exports the following data:
# 1. LV_rebal_reb_only_combined_rowPairs_ct - origin-destination rows for all rebalance records in the rebalance data with the census tract info of the origin and destination
# 2. LV_rebal_user_only_combined_rowPairs_ct - origin-destination rows for all rebalance records in the rebalance data with the census tract info of the origin and destination
#
##########################################################################

MNP_scooter_0619_addcensus <- st_join(MNP_scooter_0619_sf, MNP_Census_ct)

MNP_origin_by_tract_0619 <- data.frame(table(MNP_scooter_0619_addcensus$GEOID))
names(MNP_origin_by_tract_0619) <-c("GEOID", "Count_origin")

MNP_origin_by_tract_0619 <- merge(MNP_Census_ct, MNP_origin_by_tract_0619, by = 'GEOID')

ggplot() + 
  geom_sf(data = MNP_origin_by_tract_0619, aes(fill = Count_origin), color = "white", alpha = 0.8) +
  scale_fill_viridis()+
  labs(title = "Count of trip origin by census tract, June 2019, Minneapolis") +
  mapTheme()
