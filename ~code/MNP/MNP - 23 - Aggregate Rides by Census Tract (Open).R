##########################################################################
# This script is for aggregating scooter rides (currently open data only) by census tract
# It:
# 1. Counts the number of origins and destinations for trips in each census tract
##########################################################################

### Open Data ----
# Count origins for each census tract
MNP_open_origins_ct <- MNP_Census_ct %>% 
  mutate(origins_cnt = (lengths(st_intersects(., MNP_scooter_07to09_ori %>% st_transform(2246)))))

# Count dests for each census tract ##not working for MNP yet since not all trip dest can't be joined to street (some are trails)
# MNP_open_dests_ct <- MNP_Census_ct %>% 
#  mutate(dests_cnt = lengths(st_intersects(., MNP_open_dests)))

MNP_open_ct <- MNP_open_origins_ct

# Combine
# MNP_open_ct <- MNP_open_origins_ct %>% 
#   left_join(MNP_open_dests_ct %>% 
#               st_drop_geometry() %>%
#               dplyr::select(GEOID, dests_cnt),
#             by = "GEOID")

MNP_net_inoutflow <- rename(MNP_open_origins_ct, Outflow=origins_cnt)


ggplot()+
  geom_sf(data = st_sf(MNP_net_inoutflow), aes(fill=Outflow)) +
  #geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis()+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Outflow Map for Minneapolis, MN') +
  mapTheme()
