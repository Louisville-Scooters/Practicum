##########################################################################
# This script is for aggregating scooter rides (currently open data only) by census tract
# It:
# 1. Counts the number of origins and destinations for trips in each census tract
##########################################################################

### Open Data ----
# Count origins for each census tract
CH_open_origins_ct <- CH_Census_ct %>% 
  mutate(origins_cnt = (lengths(st_intersects(., CH_scooter_07to09)))/3)

# Count dests for each census tract ##not working for CH yet since not all trip dest can't be joined to street (some are trails)
#CH_open_dests_ct <- CH_Census_ct %>% 
#  mutate(dests_cnt = lengths(st_intersects(., CH_open_dests)))

CH_open_ct <- CH_open_origins_ct

# Combine
# CH_open_ct <- CH_open_origins_ct %>% 
#   left_join(CH_open_dests_ct %>% 
#               st_drop_geometry() %>%
#               dplyr::select(GEOID, dests_cnt),
#             by = "GEOID")

# plot net inflow/outflow
CH_scooter_ct <- CH_scooter_07to09 %>% na.omit()
CH_ORIGINS <- CH_scooter_ct %>%
  group_by(`Start Census Tract`) %>% 
  summarise(Outflow = n()) %>%
  na.omit()

CH_DESTS <- CH_scooter_ct %>%
  group_by(`End Census Tract`) %>% 
  summarise(Inflow = n()) %>%
  na.omit()

CH_net_inoutflow <- merge(CH_ORIGINS %>% st_set_geometry(NULL), CH_DESTS %>% st_set_geometry(NULL), all = T, by.x='Start Census Tract', by.y='End Census Tract')
CH_net_inoutflow[is.na(CH_net_inoutflow)] <- 0
CH_net_inoutflow <- CH_net_inoutflow %>%
  mutate(NetInflow = Inflow - Outflow) %>%
  merge(CH_Census_geoinfo %>% dplyr::select(GEOID, geometry), by.x='Start Census Tract', by.y='GEOID')
most_pickups <- CH_net_inoutflow$`Start Census Tract`[CH_net_inoutflow$Outflow==max(CH_net_inoutflow$Outflow)]
most_dropoffs <- CH_net_inoutflow$`Start Census Tract`[CH_net_inoutflow$Inflow==max(CH_net_inoutflow$Inflow)]

CH_net_inoutflow <- CH_net_inoutflow %>%
  mutate(NetInflowRate = (Inflow - Outflow)/Inflow)

most_pickups_ct <- subset(CH_net_inoutflow,CH_net_inoutflow$`Start Census Tract`==most_pickups)
most_dropoffs_ct <- subset(CH_net_inoutflow,CH_net_inoutflow$`Start Census Tract`==most_dropoffs)
max_inflow <- max(abs(CH_net_inoutflow$NetInflow))
max_inflowRate <- max(abs(CH_net_inoutflow$NetInflowRate))
# library(viridis)

CH_outflow_map <- ggplot()+
  geom_sf(data = st_sf(CH_net_inoutflow), aes(fill=Outflow)) +
  #geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis()+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Outflow Map for Chicago, IL') +
  mapTheme()

CH_inflow_map <- ggplot()+
  geom_sf(data = st_sf(CH_net_inoutflow), aes(fill=Inflow)) +
  #geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis()+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Inflow Map for Chicago, IL') +
  mapTheme()

CH_netinflow_map <- ggplot()+
  geom_sf(data = st_sf(CH_net_inoutflow), aes(fill=NetInflow)) +
  geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis(limits=c(-max_inflow, max_inflow))+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Net Inflow Map for Chicago',subtitle='Census tract in white frame is the census tract has most inflow/outflow') +
  mapTheme()

CH_netinflow_rate_map <- ggplot()+
  geom_sf(data = st_sf(CH_net_inoutflow), aes(fill=NetInflowRate)) +
  geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis(limits=c(-max_inflowRate, max_inflowRate))+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Net Inflow Rate Mapfor Chicago',subtitle='Census tract in white frame is the census tract has most inflow/outflow') +
  mapTheme()

ggsave(file.path(plot_directory,
                 "3.1 CH_Inflow.png"),
       plot = CH_inflow_map,
       height = 6,
       width = 6,
       units = "in")

ggsave(file.path(plot_directory,
                 "3.1 CH_Outflow.png"),
       plot = CH_outflow_map,
       height = 6,
       width = 6,
       units = "in")

ggsave(file.path(plot_directory,
                 "3.1 CH_Net_inflow.png"),
       plot = CH_netinflow_map,
       height = 6,
       width = 6,
       units = "in")

ggsave(file.path(plot_directory,
                 "3.1 CH_Net_inflow_rate.png"),
       plot = CH_netinflow_rate_map,
       height = 6,
       width = 6,
       units = "in")
