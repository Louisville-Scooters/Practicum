##########################################################################
# This script:
# 1. Add location field for KC trips data
#
# This script exports the following data:
# 1. KC_scooter_ct
#
##########################################################################

### users events####
# Read the structured rebalance data - users events
KC_scooter_sf <- st_as_sf(KC_scooter %>% na.omit(), coords = c('start_longitude','start_latitude'),crs=4326) %>%
  mutate(start_longitude = unlist(map(geometry, 1)),
         start_latitude = unlist(map(geometry, 2))) %>%
  st_transform(KC_proj)
KC_scooter_ct <- st_join(KC_scooter_sf, KC_Census_geoinfo %>% dplyr::select(GEOID), st_within, left=T) %>%
  rename(Start.Census.Tract=GEOID)

KC_scooter_ct <- st_as_sf(as.data.frame(KC_scooter_ct) %>% dplyr::select(-geometry), coords = c('end_longitude','end_latitude'),crs=4326) %>%
  st_transform(KC_proj) %>%
  mutate(end_longitude = unlist(map(geometry, 1)),
         end_latitude = unlist(map(geometry, 2))) 


KC_scooter_ct <- st_join(KC_scooter_ct, KC_Census_geoinfo %>% dplyr::select(GEOID), st_within, left=T) %>%
  rename(End.Census.Tract=GEOID)

KC_scooter_ct_RDS <- file.path(data_directory, 
                               "~RData/Kansas City/KC_scooter_ct")
KC_scooter_ct <- readRDS(KC_scooter_ct_RDS)
# saveRDS(KC_scooter_ct,
#         file = KC_scooter_ct_RDS)

KC_scooter_07to09 <- KC_scooter_ct %>%
  filter(year(start_time) == 2019, month(start_time) > 6 & month(start_time) < 10)


# plot net inflow/outflow

KC_ORIGINS <- KC_scooter_07to09 %>%
                    group_by(Start.Census.Tract) %>% 
                    summarise(Outflow = n()) %>%
  na.omit()

KC_DESTS <- KC_scooter_07to09 %>%
  group_by(End.Census.Tract) %>% 
  summarise(Inflow = n()) %>%
  na.omit()

KC_net_inoutflow <- merge(KC_ORIGINS %>% st_set_geometry(NULL), KC_DESTS %>% st_set_geometry(NULL), all = T, by.x='Start.Census.Tract', by.y='End.Census.Tract')
KC_net_inoutflow[is.na(KC_net_inoutflow)] <- 0
KC_net_inoutflow <- KC_net_inoutflow %>%
  mutate(NetInflow = Inflow - Outflow) %>%
  merge(KC_Census_geoinfo %>% dplyr::select(GEOID, geometry), by.x='Start.Census.Tract', by.y='GEOID')
most_pickups <- KC_net_inoutflow$Start.Census.Tract[KC_net_inoutflow$Outflow==max(KC_net_inoutflow$Outflow)]
most_dropoffs <- KC_net_inoutflow$Start.Census.Tract[KC_net_inoutflow$Inflow==max(KC_net_inoutflow$Inflow)]

KC_net_inoutflow <- KC_net_inoutflow %>%
  mutate(NetInflowRate = (Inflow - Outflow)/Inflow)
  
most_pickups_ct <- subset(KC_net_inoutflow,KC_net_inoutflow$Start.Census.Tract==most_pickups)
most_dropoffs_ct <- subset(KC_net_inoutflow,KC_net_inoutflow$Start.Census.Tract==most_dropoffs)
max_inflow <- max(abs(KC_net_inoutflow$NetInflow))
max_inflowRate <- max(abs(KC_net_inoutflow$NetInflowRate))
library(viridis)

KC_outflow_map <- ggplot()+
  geom_sf(data = st_sf(KC_net_inoutflow), aes(fill=Outflow)) +
  #geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis()+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Outflow Map for Kansas City, MO') +
  mapTheme()

KC_inflow_map <- ggplot()+
  geom_sf(data = st_sf(KC_net_inoutflow), aes(fill=Inflow)) +
  #geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis()+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Inflow Map for Kansas City, MO') +
  mapTheme()

KC_netinflow_map <- ggplot()+
  geom_sf(data = st_sf(KC_net_inoutflow), aes(fill=NetInflow)) +
  geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis(limits=c(-max_inflow, max_inflow))+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Net Inflow Map for Kansas City',subtitle='Census tract in white frame is the census tract has most inflow/outflow') +
  mapTheme()

KC_netinflow_rate_map <- ggplot()+
  geom_sf(data = st_sf(KC_net_inoutflow), aes(fill=NetInflowRate)) +
  geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis(limits=c(-max_inflowRate, max_inflowRate))+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Net Inflow Rate Mapfor Kansas City',subtitle='Census tract in white frame is the census tract has most inflow/outflow') +
  mapTheme()

ggsave(file.path(plot_directory,
                 "3.1 KC_Inflow.png"),
       plot = KC_inflow_map,
       height = 6,
       width = 6,
       units = "in")

ggsave(file.path(plot_directory,
                 "3.1 KC_Outflow.png"),
       plot = KC_outflow_map,
       height = 6,
       width = 6,
       units = "in")

ggsave(file.path(plot_directory,
                 "3.1 KC_Net_inflow.png"),
       plot = KC_netinflow_map,
       height = 6,
       width = 6,
       units = "in")

ggsave(file.path(plot_directory,
                 "3.1 KC_Net_inflow_rate.png"),
       plot = KC_netinflow_rate_map,
       height = 6,
       width = 6,
       units = "in")

