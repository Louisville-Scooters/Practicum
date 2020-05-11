##########################################################################
# This script:
# 1. Add location field for DC trips data
#
# This script exports the following data:
# 1. DC_scooter_ct
#
##########################################################################

### users events####
# Read the structured rebalance data - users events
DC_scooter_sf <- st_as_sf(DC_scooter_data %>% dplyr::select(-start_date, -end_date), coords = c('start_lon','start_lat'),crs=4326) %>%
  st_transform(DC_proj) %>%
  mutate(start_longitude = unlist(map(geometry, 1)),
         start_latitude = unlist(map(geometry, 2)))
DC_scooter_ct <- st_join(DC_scooter_sf %>% st_transform(2246), DC_Census_geoinfo %>% dplyr::select(GEOID), st_within, left=T) %>%
  rename(Start.Census.Tract=GEOID)

DC_scooter_ct <- st_as_sf(as.data.frame(DC_scooter_ct) %>% dplyr::select(-geometry) %>% na.omit(), coords = c('end_lon','end_lat'),crs=4326) %>%
  st_transform(DC_proj) %>%
  mutate(end_longitude = unlist(map(geometry, 1)),
         end_longitude = unlist(map(geometry, 2)))


DC_scooter_ct <- st_join(DC_scooter_ct %>% st_transform(2246), DC_Census_geoinfo %>% dplyr::select(GEOID), st_within, left=T) %>%
  rename(End.Census.Tract=GEOID)

DC_scooter_ct_RDS <- file.path(data_directory, 
                               "~RData/DC/DC_scooter_ct")
DC_scooter_ct <- readRDS(DC_scooter_ct_RDS)
# saveRDS(DC_scooter_ct,
#         file = DC_scooter_ct_RDS)

# plot net inflow/outflow

DC_ORIGINS <- DC_scooter_ct %>%
  group_by(Start.Census.Tract) %>% 
  summarise(Outflow = n()) %>%
  na.omit()

DC_DESTS <- DC_scooter_ct %>%
  group_by(End.Census.Tract) %>% 
  summarise(Inflow = n()) %>%
  na.omit()

DC_net_inoutflow <- merge(DC_ORIGINS %>% st_set_geometry(NULL), DC_DESTS %>% st_set_geometry(NULL), all = T, by.x='Start.Census.Tract', by.y='End.Census.Tract')
DC_net_inoutflow[is.na(DC_net_inoutflow)] <- 0
DC_net_inoutflow <- DC_net_inoutflow %>%
  mutate(NetInflow = Inflow - Outflow) %>%
  merge(DC_Census_geoinfo %>% dplyr::select(GEOID, geometry), by.x='Start.Census.Tract', by.y='GEOID')
most_pickups <- DC_net_inoutflow$Start.Census.Tract[DC_net_inoutflow$Outflow==max(DC_net_inoutflow$Outflow)]
most_dropoffs <- DC_net_inoutflow$Start.Census.Tract[DC_net_inoutflow$Inflow==max(DC_net_inoutflow$Inflow)]

DC_net_inoutflow <- DC_net_inoutflow %>%
  mutate(NetInflowRate = (Inflow - Outflow)/Inflow)

most_pickups_ct <- subset(DC_net_inoutflow,DC_net_inoutflow$Start.Census.Tract==most_pickups)
most_dropoffs_ct <- subset(DC_net_inoutflow,DC_net_inoutflow$Start.Census.Tract==most_dropoffs)
max_inflow <- max(abs(DC_net_inoutflow$NetInflow))
max_inflowRate <- max(abs(DC_net_inoutflow$NetInflowRate))
library(viridis)

DC_net_inoutflow <- rename(DC_net_inoutflow, Inflow=Inflow)

DC_outflow_map <- ggplot()+
  geom_sf(data = st_sf(DC_net_inoutflow), aes(fill=Outflow)) +
  #geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis()+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Outflow Map for Washington D.C') +
  mapTheme()

DC_inflow_map <- ggplot()+
  geom_sf(data = st_sf(DC_net_inoutflow), aes(fill=Inflow)) +
  #geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis()+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Inflow Map for Washington D.C') +
  mapTheme()

DC_netinflow_map <- ggplot()+
  geom_sf(data = st_sf(DC_net_inoutflow), aes(fill=NetInflow)) +
  geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis(limits=c(-max_inflow, max_inflow))+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Net Inflow Map for Washington D.C',subtitle='Census tract in white frame is the census tract has most inflow/outflow') +
  mapTheme()

DC_netinflow_rate_map <-  ggplot()+
  geom_sf(data = st_sf(DC_net_inoutflow), aes(fill=NetInflowRate)) +
  geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis(limits=c(-max_inflowRate, max_inflowRate))+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Net Inflow Rate Mapfor Washington D.C',subtitle='Census tract in white frame is the census tract has most inflow/outflow') +
  mapTheme()

ggsave(file.path(plot_directory,
                 "3.1 DC_Inflow.png"),
       plot = DC_inflow_map,
       height = 6,
       width = 6,
       units = "in")

ggsave(file.path(plot_directory,
                 "3.1 DC_Outflow.png"),
       plot = DC_outflow_map,
       height = 6,
       width = 6,
       units = "in")

ggsave(file.path(plot_directory,
                 "3.1 DC_Net_inflow.png"),
       plot = DC_netinflow_map,
       height = 6,
       width = 6,
       units = "in")

ggsave(file.path(plot_directory,
                 "3.1 DC_Net_inflow_rate.png"),
       plot = DC_netinflow_rate_map,
       height = 6,
       width = 6,
       units = "in")
