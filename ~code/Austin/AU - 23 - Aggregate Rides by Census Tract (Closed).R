##########################################################################
# This script is for aggregating scooter rides (currently open data only) by census tract
# It:
# 1. Counts the number of origins and destinations for trips in each census tract
##########################################################################

### Open Data ----
# Count origins for each census tract
AU_cnt_origins <- AU_scooter %>%
  filter(month %in% c(7,8,9),
         year==2019)

AU_scooter_RDS <- file.path(data_directory, 
                               "~RData/Austin/AU_scooter")
AU_scooter <- readRDS(AU_scooter_RDS)
AU_open_origins_ct <- AU_Census_ct %>% 
  mutate(origins_cnt = lengths(st_intersects(., LV_open_origins)))

# Count dests for each census tract
LV_open_dests_ct <- LV_Census_ct %>% 
  mutate(dests_cnt = lengths(st_intersects(., LV_open_dests)))

# Combine
LV_open_ct <- LV_open_origins_ct %>% 
  left_join(LV_open_dests_ct %>% 
              st_drop_geometry() %>%
              dplyr::select(GEOID, dests_cnt),
            by = "GEOID")


AU_ORIGINS <- AU_cnt_origins %>%
  group_by(census_tract_start) %>% 
  summarise(Outflow = n()) %>%
  na.omit()

AU_DESTS <- AU_cnt_origins %>%
  group_by(census_tract_end) %>% 
  summarise(Inflow = n()) %>%
  na.omit()

AU_net_inoutflow <- merge(AU_ORIGINS, AU_DESTS, all = T, by.x='census_tract_start', by.y='census_tract_end')
AU_net_inoutflow[is.na(AU_net_inoutflow)] <- 0
AU_net_inoutflow <- AU_net_inoutflow %>%
  mutate(NetInflow = Inflow - Outflow) %>%
  merge(AU_Census_geoinfo %>% dplyr::select(GEOID, geometry), by.x='census_tract_start', by.y='GEOID')
most_pickups <- AU_net_inoutflow$census_tract_start[AU_net_inoutflow$Outflow==max(AU_net_inoutflow$Outflow)]
most_dropoffs <- AU_net_inoutflow$census_tract_start[AU_net_inoutflow$Inflow==max(AU_net_inoutflow$Inflow)]

AU_net_inoutflow <- AU_net_inoutflow %>%
  mutate(NetInflowRate = (Inflow - Outflow)/Inflow)
AU_net_inoutflow$NetInflowRate[is.infinite(AU_net_inoutflow$NetInflowRate)] <- 0

most_pickups_ct <- subset(AU_net_inoutflow,AU_net_inoutflow$census_tract_start==most_pickups) %>%
  merge(AU_Census_geoinfo %>% dplyr::select(GEOID, geometry), by.x='census_tract_start', by.y='GEOID')
most_dropoffs_ct <- subset(AU_net_inoutflow,AU_net_inoutflow$census_tract_start==most_dropoffs) %>%
  merge(AU_Census_geoinfo %>% dplyr::select(GEOID, geometry), by.x='census_tract_start', by.y='GEOID')
max_inflow <- max(abs(AU_net_inoutflow$NetInflow))
max_inflowRate <- max(abs(AU_net_inoutflow$NetInflowRate))
# library(viridis)

ggplot()+
  geom_sf(data = st_sf(AU_net_inoutflow), aes(fill=Outflow)) +
  #geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis()+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Outflow Map for Austin, TX') +
  mapTheme()

ggplot()+
  geom_sf(data = st_sf(AU_net_inoutflow), aes(fill=Inflow)) +
  #geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis()+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Inflow Map for Austin, TX') +
  mapTheme()

ggplot()+
  geom_sf(data = st_sf(AU_net_inoutflow), aes(fill=NetInflow)) +
  geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis(limits=c(-max_inflow, max_inflow))+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Net Inflow Map for Austin',subtitle='Census tract in white frame is the census tract has most inflow/outflow') +
  mapTheme()

ggplot()+
  geom_sf(data = st_sf(AU_net_inoutflow), aes(fill=NetInflowRate)) +
  geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis(limits=c(-max_inflowRate, max_inflowRate))+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Net Inflow Rate Map for Austin',subtitle='Census tract in white frame is the census tract has most inflow/outflow') +
  mapTheme()

