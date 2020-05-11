##########################################################################
# This script is for aggregating scooter rides (currently open data only) by census tract
# It:
# 1. Counts the number of origins and destinations for trips in each census tract
##########################################################################

plot_directory <- "C:/Users/sumyeem/Box/Practicum - Louisville Scooters/Eugene/Eugene - Practicum/Markdown_plots"

### Open Data ----
# Count origins for each census tract

LV_open_origins_RDS <- file.path(data_directory,
                                 "~RData/Louisville/LV_open_origins")

LV_open_origins <- readRDS(LV_open_origins_RDS)


LV_open_07to09_sf <- LV_open_origins %>%
  filter(month(StartDate) >= '2019-07-01' & StartDate < '2019-10-01')

LV_open_origins_ct <- LV_Census_ct %>% 
  mutate(origins_cnt = (lengths(st_intersects(., LV_open_07to09_sf))))

# Count dests for each census tract
LV_open_dests_ct <- LV_Census_ct %>%
 mutate(dests_cnt = lengths(st_intersects(., LV_open_dests)))

# Combine
LV_open_ct <- LV_open_origins_ct %>%
  left_join(LV_open_dests_ct %>%
              st_drop_geometry() %>%
              dplyr::select(GEOID, dests_cnt),
            by = "GEOID")

LV_open_ct <- LV_open_origins_ct


LV_ORIGINS <- LV_scooter_ct %>%
  group_by(Start.Census.Tract) %>% 
  summarise(Outflow = n()) %>%
  na.omit()

LV_DESTS <- LV_scooter_ct %>%
  group_by(End.Census.Tract) %>% 
  summarise(Inflow = n()) %>%
  na.omit()

LV_net_inoutflow <- LV_open_ct %>% 
  dplyr::select(GEOID, origins_cnt, dests_cnt) %>%
  rename(Inflow = dests_cnt, Outflow = origins_cnt) %>%
  mutate(NetInflow = Inflow - Outflow,
         NetInflowRate = (Inflow - Outflow)/Inflow)
LV_net_inoutflow[is.na(LV_net_inoutflow)] <- 0
most_pickups <- LV_net_inoutflow$GEOID[LV_net_inoutflow$Outflow==max(LV_net_inoutflow$Outflow)]
most_dropoffs <- LV_net_inoutflow$GEOID[LV_net_inoutflow$Inflow==max(LV_net_inoutflow$Inflow)]


most_pickups_ct <- subset(LV_net_inoutflow,LV_net_inoutflow$GEOID==most_pickups)
most_dropoffs_ct <- subset(LV_net_inoutflow,LV_net_inoutflow$GEOID==most_dropoffs)
max_inflow <- max(abs(LV_net_inoutflow$NetInflow))
max_inflowRate <- max(abs(LV_net_inoutflow$NetInflowRate))
library(viridis)

LV_outflow_map <- ggplot()+
  geom_sf(data = st_sf(LV_net_inoutflow), aes(fill=Outflow)) +
  #geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis()+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Outflow Map for Louisville, KY') +
  mapTheme()

LV_inflow_map <- ggplot()+
  geom_sf(data = st_sf(LV_net_inoutflow), aes(fill=Inflow)) +
  #geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis()+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Inflow Map for Louisville, KY') +
  mapTheme()

LV_netinflow_map <- ggplot()+
  geom_sf(data = st_sf(LV_net_inoutflow), aes(fill=NetInflow)) +
  geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis(limits=c(-max_inflow, max_inflow))+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Net Inflow Map for Louisville, KY',subtitle='Census tract in white frame is the census tract has most inflow/outflow') +
  mapTheme()

LV_netinflow_rate_map <- ggplot()+
  geom_sf(data = st_sf(LV_net_inoutflow), aes(fill=NetInflowRate)) +
  geom_sf(data = st_sf(most_pickups_ct), color='white', fill='transparent', size=1.2) +
  #scale_fill_continuous(limits=c(-max_inflow, max_inflow)) +
  scale_fill_viridis(limits=c(-max_inflowRate, max_inflowRate))+
  #geom_sf(data = st_sf(most_dropoffs_ct), color='darkblue', fill='transparent', size=1.2) +
  labs(title='Net Inflow Rate Map for Louisville, KY',subtitle='Census tract in white frame is the census tract has most inflow/outflow') +
  mapTheme()

ggsave(file.path(plot_directory,
                 "3.1 LV_Inflow.png"),
       plot = LV_inflow_map,
       height = 6,
       width = 6,
       units = "in")

ggsave(file.path(plot_directory,
                 "3.1 LV_Outflow.png"),
       plot = LV_outflow_map,
       height = 6,
       width = 6,
       units = "in")

ggsave(file.path(plot_directory,
                 "3.1 LV_Net_inflow.png"),
       plot = LV_netinflow_map,
       height = 6,
       width = 6,
       units = "in")

ggsave(file.path(plot_directory,
                 "3.1 LV_Net_inflow_rate.png"),
       plot = LV_netinflow_rate_map,
       height = 6,
       width = 6,
       units = "in")
