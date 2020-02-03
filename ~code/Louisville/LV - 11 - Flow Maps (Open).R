##########################################################################
# This script:
# 1. Creates flow maps from the rebalancing data
#
# Depends on 01 - Louisville Data
##########################################################################
### users events####
# Read the structured rebalance data - users events
LV_rebal_user_only_combined_rowPairs_ct_RDS <- file.path(data_directory, 
                                                         "~RData/Louisville/LV_rebal_user_only_combined_rowPairs_ct")

LV_rebal_user_only_combined_rowPairs_ct <- readRDS(LV_rebal_user_only_combined_rowPairs_ct_RDS)

#### plot flow map ####

# raw data flow map, messy ####
# ggplot()+
#   geom_sf(data=LV_Census_geoinfo, fill='black', color='transparent')+
#   geom_segment(data = LV_rebal_user_only_combined_rowPairs_sf, aes(x=lon_s, y=lat_s,xend=lon_d, yend=lat_d), col="white", size=.7)+
#   scale_alpha_continuous(range = c(0.03, 0.3)) +
#   mapTheme()

## less messy way
# LV_rebal_user_only_combined_rowPairs_groupby_dest <- LV_rebal_user_only_combined_rowPairs_sf %>%
#   group_by(lon_s, lat_s,lon_d, lat_d) %>%
#   summarise(cnt=n())
# 
# 
# ggplot()+
#   geom_sf(data=LV_Census_geoinfo, fill='black', color='transparent')+
#   #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below 
#   geom_segment(data = LV_rebal_user_only_combined_rowPairs_groupby_dest, aes(x=lon_s, y=lat_s,xend=lon_d, yend=lat_d, alpha=cnt), col="white", size=.7) +
#   scale_alpha_continuous(range = c(0.03, 0.3))+
#   mapTheme()


## for highest flowins/flowouts census tracts ####

# calculate the number of trips start or end in each census tract
LV_rebal_user_only_combined_rowPairs_ct_origin_count <- LV_rebal_user_only_combined_rowPairs_ct %>% 
  group_by(Start.Census.Tract) %>%
  summarise(Ocount=n())
LV_rebal_user_only_combined_rowPairs_ct_count <- merge(LV_Census_geoinfo %>% st_set_geometry(NULL), LV_rebal_user_only_combined_rowPairs_ct_origin_count, by.x='GEOID', by.y='Start.Census.Tract')

LV_rebal_user_only_combined_rowPairs_ct_end_count <- LV_rebal_user_only_combined_rowPairs_ct %>% 
  group_by(End.Census.Tract) %>%
  summarise(Ecount=n())
LV_rebal_user_only_combined_rowPairs_ct_count <- merge(LV_rebal_user_only_combined_rowPairs_ct_count %>% select(GEOID, Ocount), LV_rebal_user_only_combined_rowPairs_ct_end_count, by.x='GEOID', by.y='End.Census.Tract')

LV_rebal_user_only_combined_rowPairs_ct_count$diff <- LV_rebal_user_only_combined_rowPairs_ct_count$Ecount - LV_rebal_user_only_combined_rowPairs_ct_count$Ocount

ggplot() +
  geom_sf(data = LV_Census_geoinfo, fill='grey') +
  geom_sf(data=LV_rebal_user_only_combined_rowPairs_ct_count %>%
            select(GEOID, diff) %>%
            merge(LV_Census_geoinfo, by='GEOID') %>% st_as_sf(), aes(fill=diff)) +
  labs(title = "Net flow for each census tract in Louisville in June, 2019",
       subtitle = "Flow in - Flow out (positive value means more flow in and \nnegative means more flow out)") +
  scale_fill_viridis()+
  mapTheme()

# extract data
LV_rebal_user_only_most_flow_out <- LV_rebal_user_only_combined_rowPairs_ct %>%
  filter(Start.Census.Tract=='21111007100') %>%
  group_by(lon_d, lat_d) %>%
  summarise(cnt=n())

ggplot()+
  geom_sf(data=LV_Census_geoinfo, fill='gray20', color='black')+
  geom_sf(data=LV_Census_geoinfo %>% st_as_sf() %>% filter(GEOID=='21111007100') , fill='black', color='transparent')+
  geom_segment(data = LV_rebal_user_only_most_flow_out, aes(x=LV_Census_geoinfo$centroid_X[LV_Census_geoinfo$GEOID=='21111007100'], y=LV_Census_geoinfo$centroid_Y[LV_Census_geoinfo$GEOID=='21111007100'],xend=lon_d, yend=lat_d, alpha=cnt), col="white", size=.7) +
  scale_alpha_continuous(range = c(0.03, 0.3))+
  labs(title = 'Census tract with highest out flow', xlab('longitude'), ylab('latitude'),
       subtitle = 'GEOID: 21111007100') +
  mapTheme()

LV_rebal_user_only_most_flow_in1 <- LV_rebal_user_only_combined_rowPairs_ct %>%
  filter(Start.Census.Tract=='21111005900') %>%
  group_by(lon_d, lat_d) %>%
  summarise(cnt=n())

ggplot()+
  geom_sf(data=LV_Census_geoinfo, fill='gray20', color='black')+
  geom_sf(data=LV_Census_geoinfo %>% st_as_sf() %>% subset(GEOID=='21111005900') , fill='black', color='transparent')+
  #geom_sf(data=dest_count, fill='gray20', color='transparent')+
  geom_segment(data = LV_rebal_user_only_most_flow_in1, aes(x=LV_Census_geoinfo$centroid_X[LV_Census_geoinfo$GEOID=='21111005900'], y=LV_Census_geoinfo$centroid_Y[LV_Census_geoinfo$GEOID=='21111005900'],xend=lon_d, yend=lat_d, alpha=cnt), col="white", size=.7) +
  scale_alpha_continuous(range = c(0.03, 0.3))+
  labs(title = 'Census tract with highest in flow', xlab('longitude'), ylab('latitude'),
       subtitle = 'GEOID: 21111005900') +
  mapTheme()

LV_rebal_user_only_most_flow_in2 <- LV_rebal_user_only_combined_rowPairs_ct %>%
  filter(Start.Census.Tract=='21111005300') %>%
  group_by(lon_d, lat_d) %>%
  summarise(cnt=n())

ggplot()+
  geom_sf(data=LV_Census_geoinfo, fill='gray20', color='black')+
  geom_sf(data=LV_Census_geoinfo %>% st_as_sf() %>% subset(GEOID=='21111005300') , fill='black', color='transparent')+
  #geom_sf(data=dest_count, fill='gray20', color='transparent')+
  geom_segment(data = LV_rebal_user_only_most_flow_in2, aes(x=LV_Census_geoinfo$centroid_X[LV_Census_geoinfo$GEOID=='21111005300'], y=LV_Census_geoinfo$centroid_Y[LV_Census_geoinfo$GEOID=='21111005300'],xend=lon_d, yend=lat_d, alpha=cnt), col="white", size=.7) +
  scale_alpha_continuous(range = c(0.03, 0.3))+
  labs(title = 'Census tract with highest in flow', xlab('longitude'), ylab('latitude'),
       subtitle = 'GEOID: 21111005300') +
  mapTheme()





### rebalance events####

#@@@@@@ Hey Ophelia, your code should be added here.

