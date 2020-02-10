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
LV_rebal_reb_only_0619_combined_rowPairs <- LV_rebal_reb_only_0619_combined_rowPairs %>% 
  mutate(lon_s = st_coordinates(LV_rebal_reb_only_0619_combined_rowPairs$trip_origin)[, 1],
         lat_s = st_coordinates(LV_rebal_reb_only_0619_combined_rowPairs$trip_origin)[, 2],
         lon_d = st_coordinates(LV_rebal_reb_only_0619_combined_rowPairs$trip_dest)[, 1],
         lat_d = st_coordinates(LV_rebal_reb_only_0619_combined_rowPairs$trip_dest)[, 2],)

LV_rebal_reb_only_0619_combined_rowPairs_sf <- st_as_sf(LV_rebal_reb_only_0619_combined_rowPairs, coords = c('lon_s','lat_s')) 

LV_rebal_reb_only_0619_combined_rowPairs_sf <- LV_rebal_reb_only_0619_combined_rowPairs_sf %>%
  st_transform(4326) %>%
  mutate(lon_s = unlist(map(trip_origin, 1)),
         lat_s = unlist(map(trip_origin, 2)))

LV_rebal_reb_only_0619_combined_rowPairs_join <- st_join(LV_rebal_reb_only_0619_combined_rowPairs_sf, ct_LV %>% dplyr::select(GEOID), st_within, left=T)
LV_rebal_reb_only_0619_combined_rowPairs_join <- rename(LV_rebal_reb_only_0619_combined_rowPairs_join, Start.Census.Tract=GEOID)


LV_rebal_reb_only_0619_combined_rowPairs_sf <- st_as_sf(LV_rebal_reb_only_0619_combined_rowPairs_join%>% st_set_geometry(NULL), coords = c('lon_d','lat_d')) %>%
  st_transform(crs = 4326)%>%
  mutate(lon_d = unlist(map(trip_dest, 1)),
         lat_d = unlist(map(trip_dest, 2)))
LV_rebal_reb_only_0619_combined_rowPairs_join <- st_join(LV_rebal_reb_only_0619_combined_rowPairs_sf, ct_LV %>% dplyr::select(GEOID), st_within, left=T)
LV_rebal_reb_only_0619_combined_rowPairs_join <- rename(LV_rebal_reb_only_0619_combined_rowPairs_join, End.Census.Tract=GEOID)

LV_rebal_reb_only_0619_new <- LV_rebal_reb_only_0619_combined_rowPairs_join %>%
  group_by(lon_s,lat_s,lon_d, lat_d) %>%
  summarise(cnt=n())

ggplot()+
  geom_sf(data=ct_LV, fill='black', color='transparent')+
  #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below 
  geom_segment(data = LV_rebal_reb_only_0619_new, aes(x=lon_s, y=lat_s, xend=lon_d, yend=lat_d, alpha=cnt), color="yellow", size=0.7) +
  scale_alpha_continuous(range = c(0.1, 0.4))+
  labs(title = "Flow of scooter rebalancing in Louisville in June, 2019")+
  theme_minimal()

# calculate the number of trips start or end in each census tract
LV_0619_origin_count <- LV_rebal_reb_only_0619_combined_rowPairs_join %>% 
  group_by(Start.Census.Tract) %>%
  summarise(Ocount=n())

LV_reb_count_tract <- merge(ct_LV %>% st_set_geometry(NULL), LV_0619_origin_count, by.x='GEOID', by.y='Start.Census.Tract')

LV_0619_dest_count <- LV_rebal_reb_only_0619_combined_rowPairs_join %>% 
  group_by(End.Census.Tract) %>%
  summarise(Dcount=n())

LV_reb_count_tract <- merge(LV_reb_count_tract %>% dplyr::select(GEOID, Ocount), LV_0619_dest_count, by.x='GEOID', by.y='End.Census.Tract')
LV_reb_count_tract$diff <- LV_reb_count_tract$Dcount - LV_reb_count_tract$Ocount

ggplot() +
  geom_sf(data=ct_LV, fill='grey')+
  geom_sf(data=LV_count_tract %>%
            dplyr::select(GEOID, diff) %>%
            merge(ct_LV, by='GEOID') %>% st_as_sf(), aes(fill=diff)) +
  #scale_fill_manual(values = palette5) +
  labs(title = "Net flow of rebalanced scooter for each census tract in Louisville in June, 2019",
       subtitle = "Flow in - Flow out (positive value means rebalanced scooter inflows)") +
  scale_fill_viridis()+
  theme_minimal()

# extract data
LV_most_reb_flow_out <- subset(LV_rebal_reb_only_0619_combined_rowPairs_join, LV_rebal_reb_only_0619_combined_rowPairs_join$Start.Census.Tract=='21111004900')
LV_most_reb_flow_out_count <- LV_most_reb_flow_out %>%
  group_by(lon_s,lat_s,lon_d, lat_d) %>%
  summarise(cnt=n())

ggplot()+
  geom_sf(data=ct_LV, fill='gray20', color='black')+
  geom_sf(data=ct_LV %>% st_as_sf() %>% subset(GEOID=='	21111004900') , fill='black', color='transparent')+
  #geom_sf(data=dest_count, fill='gray20', color='transparent')+
  geom_segment(data = LV_most_reb_flow_out_count, aes(x=ct_LV$centroid_X[ct_LV$GEOID=='21111004900'], y=ct_LV$centroid_Y[ct_LV$GEOID=='21111004900'],xend=lon_d, yend=lat_d, alpha=cnt), col="yellow", size=.7) +
  scale_alpha_continuous(range = c(0.03, 0.3))+
  labs(title = 'Census tract with highest outflow of rebalanced scooter', xlab('longitude'), ylab('latitude'),
       subtitle = 'GEOID: 21111004900') +
  theme_minimal()

LV_most_reb_flow_in1 <- subset(LV_rebal_reb_only_0619_combined_rowPairs_join, LV_rebal_reb_only_0619_combined_rowPairs_join$Start.Census.Tract=='21111004900')
LV_most_reb_flow_in1_count <- LV_most_reb_flow_in1 %>%
  group_by(lon_s,lat_s,lon_d, lat_d) %>%
  summarise(cnt=n())

LV_most_reb_flow_in2 <- subset(LV_rebal_reb_only_0619_combined_rowPairs_join, LV_rebal_reb_only_0619_combined_rowPairs_join$Start.Census.Tract=='21111005300')
LV_most_reb_flow_in2_count <- LV_most_reb_flow_in1 %>%
  group_by(lon_s,lat_s,lon_d, lat_d) %>%
  summarise(cnt=n())

LV_most_reb_flow_in3 <- subset(LV_rebal_reb_only_0619_combined_rowPairs_join, LV_rebal_reb_only_0619_combined_rowPairs_join$Start.Census.Tract=='21111005900')
LV_most_reb_flow_in3_count <- LV_most_reb_flow_in1 %>%
  group_by(lon_s,lat_s,lon_d, lat_d) %>%
  summarise(cnt=n())


grid.arrange(
ggplot()+
  geom_sf(data=ct_LV, fill='gray20', color='black')+
  geom_sf(data=ct_LV %>% st_as_sf() %>% subset(GEOID=='21111004900') , fill='black', color='transparent')+
  #geom_sf(data=dest_count, fill='gray20', color='transparent')+
  geom_segment(data = LV_most_reb_flow_in1_count, aes(x=ct_LV$centroid_X[ct_LV$GEOID=='21111004900'], y=ct_LV$centroid_Y[ct_LV$GEOID=='21111004900'],xend=lon_d, yend=lat_d, alpha=cnt), col="yellow", size=.7) +
  scale_alpha_continuous(range = c(0.03, 0.3))+
  labs(title = 'Census tract with the highest inflow of rebalanced scooter', xlab('longitude'), ylab('latitude'),
       subtitle = 'GEOID: 21111004900') +
  theme_minimal(),

ggplot()+
  geom_sf(data=ct_LV, fill='gray20', color='black')+
  geom_sf(data=ct_LV %>% st_as_sf() %>% subset(GEOID=='21111005300') , fill='black', color='transparent')+
  #geom_sf(data=dest_count, fill='gray20', color='transparent')+
  geom_segment(data = LV_most_reb_flow_in2_count, aes(x=ct_LV$centroid_X[ct_LV$GEOID=='21111005300'], y=ct_LV$centroid_Y[ct_LV$GEOID=='21111005300'],xend=lon_d, yend=lat_d, alpha=cnt), col="yellow", size=.7) +
  scale_alpha_continuous(range = c(0.03, 0.3))+
  labs(title = 'Census tract with the second highest inflow of rebalanced scooter', xlab('longitude'), ylab('latitude'),
       subtitle = 'GEOID: 21111005300') +
  theme_minimal(),

ggplot()+
  geom_sf(data=ct_LV, fill='gray20', color='black')+
  geom_sf(data=ct_LV %>% st_as_sf() %>% subset(GEOID=='21111005900') , fill='black', color='transparent')+
  #geom_sf(data=dest_count, fill='gray20', color='transparent')+
  geom_segment(data = LV_most_reb_flow_in3_count, aes(x=ct_LV$centroid_X[ct_LV$GEOID=='21111005900'], y=ct_LV$centroid_Y[ct_LV$GEOID=='21111005900'],xend=lon_d, yend=lat_d, alpha=cnt), col="yellow", size=.7) +
  scale_alpha_continuous(range = c(0.03, 0.3))+
  labs(title = 'Census tract with the third highest inflow of rebalanced scooter', xlab('longitude'), ylab('latitude'),
       subtitle = 'GEOID: 21111005900') +
  theme_minimal(),

ncol = 3)
