# # rebalance_june_sa could be obtained using 03 code
# users_june <- subset(rebalance_june_sa, startsWith(rebalance_june_sa$reason,'user'))

data_directory <- paste(str_remove(here(), "\\/Eugene\\/Eugene - Practicum|\\/Ophelia\\/Ophelia - Practicum|\\/Xinyi\\/Xinyi - Practicum"), 
                        "/~data", 
                        sep = "")
ct_LV_file <- paste(data_directory, 
                        "/LV_tracts/censusTracts.shp",
                        sep = "")
ct_LV <- read_sf(ct_LV_file) %>% st_transform(4326)

# user_june data could be obtained by using 01 code
users_june[c("lon_s", "lat_s")] <- do.call(rbind, 
                                           lapply(strsplit(users_june$trip_origin, "[()]"), 
                                                  function(col) {   
                                                    (parts <- unlist(strsplit(col[2], " ")))
                                                  }
                                           )
)

users_june$lon_s <- as.numeric(users_june$lon_s)
users_june$lat_s <- as.numeric(users_june$lat_s)

users_june_sf <- st_as_sf(users_june, coords = c('lon_s','lat_s'),crs=4326) %>%
  mutate(lon_s = unlist(map(geometry, 1)),
         lat_s = unlist(map(geometry, 2)))
users_ct_june <- st_join(users_june_sf, ct_LV %>% select(GEOID), st_within, left=T)
users_ct_june <- rename(users_ct_june, Start.Census.Tract=GEOID)

users_june[c("lon_d", "lat_d")] <- do.call(rbind, 
                                       lapply(strsplit(users_june$trip_dest, "[()]"), 
                                              function(col) {   
                                                (parts <- unlist(strsplit(col[2], " ")))
                                              }
                                       )
)

users_ct_june$lon_d <- as.numeric(users_june$lon_d)
users_ct_june$lat_d <- as.numeric(users_june$lat_d)

users_june_sf <- st_as_sf(users_ct_june %>% st_set_geometry(NULL), coords = c('lon_d','lat_d'),crs=4326) %>%
  mutate(lon_d = unlist(map(geometry, 1)),
         lat_d = unlist(map(geometry, 2)))

users_ct_june <- st_join(users_june_sf, ct_LV %>% select(GEOID), st_within, left=T)

users_ct_june <- rename(users_ct_june, End.Census.Tract=GEOID)

##try
options(repos = c(CRAN = "http://www.stats.bris.ac.uk/R/"))

# Download / install and then load sp
install.packages("sp", depend = TRUE, lib = getwd())
library("sp", lib.loc = getwd())

# Download / install and then load maptools
install.packages("maptools", depend = TRUE, lib = getwd())
library(maptools, lib.loc = getwd())

# Download / install and then load stringr
install.packages("stringr", depend = TRUE, lib = getwd())
library("stringr", lib.loc = getwd())

# Download / install and then load rgeos
install.packages("rgeos", depend = TRUE, lib = getwd())
library("rgeos", lib.loc = getwd())

ct_LV[c('centroid_X', 'centroid_Y')] <- as.data.frame(gCentroid(spgeom = methods::as( object = ct_LV, Class = "Spatial" ),byid=TRUE))
ct_LV <- select(ct_LV, GEOID, centroid_X,centroid_Y)

try1 <- merge(users_ct_june %>% st_set_geometry(NULL), ct_LV, by.x='Start.Census.Tract', by.y='GEOID', all.x=T)
users_ct_june_xy <- rename(try1, Origin.X=centroid_X, Origin.Y=centroid_Y)

try2 <- merge(users_ct_june %>% st_set_geometry(NULL), ct_LV, by.x='End.Census.Tract', by.y='GEOID', all.x=T)
temp <- rename(try2, End.X=centroid_X, End.Y=centroid_Y)

users_ct_june_xy$End.X <- temp$End.X
users_ct_june_xy$End.Y <- temp$End.Y

ggplot()+
  geom_sf(data=ct_LV, fill='black', color='transparent')+
  #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below 
  geom_segment(data = users_june_sf, aes(x=lon_s, y=lat_s,xend=lon_d, yend=lat_d), col="white", size=.7)
  # geom_point(data = users_june_sf, aes(x=lon_s, y=lat_s), col='blue', size=.8) +
  # geom_point(data = users_june_sf, aes(x=lon_d, y=lat_d), col='green', size=.8)

users_new <- users_june_sf %>%
  group_by(lon_s,lat_s,lon_d, lat_d) %>%
  summarise(cnt=n())


ggplot()+
  geom_sf(data=ct_LV, fill='black', color='transparent')+
  #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below 
  geom_segment(data = users_new, aes(x=lon_s, y=lat_s,xend=lon_d, yend=lat_d, alpha=cnt), col="white", size=.7) +
  scale_alpha_continuous(range = c(0.03, 0.3))+
  mapTheme()

# calculate the number of trips start or end in each census tract
Origin_count <- users_ct_june %>% 
  group_by(Start.Census.Tract) %>%
  summarise(Ocount=n())

ct_LV_count <- merge(ct_LV %>% st_set_geometry(NULL), Origin_count, by.x='GEOID', by.y='Start.Census.Tract')

End_count <- users_ct_june %>% 
  group_by(End.Census.Tract) %>%
  summarise(ecount=n())

ct_LV_count <- merge(ct_LV_count %>% select(GEOID, Ocount), End_count, by.x='GEOID', by.y='End.Census.Tract')
ct_LV_count$diff <- ct_LV_count$ecount - ct_LV_count$Ocount

ggplot() +
  geom_sf(data=ct_LV_count %>%
            select(GEOID, diff) %>%
            merge(ct_LV, by='GEOID') %>% st_as_sf(), aes(fill=diff)) +
  #scale_fill_manual(values = palette5) +
  labs(title = "Net flow for each census tract in Louisville in June, 2019",
       subtitle = "Flow in - Flow out (positive value means more flow in and \nnegative means more flow out)") +
  scale_fill_viridis()+
  mapTheme()

# extract data
most_flow_out <- subset(users_ct_june, users_ct_june$Start.Census.Tract=='21111007100')
most_flow_out_cnt <- most_flow_out %>%
  group_by(lon_s,lat_s,lon_d, lat_d) %>%
  summarise(cnt=n())

ggplot()+
  geom_sf(data=ct_LV, fill='gray20', color='black')+
  geom_sf(data=ct_LV %>% st_as_sf() %>% subset(GEOID=='21111007100') , fill='black', color='transparent')+
  #geom_sf(data=dest_count, fill='gray20', color='transparent')+
  geom_segment(data = most_flow_out_cnt, aes(x=ct_LV$centroid_X[ct_LV$GEOID=='21111007100'], y=ct_LV$centroid_Y[ct_LV$GEOID=='21111007100'],xend=lon_d, yend=lat_d, alpha=cnt), col="white", size=.7) +
  scale_alpha_continuous(range = c(0.03, 0.3))+
  labs(title = 'Census tract with highest out flow', xlab('longitude'), ylab('latitude'),
       subtitle = 'GEOID: 21111007100') +
  mapTheme()

most_flow_in1 <- subset(users_ct_june, users_ct_june$Start.Census.Tract=='21111005900')
most_flow_in_cnt1 <- most_flow_in1 %>%
  group_by(lon_s,lat_s,lon_d, lat_d) %>%
  summarise(cnt=n())

ggplot()+
  geom_sf(data=ct_LV, fill='gray20', color='black')+
  geom_sf(data=ct_LV %>% st_as_sf() %>% subset(GEOID=='21111005900') , fill='black', color='transparent')+
  #geom_sf(data=dest_count, fill='gray20', color='transparent')+
  geom_segment(data = most_flow_in_cnt1, aes(x=ct_LV$centroid_X[ct_LV$GEOID=='21111005900'], y=ct_LV$centroid_Y[ct_LV$GEOID=='21111005900'],xend=lon_d, yend=lat_d, alpha=cnt), col="white", size=.7) +
  scale_alpha_continuous(range = c(0.03, 0.3))+
  labs(title = 'Census tract with highest in flow', xlab('longitude'), ylab('latitude'),
       subtitle = 'GEOID: 21111005900') +
  mapTheme()

most_flow_in2 <- subset(users_ct_june, users_ct_june$Start.Census.Tract=='21111005300')
most_flow_in_cnt2 <- most_flow_in2 %>%
  group_by(lon_s,lat_s,lon_d, lat_d) %>%
  summarise(cnt=n())

ggplot()+
  geom_sf(data=ct_LV, fill='gray20', color='black')+
  geom_sf(data=ct_LV %>% st_as_sf() %>% subset(GEOID=='21111005300') , fill='black', color='transparent')+
  #geom_sf(data=dest_count, fill='gray20', color='transparent')+
  geom_segment(data = most_flow_in_cnt2, aes(x=ct_LV$centroid_X[ct_LV$GEOID=='21111005300'], y=ct_LV$centroid_Y[ct_LV$GEOID=='21111005300'],xend=lon_d, yend=lat_d, alpha=cnt), col="white", size=.7) +
  scale_alpha_continuous(range = c(0.03, 0.3))+
  labs(title = 'Census tract with highest in flow', xlab('longitude'), ylab('latitude'),
       subtitle = 'GEOID: 21111005300') +
  mapTheme()
# # Code used to identify census tract that scooters flow to
# where_they_flow <- most_flow_out %>%
#   group_by(lon_d, lat_d) %>%
#   summarise(cnt=n())
# 
# where_they_flow <- st_join(most_flow_out_cnt, ct_LV %>% select(GEOID), st_intersects, left=T)
# dest_count <- where_they_flow%>%
#     group_by(GEOID) %>%
#   summarise(count=n()) %>%
#   top_n(n = 3, wt = count) %>%
#   st_set_geometry(NULL) %>%
#   merge(ct_LV,by='GEOID') %>%
#   st_as_sf()



