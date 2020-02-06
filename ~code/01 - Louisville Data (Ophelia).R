
base_map <- st_read("https://opendata.arcgis.com/datasets/6e3dea8bd9cf49e6a764f7baa9141a95_30.geojson")
proj <- 2246 # https://www.spatialreference.org/ref/epsg/2246/
base_map_proj <- base_map %>% st_transform(proj)

# Read rebalance data ----
# rebalance_file <- paste(data_directory, 
#                         "/Louisville-MDS-Status-Changes-2019Dec17.csv",
#                         sep = "")
# 
# rebalance_data <- read_csv(rebalance_file)

# What is the distribution of scooter status change activities?

activity_distro_plot <- rebalance_data %>% 
  ggplot(aes(x = reason)) +
  geom_bar(stat = "count", position = "identity") +
  facet_wrap(~ type, scales = "free") +
  coord_flip() +
  labs(x = "Reason for Status Change",
       y = "Count",
       title = "Distribution of Scooter Status Change Activities")
activity_distro_plot

# What is the geographic distribution of rebalancing activities only?

rebalance_data_sf <- st_as_sf(rebalance_data,
                              wkt = "location", 
                              crs = 4326)

rebalance_only <- rebalance_data_sf %>% 
  filter(str_detect(reason, "rebalance"))
rebalance_only <- rebalance_only[base_map,] #intersect data

ggplot() +
  geom_sf(data = base_map, fill = NA, color = "lightgray") +
  geom_sf(data = rebalance_only, 
          aes(color = reason),
          alpha = 0.3) +
  facet_wrap(~ reason) +
  theme_minimal()

# fishnet
boundary <- st_union(base_map_proj) %>% st_sf()

cell_area <- conv_unit(0.5, from = "mi2", to = "ft2")
cell_size <- (cell_area * (2/3^0.5)) ^ 0.5 # the "cellsize" parameter is the distance between the centroids of each hexagonal cell.

lville_fishnet <- st_make_grid(boundary, cellsize = cell_size, square = FALSE) %>% 
  st_sf() %>% 
  mutate(fishnet_ID = row_number())

####analyzing trip data (Ophelia)
user_only <- rebalance_data_sf %>% 
  filter(str_detect(reason, "user"))
user_only <- user_only[base_map,] #intersect data

###pick June, 2019 
user_only_0619 <- user_only %>%
  filter(year(occurredAt) == 2019,
         month(occurredAt) == 6)

glimpse(user_only_0619)
user_only_0619$operators <- as.factor(user_only_0619$operators)

pickup_plot <- 
  ggplot() +
  geom_sf(data = base_map, fill = NA, color = "lightgray") +
  geom_sf(data = user_only_0619 %>% filter(reason == 'user pick up'), 
          aes(color = operators), 
          alpha = 0.3) +
  scale_colour_manual(values = paletteY,
                      name = "Operator") +
  theme_minimal()

pickup_plot

user_only_0619 <- user_only_0619[order(user_only_0619$vehicleId, user_only_0619$occurredAt),]
user_only_0619 <- user_only_0619 %>%
  mutate(duration = 0,
         energy_diff = 0)

new_df <- data.frame()

  for (veh in unique(user_only_0619$vehicleId)) {
    this_vehicle_set <- user_only_0619 %>% filter(vehicleId == veh)
    print(veh)
    for (i in 1:nrow(this_vehicle_set)) {
      if (i%%2 == 1) {
        this_vehicle_set$duration[i] = this_vehicle_set$occurredAt[i+1]- this_vehicle_set$occurredAt[i] 
        this_vehicle_set$energy_diff[i] = this_vehicle_set$vehicleEnergyLevel[i+1]- this_vehicle_set$vehicleEnergyLevel[i] 
      } else {}
    }
    new_df <- rbind.data.frame(new_df, this_vehicle_set)
  }

for (veh in unique(user_only_0619$vehicleId)) {
  this_vehicle_set <- user_only_0619 %>% subset(vehicleId == veh)
  print(veh)
  for (i in 1:nrow(this_vehicle_set)) {
    if (i%%2 == 1) {
      this_vehicle_set$duration[i] = this_vehicle_set$occurredAt[i+1]- this_vehicle_set$occurredAt[i] 
      this_vehicle_set$energy_diff[i] = this_vehicle_set$vehicleEnergyLevel[i+1]- this_vehicle_set$vehicleEnergyLevel[i] 
    } else {}
  }
  new_df <- rbind.data.frame(new_df, this_vehicle_set)
}

vehicleID <- NA
start_time <- c(0)
end_time <- c(0)
trip_origin <- c(0)
trip_dest <- c(0)
duration <- c(0)
energy_diff <- c(0)

test <- data.frame()


for (i in 1:nrow(new_df)) {
  if (i%%2 == 1) {
    j = i/2 + 0.5
    print(i)
    print(j)
    print(new_df$vehicleId[i])
    temp <- data.frame(vehicleID, start_time, end_time, trip_origin, trip_dest, duration, energy_diff)
    temp$vehicleID = new_df$vehicleId[i]
    temp$start_time = new_df$occurredAt[i]
    temp$end_time = new_df$occurredAt[i+1]
    temp$duration = new_df$duration[i]
    temp$trip_origin = new_df$location[i]
    temp$trip_dest = new_df$location[i+1]
    temp$energy_diff = new_df$energy_diff[i]
    print(temp)
    test <- rbind.data.frame(test, temp)
  } else {}
}

trip_0619 <- test
trip_0619$energy_diff <- abs(trip_0619$energy_diff)
trip_0619 <- trip_0619 %>%
  mutate(start_hr = floor_date((start_time), unit = "hour"),
         week = week(start_hr),
         dotw = wday(start_hr, label=TRUE))

# PLOT by day of week 
dofw_plot <- ggplot(trip_0619 %>% mutate(hour = hour(start_time)))+
  geom_freqpoly(aes(hour, color = dotw), binwidth = 1)+
  labs(title="Scooter trips in Louisville, by day of the week, June, 2019",
       x="Hour", 
       y="Trip Counts")+
 # xlim(0, 23)+
  theme_minimal()

dofw_plot

# dofw_plot2 <- ggplot(user_only_0619 %>% 
#                        filter(user_only_0619$reason == 'user pick up') %>%
#                        mutate(hour = hour(occurredAt),
#                               dotw = wday(occurredAt, label=TRUE)))+
#   geom_freqpoly(aes(hour, color = dotw), binwidth = 1)+
#   labs(title="Scooter trips in Louisville, by day of the week, June, 2019",
#        x="Hour", 
#        y="Trip Counts")+
#   # xlim(0, 23)+
#   theme_minimal()
# 
# dofw_plot2


# PLOT trip duration
duration_plot <- ggplot(trip_0619, aes(duration))+
  geom_histogram(bins = 50, colour="white", fill = "#FFD365") +
  labs(title="Distribution of scooter trip duration (mins) in Louisville, June, 2019",
       x="Minites", 
       y="Count")+
  theme_minimal()

duration_plot

# PLOT energy usage
energyuse_plot <- ggplot(trip_0619, aes(energy_diff))+
  geom_histogram(bins = 50, colour="white", fill = "#FFD365") +
  labs(title="Energy used of scooter trip in Louisville, June, 2019",
       x="Energy used", 
       y="Count")+
  theme_minimal()

energyuse_plot

# PLOT 
testtest <- trip_0619 %>% 
  mutate(hour = hour(start_time),
         weekend = ifelse(dotw %in% c("Sun", "Sat"), "Weekend", "Weekday"),
         time_of_day = case_when(hour < 7 | hour > 18 ~ "Night time",
                                 hour >= 7 & hour <= 18 ~ "Day time"))%>%
  dplyr::select(-trip_dest)

ggplot() + geom_sf(data = testtest, aes(geometry = trip_origin))

glimpse(testtest)

duration_place_plot <- ggplot()+
  geom_sf(data = base_map, fill = NA, color = "lightgray")+
  geom_sf(data = trip_0619 %>% 
            mutate(hour = hour(start_time),
                   weekend = ifelse(dotw %in% c("Sun", "Sat"), "Weekend", "Weekday"),
                   time_of_day = case_when(hour < 7 | hour > 18 ~ "Night time",
                                           hour >= 7 & hour <= 18 ~ "Day time")),
          #            group_by(weekend, time_of_day) %>%
          #            tally(),
          aes(geometry = trip_origin, color = duration), alpha = 0.3, size = 0.75)+
  scale_colour_viridis(direction = -1,
                       discrete = FALSE, option = "D")+
  #  facet_grid(weekend ~ time_of_day)+
  labs(title="Scooter trips duration. Louisville, June, 2019")+
  theme_minimal()

duration_place_plot

## for rebalance only data ####
# rebalance_0619 <- rebalance_only %>%
#   filter(year(occurredAt) == 2019,
#          month(occurredAt) == 6)
# 
# glimpse(rebalance_0619)
# rebalance_0619$operators <- as.factor(rebalance_0619$operators)


# rebalance_0619 <- rebalance_0619[order(rebalance_0619$vehicleId, rebalance_0619$occurredAt),]
# rebalance_0619 <- rebalance_0619 %>%
#   mutate(duration = 0,
#          energy_diff = 0)



#trim the dataset to be "pick-up; drop-off" format
test_list <- c()
for (veh in unique(rebalance_0619$vehicleId)) {
  this_vehicle_set <- rebalance_0619 %>% filter(vehicleId == veh)
  print(veh)
  temp_list <- c()
  for (i in 1:nrow(this_vehicle_set)) {
    if (i > 1){
      if (this_vehicle_set$reason[i] == 'rebalance drop off') {
        temp_list <- append(temp_list, c(this_vehicle_set$id[i], this_vehicle_set$id[i-1]))
        print(temp_list)} else{}
    } else{}
  }
  test_list <- append(test_list, temp_list)
}

rebalance_0619_new <- rebalance_0619 %>%
  filter(rebalance_0619$id %in% test_list)

test_list_2 <- c()
for (veh in unique(rebalance_0619_new$vehicleId)) {
  this_vehicle_set <- rebalance_0619_new %>% filter(vehicleId == veh)
  print(veh)
  temp_list <- c()
  for (i in 1:nrow(this_vehicle_set)) {
      if (this_vehicle_set$reason[i] == 'rebalance pick up') {
        temp_list <- append(temp_list, c(this_vehicle_set$id[i], this_vehicle_set$id[i+1]))
        print(temp_list)} else{}
  }
  test_list_2 <- append(test_list_2, temp_list)
}

rebalance_0619_final <- rebalance_0619_new %>%
  filter(rebalance_0619_new$id %in% test_list_2)

new_reb_df <- data.frame()

for (veh in unique(rebalance_0619_final$vehicleId)) {
  this_vehicle_set <- rebalance_0619_final %>% filter(vehicleId == veh)
  print(veh)
  for (i in 1:nrow(this_vehicle_set)) {
    if (i%%2 == 1) {
      this_vehicle_set$duration[i] = this_vehicle_set$occurredAt[i+1]- this_vehicle_set$occurredAt[i] 
      this_vehicle_set$energy_diff[i] = this_vehicle_set$vehicleEnergyLevel[i+1]- this_vehicle_set$vehicleEnergyLevel[i] 
    } else {}
  }
  new_reb_df <- rbind.data.frame(new_reb_df, this_vehicle_set)
}

vehicleID <- NA
start_time <- c(0)
end_time <- c(0)
trip_origin <- c(0)
trip_dest <- c(0)
duration <- c(0)
energy_diff <- c(0)

test <- data.frame()

for (i in 1:nrow(new_reb_df)) {
  if (i%%2 == 1) {
    j = i/2 + 0.5
    print(i)
    print(j)
    print(new_reb_df$vehicleId[i])
    temp <- data.frame(vehicleID, start_time, end_time, trip_origin, trip_dest, duration, energy_diff)
    temp$vehicleID = new_reb_df$vehicleId[i]
    temp$start_time = new_reb_df$occurredAt[i]
    temp$end_time = new_reb_df$occurredAt[i+1]
    temp$duration = new_reb_df$duration[i]
    temp$trip_origin = new_reb_df$location[i]
    temp$trip_dest = new_reb_df$location[i+1]
    temp$energy_diff = new_reb_df$energy_diff[i]
    print(temp)
    test <- rbind.data.frame(test, temp)
  } else {}
}

reb_0619 <- test
reb_0619 <- reb_0619 %>%
  mutate(start_hr = floor_date((start_time), unit = "hour"),
         week = week(start_hr),
         dotw = wday(start_hr, label=TRUE))

### flow maps ####
ct_LV_file <- paste(data_directory, 
                    "/LV_tracts/censusTracts.shp",
                    sep = "")

ct_LV <- read_sf("/Users/ophelia/Box Sync/Practicum - Louisville Scooters/~data/LV_tracts/censusTracts.shp")%>% st_transform(4326)

reb_0619 <- reb_0619 %>% 
  mutate(lon_s = st_coordinates(reb_0619$trip_origin)[, 1],
         lat_s = st_coordinates(reb_0619$trip_origin)[, 2],
         lon_d = st_coordinates(reb_0619$trip_dest)[, 1],
         lat_d = st_coordinates(reb_0619$trip_dest)[, 2],)

reb_0619_sf <- st_as_sf(reb_0619, coords = c('lon_s','lat_s'),crs=4326) %>%
  mutate(lon_s = unlist(map(geometry, 1)),
         lat_s = unlist(map(geometry, 2)))

reb_0619_join <- st_join(reb_0619_sf, ct_LV %>% select(GEOID), st_within, left=T)
reb_0619_join <- rename(reb_0619_join, Start.Census.Tract=GEOID)


reb_0619_sf <- st_as_sf(reb_0619_join%>% st_set_geometry(NULL), coords = c('lon_d','lat_d'),crs=4326) %>%
  mutate(lon_d = unlist(map(geometry, 1)),
         lat_d = unlist(map(geometry, 2)))
reb_0619_join <- st_join(reb_0619_sf, ct_LV %>% select(GEOID), st_within, left=T)
reb_0619_join <- rename(reb_0619_join, End.Census.Tract=GEOID)


## rebalancing plot 
options(repos = c(CRAN = "http://www.stats.bris.ac.uk/R/"))

ct_LV <- ct_LV %>% 
  mutate(centroid_X = st_coordinates(st_centroid(ct_LV))[, 1],
         centroid_Y = st_coordinates(st_centroid(ct_LV))[, 2])

ct_LV <- select(ct_LV, GEOID, centroid_X,centroid_Y)

reb_0619_new <- reb_0619_sf %>%
  group_by(lon_s,lat_s,lon_d, lat_d) %>%
  summarise(cnt=n())

ggplot()+
  geom_sf(data=ct_LV, fill='black', color='transparent')+
  #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below 
  geom_segment(data = reb_0619_new, aes(x=lon_s, y=lat_s, xend=lon_d, yend=lat_d, alpha=cnt), color="yellow", size=0.7) +
  scale_alpha_continuous(range = c(0.1, 0.4))+
  labs(title = "Flow of scooter rebalancing in Louisville in June, 2019")+
  theme_minimal()

# calculate the number of trips start or end in each census tract
Origin_count <- reb_0619_join %>% 
  group_by(Start.Census.Tract) %>%
  summarise(Ocount=n())

ct_LV_count <- merge(ct_LV %>% st_set_geometry(NULL), Origin_count, by.x='GEOID', by.y='Start.Census.Tract')

End_count <- reb_0619_join %>% 
  group_by(End.Census.Tract) %>%
  summarise(ecount=n())

ct_LV_count <- merge(ct_LV_count %>% select(GEOID, Ocount), End_count, by.x='GEOID', by.y='End.Census.Tract')
ct_LV_count$diff <- ct_LV_count$ecount - ct_LV_count$Ocount

ggplot() +
  geom_sf(data=ct_LV, fill='grey')+
  geom_sf(data=ct_LV_count %>%
            select(GEOID, diff) %>%
            merge(ct_LV, by='GEOID') %>% st_as_sf(), aes(fill=diff)) +
  #scale_fill_manual(values = palette5) +
  labs(title = "Net flow of rebalanced scooter for each census tract in Louisville in June, 2019",
       subtitle = "Flow in - Flow out (positive value means rebalanced scooter inflows)") +
  scale_fill_viridis()+
  theme_minimal()

# extract data
most_reb_flow_out <- subset(reb_0619_join, reb_0619_join$Start.Census.Tract=='21111007400')
most_reb_flow_out_cnt <- most_reb_flow_out %>%
  group_by(lon_s,lat_s,lon_d, lat_d) %>%
  summarise(cnt=n())

ggplot()+
  geom_sf(data=ct_LV, fill='gray20', color='black')+
  geom_sf(data=ct_LV %>% st_as_sf() %>% subset(GEOID=='	21111007400') , fill='black', color='transparent')+
  #geom_sf(data=dest_count, fill='gray20', color='transparent')+
  geom_segment(data = most_reb_flow_out_cnt, aes(x=ct_LV$centroid_X[ct_LV$GEOID=='21111007400'], y=ct_LV$centroid_Y[ct_LV$GEOID=='21111007400'],xend=lon_d, yend=lat_d, alpha=cnt), col="yellow", size=.7) +
  scale_alpha_continuous(range = c(0.03, 0.3))+
  labs(title = 'Census tract with highest outflow of rebalanced scooter', xlab('longitude'), ylab('latitude'),
       subtitle = 'GEOID: 21111007400') +
  theme_minimal()

most_reb_flow_in1 <- subset(reb_0619_join, reb_0619_join$Start.Census.Tract=='21111005300')
most_reb_flow_in_cnt1 <- most_reb_flow_in1 %>%
  group_by(lon_s,lat_s,lon_d, lat_d) %>%
  summarise(cnt=n())

ggplot()+
  geom_sf(data=ct_LV, fill='gray20', color='black')+
  geom_sf(data=ct_LV %>% st_as_sf() %>% subset(GEOID=='21111005300') , fill='black', color='transparent')+
  #geom_sf(data=dest_count, fill='gray20', color='transparent')+
  geom_segment(data = most_reb_flow_in_cnt1, aes(x=ct_LV$centroid_X[ct_LV$GEOID=='21111005300'], y=ct_LV$centroid_Y[ct_LV$GEOID=='21111005300'],xend=lon_d, yend=lat_d, alpha=cnt), col="yellow", size=.7) +
  scale_alpha_continuous(range = c(0.03, 0.3))+
  labs(title = 'Census tract with highest inflow of rebalanced scooter', xlab('longitude'), ylab('latitude'),
       subtitle = 'GEOID: 21111005300') +
  theme_minimal()

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

### how long does it take for a rebalanced scooter to be picked up again?
reb_list <- unique(rebalance_only$vehicleId)
reb_pkup <- rebalance_data_sf %>%
  filter(rebalance_data_sf$vehicleId %in% reb_list)

reb_pkup_0619 <- reb_pkup %>%
  filter(year(occurredAt) == 2019,
         month(occurredAt) == 6)

reb_pkup_0619 <- reb_pkup_0619[order(reb_pkup_0619$vehicleId, reb_pkup_0619$occurredAt),]

# PLOT rebalance by day of week 
dofw_reb_plot <- ggplot(reb_0619_sf %>% mutate(hour = hour(start_time)))+
  geom_freqpoly(aes(hour, color = dotw), binwidth = 1)+
  labs(title="Number of scooter rebalance activities in Louisville, by day of the week, June, 2019",
       x="Hour", 
       y="Scooter relabance activity")+
  # xlim(0, 23)+
  theme_minimal()

dofw_reb_plot

save.image(file='~/desktop/Practicum/ophelia.RData')
save.image(file='ophelia.RData')
#load('~/desktop/Practicum/ophelia.RData')

