library(here)
library(tidyverse)
library(sf)
library(measurements)
library(dplyr)
library(sp)
library(tidycensus)
library(lubridate)
library(viridis)

paletteY <- c("#F9F871","#FFD364","#FFAF6D","#FF8F80","#F87895", "D16BA5")
palette5 <- c("#25CB10", "#5AB60C", "#8FA108","#C48C04", "#FA7800")



setwd(here::here())

data_directory <- paste(str_remove(here(), "\\/Eugene\\/Eugene - Practicum|\\/Ophelia\\/Ophelia - Practicum|\\/Xinyi\\/Xinyi - Practicum"), 
                        "/~data", 
                        sep = "")

base_map <- st_read("https://opendata.arcgis.com/datasets/6e3dea8bd9cf49e6a764f7baa9141a95_30.geojson")
proj <- 2246 # https://www.spatialreference.org/ref/epsg/2246/

# Read rebalance data ----
rebalance_file <- paste(data_directory, 
                        "/Louisville-MDS-Status-Changes-2019Dec17.csv",
                        sep = "")

rebalance_data <- read_csv(rebalance_file)

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
boundary <- st_union(base_map) %>% st_sf()

cell_area <- conv_unit(0.5, from = "mi2", to = "m2")
cell_size <- (cell_area * (2/3^0.5)) ^ 0.5 # the "cellsize" parameter is the distance between the centroids of each hexagonal cell.

fishnet <- st_make_grid(boundary, cellsize = cell_size, square = FALSE) %>% 
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
  xlim(0, 23)+
  theme_minimal()

dofw_plot

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

save.image(file='ophelia.RData')
