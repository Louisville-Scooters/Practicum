library(tidyverse)
library(sf)
library(lubridate)
library(tigris)
library(tidycensus)
library(viridis)
library(riem)
library(gridExtra)
library(knitr)
library(kableExtra)
library(RSocrata)
library(dplyr)
library(rjson)
library(here)
library(stringr)

#read data
data_directory <- paste(str_remove(here(), "\\/Eugene\\/Eugene - Practicum|\\/Ophelia\\/Ophelia - Practicum|\\/Xinyi\\/Xinyi - Practicum"), 
                        "/~data", 
                        sep = "")

rebalance_file <- paste(data_directory, 
                        "/Louisville-MDS-Status-Changes-2019Dec17.csv",
                        sep = "")
rebalance_data <- read_csv(rebalance_file)

#extract coordination
rebalance_data[c("lon", "lat")] <- do.call(rbind, 
                                          lapply(strsplit(rebalance_data$location, "[()]"), 
                                                 function(col) {   
                                                          (parts <- unlist(strsplit(col[2], " ")))
                                                   }
                                                 )
                                          )

rebalance_data$lon <- as.numeric(rebalance_data$lon)
rebalance_data$lat <- as.numeric(rebalance_data$lat)
#csv file to sf
rebalance_data_sf <- st_as_sf(rebalance_data,coords = c('lon','lat'),crs=4326)
#service area
sa_file <- paste(data_directory, 
                        "/Dockless Vehicle Service Area/Dockless_Vehicle_Service_Area.shp",
                        sep = "")
sa <- studyArea <- read_sf(sa_file)

#focus on JUNE
rebalance_june <- subset(rebalance_data_sf, year(rebalance_data_sf$occurredAt)==2019 &&
                           month(rebalance_data_sf$occurredAt)==6)

#remove trips outsides the service area
rebalance_june_sa <- st_intersection(rebalance_june,sa %>% st_transform(crs=4326))
data$year <- year(data$occurredAt)
data$month <- month(data$occurredAt)
ggplot()+
  geom_sf(data = subset(data, (data$year==2019) && (data$month==5)))+
  geom_sf(data=sa %>% st_transform(crs=4326), fill='transparent')
#boundary data


rebalancing <- subset(rebalance_data, startsWith(rebalance_data$reason,'rebalance'))
users <- subset(rebalance_data, startsWith(rebalance_data$reason,'user'))

#plot the frequency of rebalancing for each scooter by month and year
rps <- rebalancing %>%
  group_by(year(occurredAt),month(occurredAt)) %>%
  summarise(cnt=n(), per=cnt/length(unique(vehicleId)))

names(rps)[1] <- 'year'
names(rps)[2] <- 'month'
rps$ym <- paste(rps$year,rps$month)
