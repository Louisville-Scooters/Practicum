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

#setup
plotTheme <- theme(
  plot.title =element_text(size=15),
  plot.subtitle = element_text(size=8),
  plot.caption = element_text(size = 6),
  axis.text.x = element_text(size = 10, hjust = 1),
  axis.text.y = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  # Set the entire chart region to blank
  panel.background=element_blank(),
  plot.background=element_blank(),
  #panel.border=element_rect(colour="#F0F0F0"),
  # Format the grid
  panel.grid.major=element_line(colour="#D0D0D0",size=.2),
  axis.ticks=element_blank())
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

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
#spelling mistake
rebalance_data$operators[rebalance_data$operators=='Bolt Lousiville']='Bolt Louisville'
#csv file to sf
rebalance_data_sf <- st_as_sf(rebalance_data,coords = c('lon','lat'),crs=4326)
#service area
sa_file <- paste(data_directory, 
                        "/Dockless Vehicle Service Area/Dockless_Vehicle_Service_Area.shp",
                        sep = "")
sa <- studyArea <- read_sf(sa_file)

#trips made in June
rebalance_data_sf$year <- year(rebalance_data_sf$occurredAt)
rebalance_data_sf$month <- month(rebalance_data_sf$occurredAt)
rebalance_june <- subset(rebalance_data_sf, (rebalance_data_sf$year==2019) &
                           (rebalance_data_sf$month==6))

#remove trips outsides the service area
rebalance_june_sa <- st_intersection(rebalance_june,sa %>% st_transform(crs=4326))

ggplot()+
  geom_sf(data = rebalance_june_sa)+
  geom_sf(data=sa %>% st_transform(crs=4326), fill='transparent')


#frequency of rebalancing for each scooter
rebalancing <- subset(rebalance_data, startsWith(rebalance_data$reason,'rebalance'))
users <- subset(rebalance_data, startsWith(rebalance_data$reason,'user'))

#plot the frequency of rebalancing for each scooter by month and year
rps <- rebalancing %>%
  group_by(year(occurredAt),month(occurredAt)) %>%
  summarise(cnt=n(), per=cnt/length(unique(vehicleId)))

names(rps)[1] <- 'year'
names(rps)[2] <- 'month'
rps$month <- ifelse(rps$month<=9,paste('0', rps$month, sep=''), rps$month)
rps$date <- paste(rps$year,rps$month,sep = '')

ggplot(data=subset(rps, rps$year==2019), aes(date, per, group=1))+
  geom_line(size=1) +
  plotTheme
  
#by day of week in June
rps_dow <- rebalancing %>%
  #subset((year(rebalancing$occurredAt)==2019) & (month(rebalancing$occurredAt)==7)) %>%
  group_by(month(occurredAt),weekdays(occurredAt)) %>%
  summarise(cnt=n(), per=cnt/length(unique(vehicleId)))
names(rps_dow)[2] <- 'weekdays'

rps_dow <- rps_dow %>%
  group_by(weekdays)%>%
  summarise(perd=mean(per))

rps_dow$weekdays <- factor(rps_dow$weekdays, level=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
ggplot(data=rps_dow, aes(weekdays, perd, group=1))+
  geom_line(size=1) +
  plotTheme

#which company contribute more in rebalancing
contribution <- as.data.frame(prop.table(table(rebalancing$operators)))
names(contribution)[1] <- 'Provider'
names(contribution)[2] <- 'Propertion'
contribution$Provider <- as.character(contribution$Provider)
contribution <- rbind(contribution,c('Bird Louisville',0))
contribution <- rbind(contribution,c('Spin Louisville',0))
contribution$Propertion <- round(as.numeric(contribution$Propertion),2)

contribution %>% 
  kable(caption = "Contribution by providers") %>%
  kable_styling("striped", full_width = F,position = "center") %>%
  row_spec(2, color = "white", background = "#33638dff") %>%
  row_spec(4, color = "white", background = "#33638dff")
