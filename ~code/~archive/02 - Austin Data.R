library(dplyr)
library(sp)
library(tidyverse)
library(sf)
library(tidycensus)
library(lubridate)
library(here)
options(scipen=999)

data_directory <- paste(str_remove(here(), "\\/Eugene\\/Eugene - Practicum|\\/Ophelia\\/Ophelia - Practicum|\\/Xinyi\\/Xinyi - Practicum"), 
                        "/~data", 
                        sep = "")


# Read Austin scooter data (2019) ####
# Austin_file <- paste(data_directory, 
#                      "/Austin.csv",
#                      sep = "")

Austin_file <- paste(data_directory,
                     "/Shared_Micromobility_Vehicle_Trips_austin.csv",
                     sep = "")

dat_Aus <- read_csv(Austin_file)

dat_Aus_2019 <- dat_Aus %>%
  na.omit() %>%
  subset((Year == 2019) &
         (`Vehicle Type` == 'scooter'))

dat_Aus_2019_june <- dat_Aus_2019 %>%
  subset(Month == 6)

# remove outlier, we focus on trips made in Travis county because only 19 trips started outsides the county.
dat_Aus_2019_june$`Census Tract Start` <- as.character(dat_Aus_2019_june$`Census Tract Start`)
dat_Aus_2019_june$`Census Tract End` <- as.character(dat_Aus_2019_june$`Census Tract End`)
dat_Aus_2019_june_start <- merge(dat_Aus_2019_june, ASTTracts, all.x=F, by.x='Census Tract Start', by.y='GEOID')
#dat_Aus_2019_june_end <- merge(dat_Aus_2019_june, ASTTracts, all.x=T, by.x='Census Tract End', by.y='GEOID')

#- Add field: time interval-####
dat_Aus_2019_june_start$`Start Time` <- as.POSIXct(dat_Aus_2019_june_start$`Start Time`, format='%m/%d/%Y %I:%M:%S %p')
#dat_Aus_2019_june_end$`End Time` <- as.POSIXct(dat_Aus_2019_june_end$`End Time`, format='%m/%d/%Y %I:%M:%S %p')
dat_Aus_2019_june_start <- dat_Aus_2019_june_start %>%
  mutate(interval60 = floor_date(ymd_hms(`Start Time`), unit = "hour"),
         #interval15 = floor_date(ymd_hms(starttime), unit = "15 mins"),
         week = week(interval60))
dat_Aus_2019_june_start$`Day of Week` <- weekdays(dat_Aus_2019_june_start$interval60)
dat_Aus_2019_june_start$`Day of Week` <- factor(dat_Aus_2019_june_start$`Day of Week`, level=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))

#- time pattern-####
# by hour
ggplot(dat_Aus_2019_june_start %>%
         group_by(interval60) %>%
         tally())+
  geom_line(aes(x = interval60, y = n, group=1),size=.7)+
  labs(title="Scooter trips per hr. Austin, June, 2019",
       x="Date", 
       y="Number of trips")+
  plotTheme
# by day of week
ggplot(data=dat_Aus_2019_june_start) +
  geom_freqpoly(aes(Hour, col=`Day of Week`), binwidth=1) +
  labs(title="Scooter trips in Austin by hour, by day of the week, June, 2019",
       x="Hour", 
       y="Trip Counts")+
  scale_color_viridis_d(direction = -1,
                      option = "D")+
  plotTheme

#- Scooters turnover-#### 
sctTov_austin_dow <- dat_Aus_2019_june_start %>%
  group_by(week,`Day of Week`) %>%
  summarise(cnt=n(), turnover=cnt/length(unique(`Device ID`)))

sctTov_austin_dow <- sctTov_austin_dow %>%
  group_by(`Day of Week`)%>%
  summarise(turnover_m=mean(turnover))

ggplot(data=sctTov_austin_dow, aes(`Day of Week`, turnover_m, group=1))+
  geom_line(size=1) +
  labs(title="Turnover rate of scooter in Austin by day of the week, June, 2019",
       x="Hour", 
       y="Trip Counts")+
  plotTheme

sctTov_austin_hour <- dat_Aus_2019_june_start %>%
  group_by(Hour) %>%
  summarise(cnt=n(), turnover=cnt/length(unique(`Device ID`))/length(unique(date(interval60))))

ggplot(data=sctTov_austin_hour, aes(Hour, turnover, group=1))+
  geom_line(size=1) +
  labs(title="Turnover rate of scooter in Austin by day of the week, June, 2019",
       x="Hour", 
       y="Turnoverrate")+
  plotTheme


# Add geometry information to scooter trips data


# Read census tract data
ASTCensus <- 
  get_acs(geography = "tract", 
          variables = c("B01003_001", "B19013_001", 
                        "B02001_002", "B08013_001",
                        "B08012_001", "B08301_001", 
                        "B08301_010", "B01002_001"), 
          year = 2017, 
          state = "TX", 
          geometry = TRUE, 
          county=c("Travis"),
          output = "wide") %>%
  rename(Total_Pop =  B01003_001E,
         Med_Inc = B19013_001E,
         Med_Age = B01002_001E,
         White_Pop = B02001_002E,
         Travel_Time = B08013_001E,
         Num_Commuters = B08012_001E,
         Means_of_Transport = B08301_001E,
         Total_Public_Trans = B08301_010E) %>%
  select(Total_Pop, Med_Inc, White_Pop, Travel_Time,
         Means_of_Transport, Total_Public_Trans,
         Med_Age,
         GEOID, geometry) %>%
  mutate(Percent_White = White_Pop / Total_Pop,
         Mean_Commute_Time = Travel_Time / Total_Public_Trans,
         Percent_Taking_Public_Trans = Total_Public_Trans / Means_of_Transport)

ASTTracts <- 
  ASTCensus %>%
  as.data.frame() %>%
  distinct(GEOID, .keep_all = TRUE) %>%
  select(GEOID, geometry) %>% 
  st_sf

ggplot()+
  geom_sf(data = dat_Aus_2019_june)+
  geom_sf(data=ASTTracts %>% st_transform(crs=4326), fill='transparent')


# Read census data ####
census_api_key("6c5e126bca08a1884a7a500d88db30a106f77665", overwrite = TRUE)
readRenviron("~/.Renviron")

acs_variable_list.2017 <- load_variables(2017, #year
                                         "acs5", #five year ACS estimates
                                         cache = TRUE)

acs_vars <- c("B19013_001E", # median household income 
              "B15003_001E", #edu total 25+, for calculation
              "B15003_022E", #bachelor degree
              "B15003_023E", #master degree 
              "B15003_024E", #professional school degree
              "B15003_025E", #doctorate degree
              "B02001_001E", #race total, for calculation
              "B02001_002E", #white alone
              "B25002_001E", #total housing unit
              "B25002_003E", #vacant unit
              "B25024_001E", #structure total, for calculation
              "B25024_002E", #1, detached
              "B25024_003E", #1, attached
              "B01001_001E", #total population
              "B08014_001E", #veh available, total (for calculation)
              "B08014_002E", #no veh available
              "B08014_003E", #1 veh
              "B08014_004E", #2 veh
              "B08014_005E", #3veh
              "B08014_006E", #4 veh
              "B08014_007E", #5 or more veh
              "B08006_001E", #means of trans, total (for calculation)
              "B08006_002E", #car
              "B08006_008E", #public transit
              "B08006_014E", #bike
              "B08006_015E", #walk
              "B08006_016E", #other
              "B08006_017E" #work from home
) 

acstract.2017 <- get_acs(geography = "tract", # What is the lowest level of geography are we interested in?
                         year = 2017, # What year do we want - this can also be used for 2000 census data
                         variables = acs_vars, # let's use our variables we specified above
                         geometry = TRUE, # Do we want this as a shapefile? No, not now.
                         state = c("TX"), # What states?
                         county = c('Travis'),
                         output = "wide") # get a "wide" data type

acstract.2017 <- acstract.2017 %>%
  select (GEOID, NAME, acs_vars) %>%
  rename (MedHHinc = B19013_001E,
          EduTOT = B15003_001E,
          EduBach = B15003_022E,
          EduMas = B15003_023E,
          EduPro = B15003_024E,
          EduDoc = B15003_025E,
          RaceTOT = B02001_001E,
          RaceWhite = B02001_002E,
          HUtot = B25002_001E,
          HUvac = B25002_003E,
          StruTOT = B25024_001E,
          Stru1d = B25024_002E,
          Stru1a = B25024_003E,
          Population = B01001_001E, #total population
          VehTOT = B08014_001E, #veh available, total (for calculation)
          Veh0 = B08014_002E, #no veh available
          Veh1 = B08014_003E, #1 veh
          Veh2 = B08014_004E, #2 veh
          Veh3 = B08014_005E, #3veh
          Veh4 = B08014_006E, #4 veh
          Veh5pls = B08014_007E, #5 or more veh
          MeanTOT = B08006_001E, #means of trans, total (for calculation)
          Mean_drive = B08006_002E, #car
          Mean_pub = B08006_008E, #public transit
          Mean_bike = B08006_014E, #bike
          Mean_walk = B08006_015E, #walk
          Mean_other = B08006_016E, #other
          Mean_athome = B08006_017E #work from home
  ) %>%
  mutate(VacPct = HUvac/HUtot,
         WhitePct = RaceWhite/RaceTOT,
         AboveBach = (EduDoc + EduPro + EduMas + EduBach)/EduTOT,
         Unit_single = (Stru1a + Stru1d)/StruTOT,
         Veh_no = Veh0/VehTOT,
         Veh_one = Veh1/VehTOT,
         Veh_twomore = (Veh2 + Veh3 + Veh4 + Veh5pls)/VehTOT,
         Pct_drive = Mean_drive/MeanTOT,
         Pct_pub = Mean_pub/MeanTOT,
         Pct_bike = Mean_bike/MeanTOT,
         Pct_walk = Mean_walk/MeanTOT,
         Pct_other = Mean_other/MeanTOT
  )

acstract2017_sub <- acstract.2017 %>%
  select(HUtot, GEOID, Population, MedHHinc, VacPct, WhitePct, AboveBach, Unit_single, Veh_no, Veh_one,
         Veh_twomore, Pct_drive, Pct_pub, Pct_bike, Pct_walk, Pct_other)

acstract2017_sub <- acstract2017_sub %>%
  st_set_crs(4326) %>%
  st_transform(32140)

ggplot() + geom_sf(data = acstract2017_sub)
