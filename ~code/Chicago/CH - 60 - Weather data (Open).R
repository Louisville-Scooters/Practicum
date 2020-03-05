##########################################################################
# This script is for quering weather data for CH
# It reads in: 
# 1. CH_spatial_census

# It exports:
# 1. CH_spatial_census_weather


library(riem)

CH_weather.Data <- 
  riem_measures(station = "ORD", date_start = "2019-07-01", date_end = "2019-10-01")

CH_weather.Data

CH_weather.Panel <-  
  CH_weather.Data %>%
  replace(is.na(.), 0) %>%
  mutate(interval60 = ymd_h(substr(valid,1,13))) %>%
  mutate(week = week(interval60),
         dotw = wday(interval60)) %>%
  group_by(interval60) %>%
  summarize(Temperature = max(tmpf),
            Percipitation = sum(p01i),
            Wind_Speed = max(sknt)) %>%
  mutate(Temperature = ifelse(Temperature == 0, 42, Temperature))

remove(CH_weather_bymonth)

CH_weather_average <-
  CH_weather.Panel %>%
  summarize(avg_temp = mean(Temperature),
            avt_rain = mean(Percipitation),
            avg_wind = mean(Wind_Speed)) %>%
  mutate(city = "Chicago")

CH_spatial_census_weather <- merge(CH_spatial_census, CH_weather_average, by = 'city')

CH_spatial_census_RDS <- file.path(data_directory, "~RData/Chicago/CH_spatial_census")
saveRDS(CH_spatial_census,
        file = CH_spatial_census_RDS)
CH_spatial_census <- readRDS(CH_spatial_census_RDS)
