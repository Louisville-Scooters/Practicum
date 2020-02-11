##########################################################################
# This script is for formatting the trips data for 6 cities. 
# 1. Read the scooters data from the <CITY> - 01/02 code.
# 2. Set the interested month
# 3. Format the data.

# This script exports the following data:
# 1. 
##########################################################################

# Set the month
interested_month = 6 
interested_year = 2019
unified_proj = 4326

# Format trips data from Louisville ####
# LV_rebal_user_only_0619_combined_rowPairs_ct could be obtained by running code 03 and 31 in Louisville folder.
LV_rebal_user_only_0619_combined_rowPairs_ct <- readRDS(LV_rebal_user_only_0619_combined_rowPairs_ct_RDS)

LV_format_data <- LV_rebal_user_only_0619_combined_rowPairs_ct %>%
  #filter(month(start_time)==interested_month,
  #      year(start_time)==interested_year) %>%
  st_transform(unified_proj) %>%
  as.data.frame() %>%
  dplyr::select(start_time, end_time, Start.Census.Tract, End.Census.Tract, duration)
LV_format_data$city <- 'Louisville'
# Format trips data from Austin ####
AU_scooter <- readRDS(AU_scooter_RDS)
AU_format_data <- AU_scooter %>%
  filter(month(start_time)==interested_month,
         year(start_time)==interested_year) %>%
  dplyr::select(start_time, end_time, census_tract_start,census_tract_end, trip_duration) %>%
  rename(Start.Census.Tract = census_tract_start,
         End.Census.Tract = census_tract_end,
         duration = trip_duration)
AU_format_data$city <- 'Austin'
# Format trips data from Kansas City ####
KC_scooter <- readRDS(KC_scooter_RDS)
KC_format_data <- KC_scooter_ct %>%
  filter(month(start_time)==interested_month,
         year(start_time)==interested_year) %>%
  st_transform(unified_proj) %>%
  as.data.frame() %>%
  dplyr::select(start_time, end_time, Start.Census.Tract,End.Census.Tract, trip_duration) %>%
  rename(duration = trip_duration)
KC_format_data$city <- 'Kansas City'
# Format trips data from DC (They don't have data in JUNE)#### 
DC_scooter <- readRDS(DC_scooter_RDS)
DC_format_data <- DC_scooter_ct %>%
  filter(month(original_start_time)==interested_month,
         year(original_start_time)==interested_year) %>%
  st_transform(unified_proj) %>%
  as.data.frame() %>%
  dplyr::select(original_start_time, original_end_time, Start.Census.Tract,End.Census.Tract,trip_length) %>%
  rename(start_time = original_start_time,
         end_time = original_end_time,
         duration = trip_length)
DC_format_data$city <- 'Washington DC'

format_data <- rbind(LV_format_data, AU_format_data, KC_format_data)#, DC_format_data)
