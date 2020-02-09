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
LV_format_data <- LV_rebal_user_only_0619_combined_rowPairs_ct %>%
  filter(month(start_time)==interested_month,
         year(start_time)==interested_year) %>%
  st_transform(unified_proj) %>%
  as.data.frame() %>%
  dplyr::select(-vehicleID, -energy_diff, -geometry) %>%
  rename(start_longitude = lon_s,
         end_longitude = lon_d,
         start_latitude = lat_s,
         end_latitude = lat_d,
         start_location = trip_origin,
         end_location = trip_dest)

# Format trips data from Austin ####
AU_format_data <- AU_scooter %>%
  filter(month(start_time)==interested_month,
         year(start_time)==interested_year,
         vehicle_type == 3) %>%
  st_transform(unified_proj) %>%
  as.data.frame() %>%
  dplyr::select(-id, -device_id, -geometry,-month,-hour,-day_of_week,) %>%
  rename(start_longitude = lon_s,
         end_longitude = lon_d,
         start_latitude = lat_s,
         end_latitude = lat_d,
         start_location = trip_origin,
         end_location = trip_dest)
