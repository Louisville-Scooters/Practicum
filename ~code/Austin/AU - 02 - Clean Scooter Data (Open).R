##########################################################################
# This script is for cleaning the AU scooter data
# It:
# 1. Cleans up column names
# 2. Removes unnecessary bikeshare data
# 3. Adds some different time intervals
#
# This script exports the following data:
# 1. AU_scooter
##########################################################################


# AU_scooter <- AU_scooter_raw %>% 
#   clean_names() %>% # lowercase column names and remove spaces
#   filter(vehicle_type == "scooter") %>% # remove bike data
#   mutate(interval60 = floor_date(ymd_hms(start_time), unit = "hour"),
#          week = week(interval60),
#          DoW = factor(weekdays(interval60),
#                       levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')))

AU_scooter_RDS <- file.path(data_directory, 
                            "~RData/Austin/AU_scooter")

# saveRDS(AU_scooter,
#         file = AU_scooter_RDS)

# Read the saved object with the code below
AU_scooter <- readRDS(AU_scooter_RDS)