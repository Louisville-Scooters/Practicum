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
# AU_scooter_raw <- AU_scooter_raw[-1,] # na row
# AU_scooter_raw <- AU_scooter_raw[-1,] # problematic row
# AU_scooter_raw <- dplyr::select(AU_scooter_raw,-`Modified Date`) # NA COLUMN
# # format time columns
# AU_scooter_raw$`Start Time` <- as.POSIXct(AU_scooter_raw$`Start Time`, format='%m/%d/%Y %I:%M:%S %p')
# AU_scooter_raw$`End Time` <- as.POSIXct(AU_scooter_raw$`End Time`, format='%m/%d/%Y %I:%M:%S %p')
# 
# AU_scooter <- AU_scooter_raw %>%
#   clean_names() %>% # lowercase column names and remove spaces
#   filter(vehicle_type == "scooter") # remove bike data
#   
# AU_scooter_RDS <- file.path(data_directory,
#                             "~RData/Austin/AU_scooter")

 # saveRDS(AU_scooter,
 #         file = AU_scooter_RDS)

# Read the saved object with the code below
# AU_scooter <- readRDS(AU_scooter_RDS)
# 
# # glimpse(AU_scooter)
# 
# AU_scooter_07to09 <- AU_scooter %>%
#   filter(month %in% c(7, 8, 9) & year == 2019)

AU_scooter_07to09_RDS <- file.path(data_directory,
                            "~RData/Austin/AU_scooter_07to09")

# saveRDS(AU_scooter_07to09,
#         file = AU_scooter_07to09_RDS)

AU_scooter_07to09 <- readRDS(AU_scooter_07to09_RDS)

AU_open_origins_ct <- AU_scooter_07to09 %>% 
  group_by(census_tract_start) %>%
  summarize(origins_ct = n()/3)