##########################################################################
# This script is for preparing the LV rebalance data for analyzing the user-only records
# It:
# 1. Filters for only the user records
# 2. Arranges by vehicleId and occurredAt so each row is in sequence of action for each vehicle
# 3. Creates a helper function for calculating the duration and energy usage of each trip
# 4. Creates a helper function for turning row-pairs (i.e., a pickup and dropoff row) into a single row
#
# This script exports the following data:
# 1. LV_rebal_user_only_combined_rowPairs - origin-destination rows for all user records in the rebalance data
##########################################################################


# Filter for user events, sort, and add duration and energy_diff columns
LV_rebal_user_only <- LV_rebal_sf %>% 
  filter(str_detect(reason, "user"))%>% 
  arrange(vehicleId, occurredAt) # sort by vehicle ID and time

### Helper Functions
# Define function for calcuating trip durations and energy levels ----
calc_tripDuration_and_energy <- function(x) {
  
  output <- list() # initialize list
  
  for (veh in unique(x$vehicleId)) { # for each unique vehicle
    this_vehicle_set <- x %>% filter(vehicleId == veh) # filter for that vehicle
    print(veh) # print so we can see the progress of the loop
    
    for (i in 1:nrow(this_vehicle_set)) { # for each row of this vehicle
      if (i%%2 == 1) { # if this is an odd number row
        
        # the trip duration is the time for the next row minus the time for this row
        this_vehicle_set$duration[i] <- difftime(this_vehicle_set$occurredAt[i+1], this_vehicle_set$occurredAt[i], units = 'mins')
        
        # same with energy level
        this_vehicle_set$energy_diff[i] <- this_vehicle_set$vehicleEnergyLevel[i+1]- this_vehicle_set$vehicleEnergyLevel[i] 
        } else {}
    }
    output[[veh]] <- this_vehicle_set
  }
  
  as.data.frame(data.table::rbindlist(output,
                                      idcol = "vehicleId"))
  
}

# One with a start and end time and start and end location ----
combine_rowPairs <- function(x) { # x should be the output of calc_tripDuration_and_energy()
  
  temp <- data.frame("vehicleID" = c(0), # initialize temp dataframe with columns
                     "start_time" = c(0), 
                     "end_time" = c(0), 
                     "trip_origin" = c(0), 
                     "trip_dest" = c(0), 
                     "duration" = c(0), 
                     "energy_diff" = c(0))
  
  output <- list() # initialize output list
  
  for (i in seq(nrow(x) - 1)) { 
    # need to do minus 1 b/c if there is an odd number of rows, the last row of the trip_dest column will be empty
    # and cannot be binded with the other rows
    if (i%%2 == 1) {
      
      print(i)

      temp$vehicleID = x$vehicleId[i]
      temp$start_time = x$occurredAt[i]
      temp$end_time = x$occurredAt[i+1]
      temp$trip_origin = x$location[i]
      temp$trip_dest = x$location[i+1]
      temp$duration = x$duration[i]
      temp$energy_diff = x$energy_diff[i]
      
      output[[i]] <- temp
      
    } else {}
  }
  
  output <- as.data.frame(data.table::rbindlist(output))
  
}

# June 2019 ----
# LV_rebal_user_only_0619 <- LV_rebal_user_only %>%
#   filter(year(occurredAt) == 2019, month(occurredAt) == 6) 
# 
# LV_rebal_user_only_0619_combined_rowPairs <- LV_rebal_user_only_0619 %>% 
#   calc_tripDuration_and_energy()
# 
# LV_rebal_user_only_0619_combined_rowPairs <- LV_rebal_user_only_0619_combined_rowPairs %>% 
#   combine_rowPairs()
# 
# ggplot(LV_rebal_user_only_0619_combined_rowPairs, aes(duration))+
#   geom_histogram() +
#   xlim(0, 5000) +
#   ylim(0, 250)

# All user data ----
# LV_rebal_user_only_combined_rowPairs <- LV_rebal_user_only %>%
#   calc_tripDuration_and_energy() %>%
#   combine_rowPairs()

LV_rebal_user_only_combined_rowPairs_RDS <- file.path(data_directory, 
                                                      "~RData/Louisville/LV_rebal_user_only_combined_rowPairs")

# saveRDS(LV_rebal_user_only_combined_rowPairs,
#         file = LV_rebal_user_only_combined_rowPairs_RDS)

LV_rebal_user_only_0619_combined_rowPairs_RDS <- file.path(data_directory, 
                                                      "~RData/Louisville/LV_rebal_user_only_0619_combined_rowPairs")

# saveRDS(LV_rebal_user_only_0619_combined_rowPairs,
#         file = LV_rebal_user_only_0619_combined_rowPairs_RDS)

# Read the saved object with the code below
LV_rebal_user_only_combined_rowPairs <- readRDS(LV_rebal_user_only_combined_rowPairs_RDS)
LV_rebal_user_only_0619_combined_rowPairs <- readRDS(LV_rebal_user_only_0619_combined_rowPairs_RDS)

##### TESTING ----
# LV_rebal_user_only_test <- LV_rebal_user_only %>% 
#   mutate(month = month(occurredAt),
#          year = year(occurredAt))
# 
# LV_count_dupes <- LV_rebal_user_only_test %>% 
#   mutate(duplicate = ifelse(lag(reason, 1) == reason & lag(vehicleId, 1) == vehicleId, 
#                             TRUE, 
#                             FALSE))
# 
# LV_dupe_summary <- LV_count_dupes %>% 
#   as.data.frame() %>% 
#   group_by(year, month) %>% 
#   summarize(dupe_count = sum(duplicate, na.rm = TRUE),
#             row_count = n(),
#             dupe_percentage = dupe_count / row_count)
##### END TESTING