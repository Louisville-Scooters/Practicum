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

# Filter for rebalance events, sort, and add duration and energy_diff columns
LV_rebal_rebalance_only <- LV_rebal_sf %>% 
  filter(str_detect(reason, "rebalance"))%>% 
  arrange(vehicleId, occurredAt) # sort by vehicle ID and time

# June 2019 ----
LV_rebal_rebalance_only_0619 <- LV_rebal_rebalance_only %>%
  filter(year(occurredAt) == 2019, month(occurredAt) == 6)

# Trim the dataset to be "pick-up; drop-off" format b/c sometimes there is [reb pick up, reb pick up, reb drop off]
LV_reb_ID_list <- c()
for (veh in unique(LV_rebal_rebalance_only_0619$vehicleId)) {
  this_vehicle_set <- LV_rebal_rebalance_only_0619 %>% filter(vehicleId == veh)
  print(veh)
  output_list <- c() # initialize list
  for (i in 1:nrow(this_vehicle_set)) {
      if (this_vehicle_set$reason[i] == 'rebalance drop off') {
        output_list <- append(output_list, c(this_vehicle_set$id[i], this_vehicle_set$id[i-1])) #store the id of rebalance drop off and rebalance pick up before it
  }}
    LV_reb_ID_list <- append(LV_reb_ID_list, output_list) #this should store all the [pick-up, drop-off] pairs
  } 


LV_rebal_rebalance_only_0619_trim1 <- LV_rebal_rebalance_only_0619 %>%
  filter(LV_rebal_rebalance_only_0619$id %in% LV_reb_ID_list) # Trim the data set

# Notice the dataset has some weird [pick-up, drop-off, drop-off] and some of them does not have [pick-up] info at all
# We trim the dataset one more time by the other way round
LV_reb_ID_list2 <- c()
for (veh in unique(LV_rebal_rebalance_only_0619_trim1$vehicleId)) {
  this_vehicle_set <- LV_rebal_rebalance_only_0619_trim1 %>% filter(vehicleId == veh)
  print(veh)
  output_list <- c() # initialize list
  for (i in 1:nrow(this_vehicle_set)) {
    if (this_vehicle_set$reason[i] == 'rebalance pick up') {
      output_list <- append(output_list, c(this_vehicle_set$id[i], this_vehicle_set$id[i+1])) #store the id of rebalance drop off and rebalance pick up before it
    }}
  LV_reb_ID_list2 <- append(LV_reb_ID_list2, output_list) #this should store all the [pick-up, drop-off] pairs
} 

# This should be our final dataset to use to generate trip origin-destination table
LV_rebal_rebalance_only_0619_trim2 <- LV_rebal_rebalance_only_0619_trim1 %>%
  filter(LV_rebal_rebalance_only_0619_trim1$id %in% LV_reb_ID_list2) 

