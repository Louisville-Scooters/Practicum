##########################################################################
# This script is for preparing the LV rebalance data for analyzing the user-only records
# It:
# 1. Filters for only the rebalance records
# 2. Arranges by vehicleId and occurredAt so each row is in sequence of action for each vehicle
# 3. Use the same functions in LV-03 to calculate energy difference, trip duration and turn row pairs into a single row
#
# This script exports the following data:
# 1. LV_rebal_reb_only_combined_rowPairs - origin-destination rows for all rebalance records in the rebalance data
##########################################################################

# Filter for rebalance events, sort, and add duration and energy_diff columns
LV_rebal_rebalance_only <- LV_rebal_sf %>% 
  filter(str_detect(reason, "rebalance"))%>% 
  arrange(vehicleId, occurredAt) # sort by vehicle ID and time

# Trim the dataset to be "pick-up; drop-off" format b/c sometimes there is [reb pick up, reb pick up, reb drop off]
LV_reb_ID_list <- c()
for (veh in unique(LV_rebal_rebalance_only$vehicleId)) {
  this_vehicle_set <- LV_rebal_rebalance_only %>% filter(vehicleId == veh)
  print(veh)
  output_list <- c() # initialize list
  for (i in 1:nrow(this_vehicle_set)) {
      if (this_vehicle_set$reason[i] == 'rebalance drop off') {
        output_list <- append(output_list, c(this_vehicle_set$id[i], this_vehicle_set$id[i-1])) #store the id of rebalance drop off and rebalance pick up before it
  }}
    LV_reb_ID_list <- append(LV_reb_ID_list, output_list) #this should store all the [pick-up, drop-off] pairs
  } 


LV_rebal_rebalance_only_trim1 <- LV_rebal_rebalance_only %>%
  filter(LV_rebal_rebalance_only$id %in% LV_reb_ID_list) # Trim the data set

# Notice the dataset has some weird [pick-up, drop-off, drop-off] and some of them does not have [pick-up] info at all
# We trim the dataset one more time by the other way round
LV_reb_ID_list2 <- c()
for (veh in unique(LV_rebal_rebalance_only_trim1$vehicleId)) {
  this_vehicle_set <- LV_rebal_rebalance_only_trim1 %>% filter(vehicleId == veh)
  print(veh)
  output_list <- c() # initialize list
  for (i in 1:nrow(this_vehicle_set)) {
    if (this_vehicle_set$reason[i] == 'rebalance pick up') {
      output_list <- append(output_list, c(this_vehicle_set$id[i], this_vehicle_set$id[i+1])) #store the id of rebalance drop off and rebalance pick up before it
    }}
  LV_reb_ID_list2 <- append(LV_reb_ID_list2, output_list) #this should store all the [pick-up, drop-off] pairs
} 

# This should be our final dataset to use to generate trip origin-destination table
LV_rebal_rebalance_only_trim2 <- LV_rebal_rebalance_only_trim1 %>%
  filter(LV_rebal_rebalance_only_trim1$id %in% LV_reb_ID_list2) 

LV_rebal_reb_only_combined_rowPairs <- LV_rebal_rebalance_only_trim2 %>% 
  calc_tripDuration_and_energy()

LV_rebal_reb_only_combined_rowPairs <- LV_rebal_reb_only_combined_rowPairs%>% 
  combine_rowPairs()

ggplot(LV_rebal_reb_only_combined_rowPairs, aes(duration))+
  geom_histogram() +
  xlim(0, 5000) +
  ylim(0, 500)


# June 2019 ----
LV_rebal_reb_only_0619_combined_rowPairs  <- LV_rebal_reb_only_combined_rowPairs %>%
  filter(year(start_time) == 2019) %>%
  filter(month(start_time) == 6 |month(end_time) == 6)

# Save & Load
LV_rebal_reb_only_combined_rowPairs_RDS <- file.path(data_directory, 
                                                      "~RData/Louisville/LV_rebal_reb_only_combined_rowPairs")

LV_rebal_reb_only_0619_combined_rowPairs_RDS <- file.path(data_directory, 
                                                     "~RData/Louisville/LV_rebal_reb_only_0619_combined_rowPairs")

# saveRDS(LV_rebal_reb_only_0619_combined_rowPairs,
#                 file = LV_rebal_reb_only_0619_combined_rowPairs_RDS)
# saveRDS(LV_rebal_reb_only_combined_rowPairs,
#         file = LV_rebal_reb_only_combined_rowPairs_RDS)

# Read the saved object with the code below
LV_rebal_reb_only_combined_rowPairs <- readRDS(LV_rebal_reb_only_combined_rowPairs_RDS)
LV_rebal_reb_only_0619_combined_rowPairs <- readRDS(LV_rebal_reb_only_0619_combined_rowPairs_RDS)
