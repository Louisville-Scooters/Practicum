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

