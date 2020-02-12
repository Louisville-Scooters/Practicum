##########################################################################
# This script reads in:
# 1. Louisville base map
# 2. Scooter service area
# 3. Scooter distribution areas
# 4. Scooter Open Data
# 5. Scooter rebalance data
#
# Policy:
# * Distribution Requirements
# >To ensure access to shared mobility transportation options throughout the community, Metro has established distribution zones. 
# Distribution zones are intended to ensure that no singular zone is intentionally over-served or under-served. 
# Operators must comply with distributional requirements. 
# Failure to comply with this provision constitutes a breach of the license and may result in the assessment of fleet size reductions, 
# suspension, or even termination of the license. The duration of any suspension shall be at the sole discretion of Metro but will be no less than 6 months. 
# Terminations shall apply for 1 year.
# + For operators with 150 permitted vehicles or fewer, there are no distributional requirements.
# - For operators with permitted fleets ranging in size between 150 and 350 vehicles, 20% of each operator’s vehicles must be located within zones 1 and 9.
# - Distribution plans within Zones 1 and 9 must be submitted to Metro for approval to ensure adequate accessibility for residents of each zone has been achieved.
# - For fleets ranging in size between 350 and 1050 vehicles, 20% of each operator’s vehicles must be located within zones 1 and 9 and 10% must be in zone 8.
# - Distribution plans within Zones 1, 8, and 9 must be submitted to Metro for approval to ensure adequate accessibility for residents of each zone has been achieved.
#
# Current Vehicle Limits:
# - Bird - 450 max vehicles/day - launched August 2018
# - Lime - 450 max vehicles/day - launched November 2018
# - Bolt - 150 max vehicles/day - launched July 2019
# - Spin - 150 max vehicles/day - launched August 2019
##########################################################################

# 1. Add requirements to distro areas ----
LV_distro_areas <- LV_distro_areas_raw %>% 
  mutate(Dist_Zone = as.character(Dist_Zone),
         # include the requirement for fleets between 350 and 1050. Currently the only requirements that apply
         rebal_req = case_when(Dist_Zone %in% c("1", "9") ~ 0.2,
                                 Dist_Zone == "8" ~ 0.1,
                                 TRUE ~ NA_real_)) %>% 
  st_transform(LV_proj)

# 2. Join Census tracts to distro areas
LV_Census_byDistroArea <- st_join(LV_Census_raw %>% st_centroid(.),
                                  LV_distro_areas,
                                  join = st_within) %>% 
  mutate(Dist_Zone = ifelse(GEOID == "21111000200", "1", Dist_Zone)) # manually add a district for this one

LV_Census_byDistroArea <- LV_Census_raw %>% 
  left_join(LV_Census_byDistroArea %>% st_drop_geometry() %>% dplyr::select(GEOID, Dist_Zone, Pl2040Area, rebal_req),
            by = c("GEOID"))

tm_shape(LV_Census_byDistroArea) + tm_polygons(col = "Dist_Zone")

# 3. Determine rebalance numbers for every census tract - June 2019 ----
# limit to Bird and Lime
LV_largeFleets <- c("Bird", "Lime")
LV_rebal_largeFleets_only <- LV_rebal_sf %>% 
  st_drop_geometry() %>% 
  filter(str_detect(operators, LV_largeFleets))

LV_rebal_reb_only_0619_combined_rowPairs_ct_startEnd <- LV_rebal_reb_only_0619_combined_rowPairs_ct_start %>% 
  # keep only the Bird and Lime vehicles
  filter(vehicleID %in% LV_rebal_largeFleets_only$vehicleId) %>% 
  left_join(LV_rebal_reb_only_0619_combined_rowPairs_ct_end %>% 
              st_drop_geometry() %>% 
              dplyr::select(vehicleID, start_time, end_time, End.Census.Tract),
            by = c("vehicleID", "start_time", "end_time")) %>% 
  group_by(week, Start.Census.Tract, End.Census.Tract) %>% 
  summarize(rebal_trips = n())

# Determine user trip numbers for every census tract
LV_rebal_user_only_0619_combined_rowPairs_ct_startEnd <- LV_rebal_user_only_0619_combined_rowPairs_ct_start %>% 
  # keep only the Bird and Lime vehicles
  filter(vehicleID %in% LV_rebal_largeFleets_only$vehicleId) %>% 
  left_join(LV_rebal_user_only_0619_combined_rowPairs_ct_end %>% 
              st_drop_geometry() %>% 
              dplyr::select(vehicleID, start_time, end_time, End.Census.Tract),
            by = c("vehicleID", "start_time", "end_time")) %>% 
  group_by(week, Start.Census.Tract, End.Census.Tract) %>% 
  summarize(user_trips = n())

# Join rebal and user summaries
LV_rebal_0619_combined_rowPairs_ct_startEnd <- LV_rebal_reb_only_0619_combined_rowPairs_ct_startEnd %>% 
  full_join(LV_rebal_user_only_0619_combined_rowPairs_ct_startEnd %>% st_drop_geometry(),
            by = c("week", "Start.Census.Tract", "End.Census.Tract")) %>% 
  mutate(rebal_trips = ifelse(is.na(rebal_trips), 0, rebal_trips),
         user_trips = ifelse(is.na(user_trips), 0, user_trips),
         total_trips = rebal_trips + user_trips)
  
LV_rebal_0619_combined_rowPairs_ct_outflow <- LV_rebal_0619_combined_rowPairs_ct_startEnd %>% 
  # remove rows where start and end are in the same census tract
  filter(Start.Census.Tract != End.Census.Tract) %>% 
  # add the distro areas for each tract
  left_join(LV_Census_byDistroArea %>% st_drop_geometry() %>% dplyr::select(GEOID, Dist_Zone),
            by = c("Start.Census.Tract" = "GEOID")) %>% 
  group_by(week, Dist_Zone) %>% 
  summarize(rebal_out = sum(rebal_trips),
            user_out = sum(user_trips),
            total_out = sum(total_trips))

LV_rebal_0619_combined_rowPairs_ct_inflow <- LV_rebal_0619_combined_rowPairs_ct_startEnd %>% 
  # remove rows where start and end are in the same census tract
  filter(Start.Census.Tract != End.Census.Tract) %>% 
  # add the distro areas for each tract
  left_join(LV_Census_byDistroArea %>% st_drop_geometry() %>% dplyr::select(GEOID, Dist_Zone),
            by = c("End.Census.Tract" = "GEOID")) %>% 
  group_by(week, Dist_Zone) %>% 
  summarize(rebal_in = sum(rebal_trips),
            user_in = sum(user_trips),
            total_in = sum(total_trips))
  
LV_rebal_0619_combined_rowPairs_ct_flows <- LV_rebal_0619_combined_rowPairs_ct_outflow %>% 
  left_join(LV_rebal_0619_combined_rowPairs_ct_inflow %>% st_drop_geometry(),
            by = c("week", "Dist_Zone")) %>% 
  mutate(OI = rebal_in + user_in - rebal_out,
         OI_percent = OI / sum(OI))

# Plot by week
ggplot(LV_rebal_0619_combined_rowPairs_ct_flows,
       aes(x = week,
           y = OI_percent,
           color = Dist_Zone)) +
  geom_jitter() +
  facet_wrap(~ Dist_Zone) +
  labs(title = "Proportion of Opportunity Index by Zone",
       subtitle = "Requirement: 20% of each operator’s vehicles must be located\nwithin zones 1 and 9 and 10% must be in zone 8")

# Average for the month and map
LV_rebal_0619_combined_rowPairs_ct_flows_month <- LV_rebal_0619_combined_rowPairs_ct_flows %>% 
  ungroup() %>% 
  group_by(Dist_Zone) %>% 
  summarize(OI = sum(OI, na.rm = TRUE),
            OI_percent = mean(OI_percent, na.rm = TRUE)) %>% 
  st_drop_geometry() %>% 
  right_join(LV_distro_areas, by = "Dist_Zone") %>% 
  st_as_sf()

tm_shape(LV_rebal_0619_combined_rowPairs_ct_flows_month) + tm_polygons(col = "OI_percent")

# 4. Determine rebalance numbers for every census tract - all rebalance data ----
LV_rebal_reb_only_combined_rowPairs_ct_startEnd <- LV_rebal_reb_only_combined_rowPairs_ct_start %>% 
  # keep only the Bird and Lime vehicles
  filter(vehicleID %in% LV_rebal_largeFleets_only$vehicleId) %>% 
  left_join(LV_rebal_reb_only_combined_rowPairs_ct_end %>% 
              st_drop_geometry() %>% 
              dplyr::select(vehicleID, start_time, end_time, End.Census.Tract),
            by = c("vehicleID", "start_time", "end_time")) %>% 
  group_by(week, Start.Census.Tract, End.Census.Tract) %>% 
  summarize(rebal_trips = n())

# Determine user trip numbers for every census tract
LV_rebal_user_only_combined_rowPairs_ct_startEnd <- LV_rebal_user_only_combined_rowPairs_ct_start %>% 
  # keep only the Bird and Lime vehicles
  filter(vehicleID %in% LV_rebal_largeFleets_only$vehicleId) %>% 
  left_join(LV_rebal_user_only_combined_rowPairs_ct_end %>% 
              st_drop_geometry() %>% 
              dplyr::select(vehicleID, start_time, end_time, End.Census.Tract),
            by = c("vehicleID", "start_time", "end_time")) %>% 
  group_by(week, Start.Census.Tract, End.Census.Tract) %>% 
  summarize(user_trips = n())

# Join rebal and user summaries
LV_rebal_combined_rowPairs_ct_startEnd <- LV_rebal_reb_only_combined_rowPairs_ct_startEnd %>% 
  full_join(LV_rebal_user_only_combined_rowPairs_ct_startEnd %>% st_drop_geometry(),
            by = c("week", "Start.Census.Tract", "End.Census.Tract")) %>% 
  mutate(rebal_trips = ifelse(is.na(rebal_trips), 0, rebal_trips),
         user_trips = ifelse(is.na(user_trips), 0, user_trips),
         total_trips = rebal_trips + user_trips)

LV_rebal_combined_rowPairs_ct_outflow <- LV_rebal_combined_rowPairs_ct_startEnd %>% 
  # remove rows where start and end are in the same census tract
  filter(Start.Census.Tract != End.Census.Tract) %>% 
  # add the distro areas for each tract
  left_join(LV_Census_byDistroArea %>% st_drop_geometry() %>% dplyr::select(GEOID, Dist_Zone),
            by = c("Start.Census.Tract" = "GEOID")) %>% 
  group_by(week, Dist_Zone) %>% 
  summarize(rebal_out = sum(rebal_trips),
            user_out = sum(user_trips),
            total_out = sum(total_trips))

LV_rebal_combined_rowPairs_ct_inflow <- LV_rebal_combined_rowPairs_ct_startEnd %>% 
  # remove rows where start and end are in the same census tract
  filter(Start.Census.Tract != End.Census.Tract) %>% 
  # add the distro areas for each tract
  left_join(LV_Census_byDistroArea %>% st_drop_geometry() %>% dplyr::select(GEOID, Dist_Zone),
            by = c("End.Census.Tract" = "GEOID")) %>% 
  group_by(week, Dist_Zone) %>% 
  summarize(rebal_in = sum(rebal_trips),
            user_in = sum(user_trips),
            total_in = sum(total_trips))

LV_rebal_combined_rowPairs_ct_flows <- LV_rebal_combined_rowPairs_ct_outflow %>% 
  left_join(LV_rebal_combined_rowPairs_ct_inflow %>% st_drop_geometry(),
            by = c("week", "Dist_Zone")) %>% 
  mutate(OI = rebal_in + user_in - rebal_out,
         OI_percent = OI / sum(OI))

# Plot by week
ggplot(LV_rebal_combined_rowPairs_ct_flows,
       aes(x = week,
           y = OI_percent,
           color = Dist_Zone)) +
  geom_jitter() +
  facet_wrap(~ Dist_Zone) +
  labs(title = "Proportion of Opportunity Index by Zone",
       subtitle = "Requirement: 20% of each operator’s vehicles must be located within zones 1 and 9\nand 10% must be in zone 8")

# Average for the month and map
LV_rebal_combined_rowPairs_ct_flows_month <- LV_rebal_combined_rowPairs_ct_flows %>% 
  ungroup() %>% 
  group_by(Dist_Zone) %>% 
  summarize(OI = sum(OI, na.rm = TRUE),
            OI_percent = mean(OI_percent, na.rm = TRUE)) %>% 
  st_drop_geometry() %>% 
  right_join(LV_distro_areas, by = "Dist_Zone") %>% 
  st_as_sf()

tm_shape(LV_rebal_combined_rowPairs_ct_flows_month) + tm_polygons(col = "OI_percent")




#*******************#
# Rolling Sums by Operator in each census tract ----
#*******************#

LV_rebal_user_only_combined_rowPairs
LV_rebal_reb_only_combined_rowPairs

# Associate each vehicle with an operator
LV_veh_operator_key <- LV_rebal_sf %>% 
  st_drop_geometry() %>% 
  dplyr::select(vehicleId, operators) %>% 
  group_by(vehicleId, operators) %>% 
  summarize(count = n()) # associate each vehicleId with an operator and count how many events that vehicle has

# Are there any vehicles that have more than one operator? No
LV_veh_operator_key %>% 
  ungroup() %>% 
  group_by(vehicleId) %>% 
  summarize(count = n()) %>% 
  filter(count > 1)
  
# Link operators to the combined_rowPairs objects by vehicleId
LV_rebal_user_only_combined_rowPairs_operator <- LV_rebal_user_only_combined_rowPairs %>% 
  left_join(LV_veh_operator_key %>% dplyr::select(vehicleID, operators), by = c("vehicleID" = "vehicleId")) %>% 
  mutate(reason = "user")

LV_rebal_reb_only_combined_rowPairs_operator <- LV_rebal_reb_only_combined_rowPairs %>% 
  left_join(LV_veh_operator_key, by = c("vehicleID" = "vehicleId")) %>% 
  mutate(reason = "rebalance")

# combine them and find the origin and destination rebalancing district for each row
LV_rebal_rowPairs_operator <- rbind(LV_rebal_user_only_combined_rowPairs_operator, LV_rebal_reb_only_combined_rowPairs_operator) %>% 
  st_as_sf(sf_column_name = "trip_origin") %>% 
  st_join(LV_distro_areas %>% dplyr::select(Dist_Zone)) %>% 
  as.data.frame() %>% 
  rename(Origin_Dist_Zone = Dist_Zone) %>% 
  st_as_sf(sf_column_name = "trip_dest") %>% 
  st_join(LV_distro_areas %>% dplyr::select(Dist_Zone)) %>% 
  as.data.frame() %>% 
  rename(Dest_Dist_Zone = Dist_Zone)

# 


# summarize inflows and outflows by distribution district
LV_rebal_rowPairs_operator_outflow <- LV_rebal_rowPairs_operator %>% 
  mutate(Origin_Dist_Zone = as.factor(Origin_Dist_Zone),
         outflow = ifelse(Origin_Dist_Zone != Dest_Dist_Zone, 1, 0))%>% 
  group_by(as.Date(start_time),
           Origin_Dist_Zone,
           .drop = FALSE) %>% 
  summarize(outflow = sum(outflow, na.rm = TRUE))

LV_rebal_rowPairs_operator_inflow <- LV_rebal_rowPairs_operator %>% 
  mutate(Dest_Dist_Zone = as.factor(Dest_Dist_Zone),
         inflow = ifelse(Origin_Dist_Zone != Dest_Dist_Zone, 1, 0))%>% 
  group_by(as.Date(start_time),
           Dest_Dist_Zone,
           .drop = FALSE) %>% 
  summarize(inflow = sum(inflow, na.rm = TRUE))

LV_rebal_rowPairs_operator_summary <- LV_rebal_rowPairs_operator_inflow %>% 
  left_join(LV_rebal_rowPairs_operator_outflow,
            by = c("as.Date(start_time)", "Dest_Dist_Zone" = "Origin_Dist_Zone"))
  
LV_rebal_rowPairs_operator %>% filter(Origin_Dist_Zone == Dest_Dist_Zone) %>% nrow()
LV_rebal_rowPairs_operator %>% filter(Origin_Dist_Zone != Dest_Dist_Zone) %>% nrow()
sum(LV_rebal_rowPairs_operator_summary$outflow, na.rm=T)

# 



#### TESTING - compare number of user rebalance records vs. open data records in same time period
test_rebal <- LV_rebal_raw %>% 
  filter(str_detect(reason, "user"),
         as.Date(occurredAt) <= max(LV_open_raw$StartDate))

test_open <- LV_open_raw %>% 
  filter(StartDate >= as.Date(min(test_rebal$occurredAt)))

min(test_open$StartDate)
max(test_open$StartDate)
min(test_rebal$occurredAt)
max(test_rebal$occurredAt)

nrow(test_open)
nrow(test_rebal)
table(test_rebal$reason)
nrow(test_open) - nrow(test_rebal)/2
