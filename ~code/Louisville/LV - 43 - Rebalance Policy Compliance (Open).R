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
