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
         outflow = ifelse(Origin_Dist_Zone != Dest_Dist_Zone, 1, 0),
         in_zone = ifelse(Origin_Dist_Zone == Dest_Dist_Zone, 1, 0)) %>% 
  group_by(as.Date(start_time),
           Origin_Dist_Zone,
           .drop = FALSE) %>% 
  summarize(outflow = sum(outflow, na.rm = TRUE),
            in_zone = sum(in_zone, na.rm = TRUE))

LV_rebal_rowPairs_operator_inflow <- LV_rebal_rowPairs_operator %>% 
  mutate(Dest_Dist_Zone = as.factor(Dest_Dist_Zone),
         inflow = ifelse(Origin_Dist_Zone != Dest_Dist_Zone, 1, 0))%>% 
  group_by(as.Date(start_time),
           Dest_Dist_Zone,
           .drop = FALSE) %>% 
  summarize(inflow = sum(inflow, na.rm = TRUE))

LV_rebal_rowPairs_operator_summary <- LV_rebal_rowPairs_operator_inflow %>% 
  left_join(LV_rebal_rowPairs_operator_outflow,
            by = c("as.Date(start_time)", "Dest_Dist_Zone" = "Origin_Dist_Zone")) %>% 
  mutate(total_vehicles = inflow + in_zone - outflow)
  
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

lime <- LV_rebal_raw %>% 
  filter(operators == "Lime Louisville",
         month(occurredAt) == 6,
         year(occurredAt) == 2019,
         day(occurredAt) == 8)

length(unique(lime$vehicleId))


########################## NESTED LIST APPROACH ----
unique(LV_rebal_sf$reason) 
# [1] "user pick up"         "maintenance"          "rebalance pick up"    "user drop off"        
# "maintenance pick up"  "rebalance drop off"   "service end"          "service start"        "maintenance drop off"
# [10] "low battery" 

LV_active_status <- c("user drop off",
                      "rebalance drop off",
                      "maintenance drop off",
                      "service start")

LV_reserved_status <- c("user pick up")

LV_inactive_status <- c("rebalance pick up",
                        "maintenance pick up",
                        "service end",
                        "low battery",
                        "maintenance")

# Function for finding the last status for every scooter 
LV_extract_latest_status <- function(x, # list of scooter dataframes
                                     datetime, # format = "YYYY-MM-DD HH:MM"
                                     # hour, # format = "HH:MM" in 24 hour time
                                     buffer # of days since last activity that we're willing to consider a scooter still active. 
){
  
  time <- as.POSIXct(datetime)
  
  tmp <-  x %>% 
    as.data.frame() %>% 
    dplyr::select(vehicleId, occurredAt, reason, operators, location, long, lat) %>% 
    filter(occurredAt <= time) %>% # can we replace this with the buffer call below?
    arrange(occurredAt) %>% 
    tail(1) %>% 
    filter(as.numeric(time - occurredAt) <= buffer,
           reason %in% LV_active_status) # should this also include user pickups?
  
  if(nrow(tmp) > 0) {
    output <- tmp %>% 
      mutate(Date = as.Date(occurredAt),
             Hour = hour(occurredAt),
             active = 1) %>% 
      dplyr::select(vehicleId, Date, Hour, operators, active, long, lat)
    
  } else {
    
    output <- data.frame(vehicleId = x$vehicleId[1],
                         Date = as.Date(time),
                         Hour = hour(time),
                         operators = x$operators[1],
                         active = 0,
                         long = NA_real_,
                         lat = NA_real_,
                         stringsAsFactors = FALSE)
  }
  
  output
  
}



# Find latest status for every scooter
time_intervals <- seq(from = as.POSIXct("2018-11-15 12:00:00 EDT"), 
                      to = as.POSIXct("2019-12-15 12:00:00 EDT"),
                      by = "1 month")

# loop over dates
library(sf)
library(tidyverse)
library(lubridate)
library(microbenchmark)

LV_rebal_sf_sample <- readRDS("~archive/LV_rebal_sf_sample")

microbenchmark(map(time_intervals,
                   function(x){LV_rebal_sf_sample %>% 
                       mutate(long = st_coordinates(.)[,1],
                              lat = st_coordinates(.)[,2]) %>% 
                       as.data.frame() %>% 
                       split(.$vehicleId) %>% 
                       map(., 
                           function(y){LV_extract_latest_status(y,
                                                                datetime = x,
                                                                # hour = "12:00",
                                                                buffer = 10)}) %>%
                       bind_rows() %>% 
                       mutate(audit_date = x)}) %>%
                 bind_rows(),
               times = 10)


LV_rebal_sf_list <- map(time_intervals,
                        function(x){LV_rebal_sf_sample %>% 
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>% 
  as.data.frame() %>% 
  split(.$vehicleId) %>% 
  map(., 
      function(y){LV_extract_latest_status(y,
                                           datetime = x,
                                           # hour = "12:00",
                                           buffer = 10)}) %>%
    bind_rows() %>% 
    mutate(audit_date = x)}) %>%
  bind_rows()

LV_rebal_sf_list_RDS <- file.path(data_directory, 
                             "~RData/Louisville/LV_rebal_sf_list")

# saveRDS(LV_rebal_sf_list,
#         file = LV_rebal_sf_list_RDS)

# Read the saved object with the code below
LV_rebal_sf_list <- readRDS(LV_rebal_sf_list_RDS)


# no loop over dates
# LV_rebal_sf_list <- LV_rebal_sf %>% 
#                           mutate(long = st_coordinates(.)[,1],
#                                  lat = st_coordinates(.)[,2]) %>% 
#                           as.data.frame() %>% 
#                           split(.$vehicleId) %>% 
#                           map(., 
#                               LV_extract_latest_status,
#                               datetime = "2019-10-01 12:00:00 EDT",
#                               # hour = "12:00",
#                               buffer = 10) %>% 
#                           bind_rows() %>% 
#                           mutate(audit_date = as.Date("2019-10-01 12:00:00 EDT"))

# Add region for every active scooter
LV_rebal_sf_list_2 <- LV_rebal_sf_list %>% 
  filter(!is.na(long),
         !is.na(lat)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = LV_proj, remove = FALSE) %>% 
  st_join(., LV_distro_areas %>% dplyr::select(Dist_Zone)) %>% 
  st_drop_geometry() %>% 
  mutate(Dist_Zone = factor(Dist_Zone,
                            levels = paste(1:9)))

LV_rebal_sf_list_summary <- LV_rebal_sf_list %>% 
  left_join(LV_rebal_sf_list_2 %>% dplyr::select(vehicleId, Dist_Zone, audit_date), by = c("vehicleId", "audit_date")) %>% 
  group_by(audit_date, Dist_Zone, operators, .drop = FALSE) %>% 
  summarize(scooters = n()) %>% 
  filter(str_detect(operators, "Bird|Lime"),
         !is.na(Dist_Zone)) %>%
  ungroup() %>%
  group_by(audit_date, operators) %>%
  mutate(scooter_total = sum(scooters),
         scooter_pct = scooters / scooter_total)

LV_rebal_sf_list_summary_2 <- LV_rebal_sf_list_summary %>% 
  dplyr::select(-scooter_pct) %>% 
  spread(Dist_Zone, scooters, sep = "_") %>% 
  mutate(Dist_8_pct = ifelse(is.na(Dist_Zone_8 / scooter_total), 0, Dist_Zone_8 / scooter_total), 
         Dist_1_9_pct = ifelse(is.na((Dist_Zone_1 + Dist_Zone_9) / scooter_total), 0, (Dist_Zone_1 + Dist_Zone_9) / scooter_total),
         compliance = case_when(scooter_total > 150 & Dist_1_9_pct < 0.2 ~ "No",
                                scooter_total > 350 & (Dist_1_9_pct < 0.2 | Dist_8_pct < 0.1) ~ "No",
                                TRUE ~ "Yes"))



# Look at Bird and Lime
# LV_rebal_audit <- LV_rebal_sf_list_summary %>% 
#   dplyr::select(audit_date, Dist_Zone, `Bird Louisville`, `Lime Louisville`) %>% 
#   ungroup() %>% 
#   group_by(audit_date) %>% 
#   mutate(Bird_total = sum(`Bird Louisville`),
#          Lime_total = sum(`Lime Louisville`),
#          Bird_pct = `Bird Louisville` / Bird_total,
#          Lime_pct = `Lime Louisville` / Lime_total)

# Zone 1 and 9
# LV_zone1_9 <- ggplot(LV_rebal_sf_list_summary %>% filter(Dist_Zone %in% c("1", "9")),
#        aes(x = Dist_Zone,
#            y = scooter_pct,
#            fill = operators)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_hline(yintercept = 0.2, color = "red", size = 2) +
#   facet_wrap(audit_date~operators) +
#   plotTheme +
#   labs(title = "Zones 1 and 9 - Allocation of Available Scooters",
#        subtitle = "10% of available scooters must be in Zones 1 and 9.")
# 
# LV_zone8 <- ggplot(LV_rebal_sf_list_summary %>% filter(Dist_Zone %in% c("8")),
#                      aes(x = Dist_Zone,
#                          y = scooter_pct,
#                          fill = operators)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_hline(yintercept = 0.1, color = "red", size = 2) +
#   facet_wrap(audit_date~operators) +
#   plotTheme +
#   labs(title = "Zone 8 - Allocation of Available Scooters",
#        subtitle = "10% of available scooters must be in Zone 8.")
# 
# LV_all_zones <- ggplot(LV_rebal_sf_list_summary %>% filter(!Dist_Zone %in% c("1", "8", "9")),
#                        aes(x = Dist_Zone,
#                            y = scooter_pct,
#                            fill = operators)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   # geom_hline(yintercept = 0.1, color = "red", size = 2) +
#   facet_wrap(audit_date~operators) +
#   plotTheme +
#   labs(title = "Zones 2 to 7 - Allocation of Available Scooters",
#        subtitle = "These zones have no allocation requirements.")
# 
# LV_all_zones <- ggplot(LV_rebal_sf_list_summary,
#                        aes(x = audit_date,
#                            y = scooter_pct,
#                            fill = operators,
#                            group = Dist_Zone)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   # geom_hline(yintercept = 0.1, color = "red", size = 2) +
#   # facet_wrap(audit_date~operators) +
#   plotTheme +
#   labs(title = "Zones 2 to 7 - Allocation of Available Scooters",
#        subtitle = "These zones have no allocation requirements.")

LV_rebal_sf_list_summary_map <- LV_rebal_sf_list_summary %>% 
  ungroup() %>% 
  group_by(Dist_Zone, operators) %>% 
  summarize(scooter_pct = mean(scooter_pct, na.rm = TRUE)) %>% 
  left_join(LV_distro_areas, by = "Dist_Zone") %>% 
  st_as_sf() %>% 
  arrange(operators)

tm_shape(LV_rebal_sf_list_summary_map) + tm_polygons(col = "scooter_pct")

# map of distro areas with requirements
LV_distro_areas_map <- LV_distro_areas %>% 
  mutate(Dist_Zone2 = case_when(Dist_Zone %in% c(1, 9) ~ "Zones 1 and 9 (20%)",
                                Dist_Zone == 8 ~ "Zone 8 (10%)",
                                TRUE ~ NA_character_))

ggplot() +
  geom_sf(data = LV_distro_areas_map, aes(fill = Dist_Zone2)) +
  scale_fill_viridis_d(limits = c("Zones 1 and 9 (20%)", "Zone 8 (10%)"),
                       direction = -1, 
                       na.translate = FALSE) +
  mapTheme() +
  labs(title = "Scooter Rebalancing Requirements in Louisville")

LV_rebal_sf_list_summary_2_map <- LV_rebal_sf_list_summary_2 %>% 
  gather(dist_zone, dist_pct, Dist_8_pct:Dist_1_9_pct) %>% 
  mutate(requirement = case_when(dist_zone == "Dist_8_pct" ~ 0.1,
                                 dist_zone == "Dist_1_9_pct" ~ 0.2,
                                 TRUE ~ NA_real_),
         dist_zone = factor(case_when(dist_zone == "Dist_8_pct" ~ "Dist_8_pct",
                                      dist_zone == "Dist_1_9_pct" ~ "Dist_1_9_pct",
                                      TRUE ~ NA_character_),
                            levels = c("Dist_8_pct", "Dist_1_9_pct"),
                            labels = c("Zone 8", "Zone 9")))

ggplot(LV_rebal_sf_list_summary_2_map,
       aes(x = audit_date,
           y = dist_pct, 
           fill = operators)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  geom_hline(data = LV_rebal_sf_list_summary_2_map, 
             aes(yintercept = requirement),
             color = "red",
             size = 1) +
  facet_wrap(operators~dist_zone) +
  plotTheme +
  labs(title = "Percentage of Scooters in Distribution Zones",
       subtitle = "Each audit conducted at mid-day",
       y = "Percentage of all Scooters",
       x = "Audit Date")  +
  scale_x_datetime(date_labels = "%Y-%m-%d",
                   breaks = LV_rebal_sf_list_summary_2_map$audit_date) + 
  scale_fill_discrete(name = "Distribution Zone") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

LV_audit_map <- LV_rebal_sf_list_2 %>% 
  filter(audit_date == as.POSIXct("2019-11-15 12:00:00 EST")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = LV_proj)

ggplot() +
  geom_sf(data = LV_distro_areas, fill = "lightgray") +  
  geom_sf(data = LV_audit_map %>% filter(str_detect(operators, "Lime|Bird")),
          aes(color = operators, fill = operators)) +
  facet_wrap(~operators, ncol = 1) +
  mapTheme() +
  labs(title = "Scooter Location Audit on 11-15-2019",
       subtitle = "Providers are not meeting their distribution requirements in zones 1, 8, and 9.")


###### MATT'S IMPLEMENTATION ----
plan(multiprocess) ## FOR PARALLEL PROCESSING

LV_active_status <- c("user drop off",
                      "rebalance drop off",
                      "maintenance drop off",
                      "service start",
                      "user pick up")

LV_reserved_status <- c("user pick up")

LV_inactive_status <- c("rebalance pick up",
                        "maintenance pick up",
                        "service end",
                        "low battery",
                        "maintenance")

time_intervals <- seq(from = as.POSIXct("2018-11-15 07:00:00 EDT"), 
                      to = as.POSIXct("2019-12-15 07:00:00 EDT"),
                      by = "1 week")

LV_extract_latest_status2 <- function(trip_dat, datetime, buffer, 
                                      Astatus = LV_active_status){
  time <- as.POSIXct(datetime)
  tmp <- trip_dat[which(trip_dat$occurredAt <= time),]
  # first pass to modify is data remains
  if(nrow(tmp) > 0) {
    tmp <- tmp[order(tmp$occurredAt),]
    tmp <- tmp[nrow(tmp),]
    tmp <- tmp[as.numeric(time - tmp$occurredAt) <= buffer,]
    tmp <- tmp[tmp$reason %in% Astatus,] 
  }
  # 2nd pass if the above still had rows (e.g. stilla active)
  if(nrow(tmp) > 0) {
    output <- tmp
    output$Date <- as.Date(output$occurredAt)
    output$Hour <- lubridate::hour(output$occurredAt)
    output$active <- 1
    output <- output[,c("vehicleId", "Date", "Hour", 
                        "operators", "active", "long", "lat")]
  } else { # if the scooter is "unavailable"
    output <- data.frame(vehicleId = trip_dat$vehicleId[1],
                         Date = as.Date(time),
                         Hour = hour(time),
                         operators = trip_dat$operators[1],
                         active = 0,
                         long = NA_real_,
                         lat = NA_real_,
                         stringsAsFactors = FALSE)
  }
  return(output)
}

new_func <- function(...){
  rebal_lst <- LV_rebal_sf %>% 
    mutate(long = st_coordinates(.)[,1], 
           lat = st_coordinates(.)[,2]) %>%
    st_drop_geometry() %>%
    split(.$vehicleId)
  
  results <- vector(mode="list", length = length(time_intervals))
  for(i in seq_along(time_intervals)){
    cat("Running loop for",i,"of", length(time_intervals),
        ":",as.character(time_intervals[i]),"\n")
    LV_rebal_sf_list_i <- map(rebal_lst, 
                              function(y){LV_extract_latest_status2(y,
                                                                    time_intervals[i],
                                                                    buffer = 10)}) %>%
      bind_rows() %>% 
      mutate(audit_date = time_intervals[i])
    
    results[[i]] <- LV_rebal_sf_list_i
  }
  results <- bind_rows(results)
}

new_func_parallel <- function(...){
  rebal_lst <- LV_rebal_sf %>% 
    mutate(long = st_coordinates(.)[,1], 
           lat = st_coordinates(.)[,2]) %>%
    st_drop_geometry() %>%
    split(.$vehicleId)
  
  LV_rebal_sf_list_i <- future_map(time_intervals,
                                   function(x) map(rebal_lst,                                   function(y){LV_extract_latest_status2(y,x,10)}) %>%
                                     bind_rows() %>% 
                                     mutate(audit_date = x), .progress = TRUE) %>% 
    bind_rows()
}

new_results_parallel <- new_func_parallel() # same as LV_rebal_sf_list

LV_rebal_sf_list_2 <- new_results_parallel %>% 
  filter(!is.na(long),
         !is.na(lat)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = LV_proj, remove = FALSE) %>% 
  st_join(., LV_distro_areas %>% dplyr::select(Dist_Zone)) %>% 
  st_drop_geometry() %>% 
  mutate(Dist_Zone = factor(Dist_Zone,
                            levels = paste(1:9)))

LV_rebal_sf_list_summary <- new_results_parallel %>% 
  left_join(LV_rebal_sf_list_2 %>% dplyr::select(vehicleId, Dist_Zone, audit_date), by = c("vehicleId", "audit_date")) %>% 
  group_by(audit_date, Dist_Zone, operators, .drop = FALSE) %>% 
  summarize(scooters = n()) %>% 
  filter(str_detect(operators, "Bird|Lime"),
         !is.na(Dist_Zone)) %>%
  ungroup() %>%
  group_by(audit_date, operators) %>%
  mutate(scooter_total = sum(scooters),
         scooter_pct = scooters / scooter_total)

LV_rebal_sf_list_summary_2 <- LV_rebal_sf_list_summary %>% 
  dplyr::select(-scooter_pct) %>% 
  spread(Dist_Zone, scooters, sep = "_") %>% 
  mutate(Dist_8_pct = ifelse(is.na(Dist_Zone_8 / scooter_total), 0, Dist_Zone_8 / scooter_total), 
         Dist_1_9_pct = ifelse(is.na((Dist_Zone_1 + Dist_Zone_9) / scooter_total), 0, (Dist_Zone_1 + Dist_Zone_9) / scooter_total),
         compliance = case_when(scooter_total > 150 & Dist_1_9_pct < 0.2 ~ "No",
                                scooter_total > 350 & (Dist_1_9_pct < 0.2 | Dist_8_pct < 0.1) ~ "No",
                                TRUE ~ "Yes"))

LV_rebal_sf_list_summary_map <- LV_rebal_sf_list_summary %>% 
  ungroup() %>% 
  group_by(Dist_Zone, operators) %>% 
  summarize(scooter_pct = mean(scooter_pct, na.rm = TRUE)) %>% 
  left_join(LV_distro_areas, by = "Dist_Zone") %>% 
  st_as_sf() %>% 
  arrange(operators)

LV_distro_areas_map <- LV_distro_areas %>% 
  mutate(Dist_Zone2 = case_when(Dist_Zone %in% c(1, 9) ~ "Zones 1 and 9 (20%)",
                                Dist_Zone == 8 ~ "Zone 8 (10%)",
                                TRUE ~ NA_character_))

ggplot() +
  geom_sf(data = LV_distro_areas_map, aes(fill = Dist_Zone2)) +
  scale_fill_viridis_d(limits = c("Zones 1 and 9 (20%)", "Zone 8 (10%)"),
                       direction = -1, 
                       na.translate = FALSE) +
  mapTheme() +
  labs(title = "Scooter Rebalancing Requirements in Louisville")

LV_rebal_sf_list_summary_2_map <- LV_rebal_sf_list_summary_2 %>% 
  gather(dist_zone, dist_pct, Dist_8_pct:Dist_1_9_pct) %>% 
  mutate(requirement = case_when(dist_zone == "Dist_8_pct" ~ 0.1,
                                 dist_zone == "Dist_1_9_pct" ~ 0.2,
                                 TRUE ~ NA_real_),
         dist_zone = factor(case_when(dist_zone == "Dist_8_pct" ~ "Dist_8_pct",
                                      dist_zone == "Dist_1_9_pct" ~ "Dist_1_9_pct",
                                      TRUE ~ NA_character_),
                            levels = c("Dist_8_pct", "Dist_1_9_pct"),
                            labels = c("Zone 8", "Zone 9")))

ggplot(LV_rebal_sf_list_summary_2_map,
       aes(x = audit_date,
           y = dist_pct, 
           fill = operators)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  geom_hline(data = LV_rebal_sf_list_summary_2_map, 
             aes(yintercept = requirement),
             color = "red",
             size = 1) +
  facet_wrap(operators~dist_zone) +
  plotTheme +
  labs(title = "Percentage of Scooters in Distribution Zones",
       subtitle = "Each audit conducted at 7AM",
       y = "Percentage of all Scooters",
       x = "Audit Date")  +
  scale_x_datetime(date_labels = "%Y-%m-%d",
                   breaks = LV_rebal_sf_list_summary_2_map$audit_date) + 
  scale_fill_discrete(name = "Distribution Zone") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

LV_audit_map <- LV_rebal_sf_list_2 %>% 
  filter(audit_date == as.POSIXct("2019-11-15 12:00:00 EST")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = LV_proj)

ggplot() +
  geom_sf(data = LV_distro_areas, fill = "lightgray") +  
  geom_sf(data = LV_audit_map %>% filter(str_detect(operators, "Lime|Bird")),
          aes(color = operators, fill = operators)) +
  facet_wrap(~operators, ncol = 1) +
  mapTheme() +
  labs(title = "Scooter Location Audit on 11-15-2019",
       subtitle = "Providers are not meeting their distribution requirements in zones 1, 8, and 9.")
