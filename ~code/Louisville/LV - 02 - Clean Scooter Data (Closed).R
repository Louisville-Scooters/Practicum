##########################################################################
# This script is for cleaning the LV scooter data
# It:
# 1. Re-projects the LV base map and service area in the new projection
# 2. Re-projects the rebal and open scooter data.
# 3. Filters out any rebal scooter data outside the service area
# 4. Defines a function for creating LV scooter origin and destination sf objects, 
#    which can be linked via the 'TripID' column to the rest of the dataset ('LV_open_raw')
#
# This script exports the following data:
# 1. LV_rebal_sf
##########################################################################

# Project Base Map
LV_base_map <- LV_base_map_raw %>% 
  st_transform(LV_proj)

# Project Service Area Map
LV_SA <- LV_SA_raw %>% 
  st_transform(LV_proj)

### Make rebalance sf object ----
# Make the object with the code below ('ctrl + shift + c' to un-comment multiple lines at once)

# LV_rebal_sf <- st_as_sf(LV_rebal_raw,
#                             wkt = "location",
#                             crs = 4326) %>%
#   st_transform(LV_proj) %>%
#   mutate(occurredAt = with_tz(occurredAt, "America/New_York"),
#          operators = ifelse(operators == "Bolt Lousiville", # fix typo
#                             "Bolt Louisville",
#                             operators),
#          operators = as.factor(operators),
#          duration = 0, # initialize columns for for-loop
#          energy_diff = 0) %>%
#   .[LV_SA,] # filter out any trips outside the service area

LV_rebal_sf_RDS <- file.path(data_directory, 
                             "~RData/Louisville/LV_rebal_sf")
# 
# saveRDS(LV_rebal_sf,
#         file = LV_rebal_sf_RDS)

# Read the saved object with the code below
LV_rebal_sf <- readRDS(LV_rebal_sf_RDS)

# Make sf objects with open data ----
make_LV_open_sf <- function(x, # x should be 'LV_open_raw'
                            trip_end, # define whether you want the origins or the destinations
                            proj) { # proj should be 'LV_proj'
  
  if(!grepl("ori|des", trip_end)) {
    
    stop("trip_end must be either 'origins' or 'dests'")
    
  } else if (grepl("ori", trip_end)) {
    
    output <- x %>%
      dplyr::select(TripID,
                    StartLatitude,   
                    StartLongitude,
                    StartDate) %>% 
      st_as_sf(coords = c("StartLongitude", "StartLatitude"), 
               crs = 4326) %>% 
      st_transform(proj)
    
  } else {
    
    output <- x %>%
      dplyr::select(TripID,
                    EndLatitude,   
                    EndLongitude, 
                    StartDate) %>% 
      st_as_sf(coords = c("EndLongitude", "EndLatitude"), 
               crs = 4326) %>% 
      st_transform(proj)
    
  }
  output
}

### Make and save open data origins ----
 LV_open_origins <- make_LV_open_sf(LV_open_raw,
                                    trip_end = "origins",
                                    proj = LV_proj) %>%
   .[LV_SA,]

xLV_open_origins_RDS <- file.path(data_directory, 
                             "~RData/Louisville/LV_open_origins")
# 
 saveRDS(LV_open_origins,
         file = LV_open_origins_RDS)

# Read the saved object with the code below
LV_open_origins <- readRDS(LV_open_origins_RDS)


### Make and save open data destinations ----
# LV_open_dests <- make_LV_open_sf(LV_open_raw,
#                                  trip_end = "dests",
#                                  proj = LV_proj) %>%
#   .[LV_SA,]

LV_open_dests_RDS <- file.path(data_directory, 
                                 "~RData/Louisville/LV_open_dests")

# saveRDS(LV_open_dests,
#         file = LV_open_dests_RDS)

# Read the saved object with the code below
LV_open_dests <- readRDS(LV_open_dests_RDS)

