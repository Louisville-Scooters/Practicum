##########################################################################
# This script:
# 1. Creates datetime columns for the KC scooter data and removes unnecessary columns
# 2. Defines a function for creating KC scooter origin and destination sf objects, 
#    which can be linked via the 'trip_id' column to the rest of the dataset ('KC_scooter')
#
# This makes no changes to the raw data.
##########################################################################

# Make datetime columns
KC_scooter <- KC_scooter_raw %>% 
  mutate(start_time = as.POSIXct(paste(substring(.$`Start Date`, 1, 10), 
                                       as.character(.$`Start Time`)),
                                 format = '%m/%d/%Y %H:%M:%S'),
         end_time =  as.POSIXct(paste(substring(.$`End Date`, 1, 10), 
                                      as.character(.$`End Time`)),
                                format = '%m/%d/%Y %H:%M:%S')) %>% 
  dplyr::select(-c("Start Time", 
                   "End Time",
                   "Start Date",
                   "End Date")) %>% 
  clean_names() # remove spaces in column names and make lowercase

# Make sf objects
make_KC_sf <- function(x, # x should be 'KC_scooter'
                       trip_end, # define whether you want the origins or the destinations
                       proj) { # proj should be 'KC_proj'
  
  if(!grepl("ori|des", trip_end)) {
    
    stop("trip_end must be either 'origins' or 'dests'")
    
  } else if (grepl("ori", trip_end)) {
    
    output <- x %>%
      dplyr::select(trip_id,
                    start_location) %>% 
      st_as_sf(wkt = "start_location", 
               crs = 4326) %>% 
      st_transform(proj)
    
  } else {
    
    output <- x %>%
      dplyr::select(trip_id,
                    end_location) %>% 
      st_as_sf(wkt = "end_location", 
               crs = 4326) %>% 
      st_transform(proj)
    
  }
  output
}

# Example of make_KC_sf() function
# KC_scooter_sf <- make_KC_sf(KC_scooter[1:10,],
#                             trip_end = "origins",
#                             proj = KC_proj)