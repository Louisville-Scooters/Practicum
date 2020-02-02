##########################################################################
# This script is for cleaning the DC scooter data
# It:
# 1. Adds some helper columns: company, start and end dates and times
# 2. Defines a function for creating DC scooter origin and destination sf objects, which can be linked via the 'trip_id' column to the rest of the dataset
#    The DC scooter data should be filtered to fewer rows before running this function, as the objects would be massive
##########################################################################

# Add helper columns ----
DC_scooter_data <- DC_scooter_data_raw %>% 
  mutate(# this is the start time from the original data. Some data includes the date, but others only have the time
    original_start_time = start_time,
    company = tolower(str_extract(dataset, "Lime|Bird|JUMP|skip|Spin|Razor|razor|Lyft")),         
    start_date = str_extract(original_start_time, "[0-9]{1,2}\\/[0-9]{1,2}\\/[0-9]{1,2}"),
    start_time = str_extract(original_start_time, "[0-9]{1,2}:[0-9]{1,2}"),
    original_end_time = end_time,
    company = tolower(str_extract(dataset, "Lime|Bird|JUMP|skip|Spin|Razor|razor|Lyft")),         
    end_date = str_extract(original_end_time, "[0-9]{1,2}\\/[0-9]{1,2}\\/[0-9]{1,2}"),
    end_time = str_extract(original_end_time, "[0-9]{1,2}:[0-9]{1,2}"))

# Make sf objects ----
make_DC_sf <- function(x, # x should be 'DC_scooter_data'
                       trip_end, # define whether you want the origins or the destinations
                       proj) { # proj should be 'DC_proj'
  
  if(!grepl("ori|des", trip_end)) {
    
    stop("trip_end must be either 'origins' or 'dests'")
    
  } else if (grepl("ori", trip_end)) {
    
    output <- x %>%
      dplyr::select(trip_id,
                    company,
                    start_lat,   
                    start_lon) %>% 
      st_as_sf(coords = c("start_lon", "start_lat"), 
               crs = 4326) %>% 
      st_transform(proj)
    
    } else {
    
    output <- x %>%
      dplyr::select(trip_id,
                    company,
                    end_lat,   
                    end_lon) %>% 
      st_as_sf(coords = c("end_lon", "end_lat"), 
               crs = 4326) %>% 
      st_transform(proj)
    
    }
  output
}

# Example of make_DC_sf() function
# DC_origins_sf <- make_DC_sf(DC_scooter_data[1:10,], 
#                             trip_end = "origins", 
#                             proj = DC_proj)