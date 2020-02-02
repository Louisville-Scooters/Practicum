##########################################################################
# This script is for reading in the 
# 1. MNP scooter data.
# 2. MNP street centerline shapefile


##########################################################################

# Set directory for DC data
MNP_directory <- paste(data_directory, 
                      "/MNP",
                      sep = "")

# List of all scooter-related files
MNP_scooter_trip_list <- list.files(path = MNP_directory, pattern = "*.csv", full.names = T)

MNP_scooter_trip_list
MNP_directory

MNP_scooter_data_raw <- MNP_scooter_trip_list %>% 
  map_df(., 
         ~ read_csv(.,
                    col_types = cols(.default = "c")) %>% # read all the columns as characters for simplicity
           set_names(., tolower(names(.))) %>%
           mutate(dataset = .x,
                  dataset = str_match(dataset, paste(MNP_directory, "(.*?)", "\\.csv", sep = ""))[, 2]))

# Change the stattime and endtime to datetime object
MNP_scooter_data_raw$starttime <- as_datetime(MNP_scooter_data_raw$starttime)
MNP_scooter_data_raw$endtime <- as_datetime(MNP_scooter_data_raw$endtime)

MNP_scooter_data_raw <- MNP_scooter_data_raw %>%
  dplyr::select(-objectid)

# Read street centerline shapefile
MNP_ST_file <- file.path(MNP_directory,
                         "PW_Street_Centerline/PW_Street_Centerline.shp")

MNP_street <- st_read(MNP_ST_file) %>%
  st_transform(MNP_proj)
