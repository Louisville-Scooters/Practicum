##########################################################################
# This script is for reading in the MNP scooter data.


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
           set_names(., tolower(names(.))))

# Change the stattime and endtime to datetime object
MNP_scooter_data_raw$starttime <- as_datetime(MNP_scooter_data_raw$starttime)
MNP_scooter_data_raw$endtime <- as_datetime(MNP_scooter_data_raw$endtime)

MNP_scooter_data_raw <- MNP_scooter_data_raw %>%
  dplyr::select(-objectid)
