##########################################################################
# This script is for reading in the 
# 1. MNP scooter data.
# 2. MNP street centerline shapefile


##########################################################################

# Set directory for DC data
MNP_directory <- paste(data_directory, 
                      "/MNP",
                      sep = "")
MNP_directory
# List of all scooter-related files
MNP_scooter_trip_list <- list.files(path = MNP_directory, pattern = "*.csv", full.names = T)

MNP_scooter_trip_list
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

# Read trail centerline shapefile
MNP_TR_file <- file.path(MNP_directory,
                         "Pedestrian_and_Bicycle_Trails/Pedestrian_and_Bicycle_Trails.shp")

MNP_Trail <- st_read(MNP_TR_file) %>%
  st_transform(MNP_proj)

# Read city boundary shapefile 
MNP_ct_file <- file.path(MNP_directory,
                         "MNP_CityLimits/msvcGIS_MinneapolisCityLimits.shp")

MNP_ct <- st_read(MNP_ct_file) %>%
  st_transform(MNP_proj)

ggplot(MNP_Trail) + geom_sf()
