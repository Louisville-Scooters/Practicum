##########################################################################
# This script reads in:
# 1. Austin Work Area Characteristic (WAC) Data
# 2. Selects the tract ID and total jobs columns
# 3. Austin Residence Area Characteristic (RAC) Data
# 4. Selects the tract ID and total jobs columns
# 5. Joins them.
#
##########################################################################

# Read in WAC Data
AU_WAC_file <- file.path(data_directory,
                         "LODES/tx_wac_S000_JT00_2017.csv.gz")

AU_WAC <- read_csv(AU_WAC_file) %>% 
  dplyr::select(geocode = w_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(jobs_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% AU_tract_list) 

# Read in RAC Data
AU_RAC_file <- file.path(data_directory,
                         "LODES/tx_rac_S000_JT00_2017.csv.gz")

AU_RAC <- read_csv(AU_RAC_file) %>% 
  dplyr::select(geocode = h_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(workers_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% AU_tract_list) 

# Join them
AU_LODES <- left_join(AU_WAC, AU_RAC, by = c("geocode"))
