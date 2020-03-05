##########################################################################
# This script reads in:
# 1. Chicago Work Area Characteristic (WAC) Data
# 2. Selects the tract ID and total jobs columns
# 3. Chicago Residence Area Characteristic (RAC) Data
# 4. Selects the tract ID and total jobs columns
# 5. Joins them.
#
##########################################################################



# Read in WAC Data
CH_WAC_file <- file.path(data_directory,
                         "LODES/il_wac_S000_JT00_2017.csv.gz")

CH_WAC <- read_csv(CH_WAC_file) %>% 
  dplyr::select(geocode = w_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(jobs_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% CH_tract_list) 

# Read in RAC Data
CH_RAC_file <- file.path(data_directory,
                         "LODES/il_rac_S000_JT00_2017.csv.gz")

CH_RAC <- read_csv(CH_RAC_file) %>% 
  dplyr::select(geocode = h_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(workers_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% CH_tract_list) 

# Join them
CH_LODES <- left_join(CH_WAC, CH_RAC, by = c("geocode"))
