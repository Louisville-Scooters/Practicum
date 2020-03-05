##########################################################################
# This script reads in:
# 1. KC Work Area Characteristic (WAC) Data
# 2. Selects the tract ID and total jobs columns
# 3. KC Residence Area Characteristic (RAC) Data
# 4. Selects the tract ID and total jobs columns
# 5. Joins them.
#
##########################################################################



# Read in WAC Data
KC_WAC_file <- file.path(data_directory,
                         "LODES/mo_wac_S000_JT00_2017.csv.gz")

KC_WAC <- read_csv(KC_WAC_file) %>% 
  dplyr::select(geocode = w_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(jobs_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% KC_tract_list) 

# Read in RAC Data
KC_RAC_file <- file.path(data_directory,
                         "LODES/mo_rac_S000_JT00_2017.csv.gz")

KC_RAC <- read_csv(KC_RAC_file) %>% 
  dplyr::select(geocode = h_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(workers_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% KC_tract_list) 

# Join them
KC_LODES <- left_join(KC_WAC, KC_RAC, by = c("geocode"))
