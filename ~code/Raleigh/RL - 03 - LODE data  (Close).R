##########################################################################
# This script reads in:
# 1. Louisville Work Area Characteristic (WAC) Data
# 2. Selects the tract ID and total jobs columns
# 3. Louisville Residence Area Characteristic (RAC) Data
# 4. Selects the tract ID and total jobs columns
# 5. Joins them.
#
##########################################################################



# Read in WAC Data
RL_WAC_file <- file.path(data_directory,
                         "LODES/nc_wac_S000_JT00_2017.csv.gz")

RL_WAC <- read_csv(RL_WAC_file) %>% 
  dplyr::select(geocode = w_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(jobs_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% RL_Census_geoinfo$GEOID) # from RL - 20 - Collect Census Data

# Read in RAC Data
RL_RAC_file <- file.path(data_directory,
                         "LODES/nc_rac_S000_JT00_2017.csv.gz")

RL_RAC <- read_csv(RL_RAC_file) %>% 
  dplyr::select(geocode = h_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(workers_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% RL_Census_geoinfo$GEOID) # from RL - 20 - Collect Census Data

# Join them
RL_LODES <- left_join(RL_WAC, RL_RAC, by = c("geocode"))

RL_LODES_RDS <- file.path(data_directory, 
                          "~RData/Raleigh/RL_LODES")

# saveRDS(RL_LODES,
#         file = RL_LODES_RDS)
