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
SY_WAC_file <- file.path(data_directory,
                         "LODES/ny_wac_S000_JT00_2017.csv.gz")

SY_WAC <- read_csv(SY_WAC_file) %>% 
  dplyr::select(geocode = w_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(jobs_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% SY_Census_geoinfo$GEOID) # from HS - 20 - Collect Census Data

# Read in RAC Data
SY_RAC_file <- file.path(data_directory,
                         "LODES/ny_rac_S000_JT00_2017.csv.gz")

SY_RAC <- read_csv(SY_RAC_file) %>% 
  dplyr::select(geocode = h_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(workers_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% SY_Census_geoinfo$GEOID) # from HS - 20 - Collect Census Data

# Join them
SY_LODES <- left_join(SY_WAC, SY_RAC, by = c("geocode"))

SY_LODES_RDS <- file.path(data_directory, 
                          "~RData/Omaha/SY_LODES")

# saveRDS(SY_LODES,
#         file = SY_LODES_RDS)
