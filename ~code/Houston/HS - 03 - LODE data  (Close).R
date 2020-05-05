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
HS_WAC_file <- file.path(data_directory,
                         "LODES/tx_wac_S000_JT00_2017.csv.gz")

HS_WAC <- read_csv(HS_WAC_file) %>% 
  dplyr::select(geocode = w_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(jobs_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% HS_Census_geoinfo$GEOID) # from HS - 20 - Collect Census Data

# Read in RAC Data
HS_RAC_file <- file.path(data_directory,
                         "LODES/tx_rac_S000_JT00_2017.csv.gz")

HS_RAC <- read_csv(HS_RAC_file) %>% 
  dplyr::select(geocode = h_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(workers_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% HS_Census_geoinfo$GEOID) # from HS - 20 - Collect Census Data

# Join them
HS_LODES <- left_join(HS_WAC, HS_RAC, by = c("geocode"))

HS_LODES_RDS <- file.path(data_directory, 
                          "~RData/Houston/HS_LODES")

# saveRDS(HS_LODES,
#         file = HS_LODES_RDS)
