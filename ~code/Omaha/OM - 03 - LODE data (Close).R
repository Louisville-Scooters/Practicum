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
OM_WAC_file <- file.path(data_directory,
                         "LODES/ne_wac_S000_JT00_2017.csv.gz")

OM_WAC <- read_csv(OM_WAC_file) %>% 
  dplyr::select(geocode = w_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(jobs_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% OM_Census_geoinfo$GEOID) # from HS - 20 - Collect Census Data

# Read in RAC Data
OM_RAC_file <- file.path(data_directory,
                         "LODES/ne_rac_S000_JT00_2017.csv.gz")

OM_RAC <- read_csv(OM_RAC_file) %>% 
  dplyr::select(geocode = h_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(workers_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% OM_Census_geoinfo$GEOID) # from HS - 20 - Collect Census Data

# Join them
OM_LODES <- left_join(OM_WAC, OM_RAC, by = c("geocode"))

OM_LODES_RDS <- file.path(data_directory, 
                          "~RData/Omaha/OM_LODES")

# saveRDS(OM_LODES,
#         file = OM_LODES_RDS)
