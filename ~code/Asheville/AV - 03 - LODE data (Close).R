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
AV_WAC_file <- file.path(data_directory,
                         "LODES/nc_wac_S000_JT00_2017.csv.gz")

AV_WAC <- read_csv(AV_WAC_file) %>% 
  dplyr::select(geocode = w_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(jobs_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% AV_Census_geoinfo$GEOID) # from HS - 20 - Collect Census Data

# Read in RAC Data
AV_RAC_file <- file.path(data_directory,
                         "LODES/nc_rac_S000_JT00_2017.csv.gz")

AV_RAC <- read_csv(AV_RAC_file) %>% 
  dplyr::select(geocode = h_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(workers_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% AV_Census_geoinfo$GEOID) # from HS - 20 - Collect Census Data

# Join them
AV_LODES <- left_join(AV_WAC, AV_RAC, by = c("geocode"))

AV_LODES_RDS <- file.path(data_directory, 
                          "~RData/Asheville/AV_LODES")

# saveRDS(AV_LODES,
#         file = AV_LODES_RDS)
