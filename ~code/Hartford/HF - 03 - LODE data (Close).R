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
HF_WAC_file <- file.path(data_directory,
                         "LODES/ct_wac_S000_JT00_2017.csv.gz")

HF_WAC <- read_csv(HF_WAC_file) %>% 
  dplyr::select(geocode = w_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(jobs_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% HF_Census_geoinfo$GEOID) # from HS - 20 - Collect Census Data

# Read in RAC Data
HF_RAC_file <- file.path(data_directory,
                         "LODES/ct_rac_S000_JT00_2017.csv.gz")

HF_RAC <- read_csv(HF_RAC_file) %>% 
  dplyr::select(geocode = h_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(workers_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% HF_Census_geoinfo$GEOID) # from HS - 20 - Collect Census Data

# Join them
HF_LODES <- left_join(HF_WAC, HF_RAC, by = c("geocode"))

HF_LODES_RDS <- file.path(data_directory, 
                          "~RData/Hartford/HF_LODES")

# saveRDS(HF_LODES,
#         file = HF_LODES_RDS)
