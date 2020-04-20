##########################################################################
# This script reads in:
# 1. Madison Work Area Characteristic (WAC) Data
# 2. Selects the tract ID and total jobs columns
# 3. Madison Residence Area Characteristic (RAC) Data
# 4. Selects the tract ID and total jobs columns
# 5. Joins them.
#
##########################################################################



# Read in WAC Data
MD_WAC_file <- file.path(data_directory,
                         "LODES/wi_wac_S000_JT00_2017.csv.gz")

MD_WAC <- read_csv(MD_WAC_file) %>% 
  dplyr::select(geocode = w_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(jobs_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% MD_Census_geoinfo$GEOID) # from MD - 20 - Collect Census Data

# Read in RAC Data
MD_RAC_file <- file.path(data_directory,
                         "LODES/wi_rac_S000_JT00_2017.csv.gz")

MD_RAC <- read_csv(MD_RAC_file) %>% 
  dplyr::select(geocode = h_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(workers_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% MD_Census_geoinfo$GEOID) # from MD - 20 - Collect Census Data

# Join them
MD_LODES <- left_join(MD_WAC, MD_RAC, by = c("geocode"))

MD_LODES_RDS <- file.path(data_directory, 
                          "~RData/Philadelphia/MD_LODES")

# saveRDS(MD_LODES,
#         file = MD_LODES_RDS)
