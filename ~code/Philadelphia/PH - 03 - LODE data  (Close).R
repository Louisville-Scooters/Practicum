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
PH_WAC_file <- file.path(data_directory,
                         "LODES/pa_wac_S000_JT00_2017.csv.gz")

PH_WAC <- read_csv(PH_WAC_file) %>% 
  dplyr::select(geocode = w_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(jobs_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% PH_Census_geoinfo$GEOID) # from PH - 20 - Collect Census Data

# Read in RAC Data
PH_RAC_file <- file.path(data_directory,
                         "LODES/pa_rac_S000_JT00_2017.csv.gz")

PH_RAC <- read_csv(PH_RAC_file) %>% 
  dplyr::select(geocode = h_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(workers_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% PH_Census_geoinfo$GEOID) # from PH - 20 - Collect Census Data

# Join them
PH_LODES <- left_join(PH_WAC, PH_RAC, by = c("geocode"))

PH_LODES_RDS <- file.path(data_directory, 
                          "~RData/Philadelphia/PH_LODES")

# saveRDS(PH_LODES,
#         file = PH_LODES_RDS)
