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
LV_WAC_file <- file.path(data_directory,
                         "LODES/ky_wac_S000_JT00_2017.csv.gz")

LV_WAC <- read_csv(LV_WAC_file) %>% 
  dplyr::select(geocode = w_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(jobs_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% LV_Census_geoinfo$GEOID) # from LV - 20 - Collect Census Data

# Read in RAC Data
LV_RAC_file <- file.path(data_directory,
                         "LODES/ky_rac_S000_JT00_2017.csv.gz")

LV_RAC <- read_csv(LV_RAC_file) %>% 
  dplyr::select(geocode = h_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(workers_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% LV_Census_geoinfo$GEOID) # from LV - 20 - Collect Census Data

# Join them
LV_LODES <- left_join(LV_WAC, LV_RAC, by = c("geocode"))

LV_LODES_RDS <- file.path(data_directory, 
                          "~RData/Louisville/LV_LODES")

# saveRDS(LV_LODES,
#         file = LV_LODES_RDS)
