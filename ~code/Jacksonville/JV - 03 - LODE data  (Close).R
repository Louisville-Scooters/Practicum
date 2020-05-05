##########################################################################
# This script reads in:
# 1. Louisville Work Area Characteristic (WAC) Data
# 2. Selects the tract ID and total jobs columns
# 3. Louisville Residefle Area Characteristic (RAC) Data
# 4. Selects the tract ID and total jobs columns
# 5. Joins them.
#
##########################################################################



# Read in WAC Data
JV_WAC_file <- file.path(data_directory,
                         "LODES/fl_wac_S000_JT00_2017.csv.gz")

JV_WAC <- read_csv(JV_WAC_file) %>% 
  dplyr::select(geocode = w_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(jobs_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% JV_Census_geoinfo$GEOID) # from JV - 20 - Collect Census Data

# Read in RAC Data
JV_RAC_file <- file.path(data_directory,
                         "LODES/fl_rac_S000_JT00_2017.csv.gz")

JV_RAC <- read_csv(JV_RAC_file) %>% 
  dplyr::select(geocode = h_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(workers_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% JV_Census_geoinfo$GEOID) # from JV - 20 - Collect Census Data

# Join them
JV_LODES <- left_join(JV_WAC, JV_RAC, by = c("geocode"))

JV_LODES_RDS <- file.path(data_directory, 
                          "~RData/Jacksonville/JV_LODES")

# saveRDS(JV_LODES,
#         file = JV_LODES_RDS)
