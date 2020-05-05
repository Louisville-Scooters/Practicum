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
JC_WAC_file <- file.path(data_directory,
                         "LODES/nj_wac_S000_JT00_2017.csv.gz")

JC_WAC <- read_csv(JC_WAC_file) %>% 
  dplyr::select(geocode = w_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(jobs_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% JC_Census_geoinfo$GEOID) # from JC - 20 - Collect Census Data

# Read in RAC Data
JC_RAC_file <- file.path(data_directory,
                         "LODES/nj_rac_S000_JT00_2017.csv.gz")

JC_RAC <- read_csv(JC_RAC_file) %>% 
  dplyr::select(geocode = h_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(workers_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% JC_Census_geoinfo$GEOID) # from JC - 20 - Collect Census Data

# Join them
JC_LODES <- left_join(JC_WAC, JC_RAC, by = c("geocode"))

JC_LODES_RDS <- file.path(data_directory, 
                          "~RData/Jersey City/JC_LODES")

# saveRDS(JC_LODES,
#         file = JC_LODES_RDS)
