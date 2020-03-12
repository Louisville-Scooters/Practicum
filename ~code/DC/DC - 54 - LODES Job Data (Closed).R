##########################################################################
# This script reads in:
# 1. DC Work Area Characteristic (WAC) Data
# 2. Selects the tract ID and total jobs columns
# 3. DC Residence Area Characteristic (RAC) Data
# 4. Selects the tract ID and total jobs columns
# 5. Joins them.
#
##########################################################################



# Read in WAC Data
DC_WAC_file <- file.path(data_directory,
                         "LODES/dc_wac_S000_JT00_2017.csv.gz")

DC_WAC <- read_csv(DC_WAC_file) %>% 
  dplyr::select(geocode = w_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(jobs_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% DC_tract_list) 

# Read in RAC Data
DC_RAC_file <- file.path(data_directory,
                         "LODES/dc_rac_S000_JT00_2017.csv.gz")

DC_RAC <- read_csv(DC_RAC_file) %>% 
  dplyr::select(geocode = h_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(workers_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% DC_tract_list) 

# Join them
DC_LODES <- left_join(DC_WAC, DC_RAC, by = c("geocode"))

DC_LODES_RDS <- file.path(data_directory, 
                          "~RData/DC/DC_LODES")

# saveRDS(DC_LODES,
#         file = DC_LODES_RDS)
