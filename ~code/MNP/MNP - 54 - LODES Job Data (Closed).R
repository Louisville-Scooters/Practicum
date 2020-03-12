##########################################################################
# This script reads in:
# 1. Minneapolis Work Area Characteristic (WAC) Data
# 2. Selects the tract ID and total jobs columns
# 3. Minneapolis Residence Area Characteristic (RAC) Data
# 4. Selects the tract ID and total jobs columns
# 5. Joins them.
#
##########################################################################



# Read in WAC Data
MNP_WAC_file <- file.path(data_directory,
                         "LODES/mn_wac_S000_JT00_2017.csv.gz")

MNP_WAC <- read_csv(MNP_WAC_file) %>% 
  dplyr::select(geocode = w_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(jobs_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% MNP_tract_list) 

# Read in RAC Data
MNP_RAC_file <- file.path(data_directory,
                         "LODES/mn_rac_S000_JT00_2017.csv.gz")

MNP_RAC <- read_csv(MNP_RAC_file) %>% 
  dplyr::select(geocode = h_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(workers_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% MNP_tract_list) 

# Join them
MNP_LODES <- left_join(MNP_WAC, MNP_RAC, by = c("geocode"))

MNP_LODES_RDS <- file.path(data_directory, 
                          "~RData/Minneapolis/MNP_LODES")

# saveRDS(MNP_LODES,
#         file = MNP_LODES_RDS)
