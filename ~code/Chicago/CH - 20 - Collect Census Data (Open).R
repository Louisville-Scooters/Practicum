##########################################################################
# This script:
# 1. Installs a Census API key
# 2. Collects Census data and geometries for Chicago See "02 - Census Vars and Functions.R" for variable list.
# 3. Projects the geometry to CH_proj
##########################################################################

# Read and input census key
census_key_RDS <- file.path(data_directory,
                            "~RData/Census/EC_census_key")
census_key <- readRDS(census_key_RDS)
tidycensus::census_api_key(census_key, install = TRUE, overwrite = TRUE)

# Collect census data and geometries
CH_Census_raw <- get_acs(geography = "tract", 
                          variables = census_vars, 
                          year = 2018, 
                          state = "IL", 
                          geometry = TRUE, 
                          county = c("Cook"),
                          output = "wide") %>%
  rename_census_cols %>%
  dplyr::select(GEOID, 
                geometry,
                census_colNames) %>% 
  st_transform(CH_proj)

CH_tract_list <- CH_ct$geoid10

# extract centroid of each census tract
CH_ct <- CH_ct %>% 
  mutate(centroid_X = st_coordinates(st_centroid(CH_ct))[, 1],
         centroid_Y = st_coordinates(st_centroid(CH_ct))[, 2])

