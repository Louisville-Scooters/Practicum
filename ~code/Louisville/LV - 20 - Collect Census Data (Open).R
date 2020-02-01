##########################################################################
# This script:
# 1. Installs a Census API key
# 2. Collects Census data and geometries for Louisville. See "02 - Census Vars and Functions.R" for variable list.
# 3. Projects the geometry to LV_proj
##########################################################################

# Read and input census key
census_key_RDS <- file.path(data_directory,
                            "~RData/Census/EC_census_key")
census_key <- readRDS(census_key_RDS)
tidycensus::census_api_key(census_key, install = TRUE, overwrite = TRUE)

# Collect census data and geometries
LV_Census_raw <- get_acs(geography = "tract", 
                     variables = census_vars, 
                     year = 2018, 
                     state = "KY", 
                     geometry = TRUE, 
                     county = c("Jefferson"),
                     output = "wide") %>%
  rename_census_cols %>%
  dplyr::select(GEOID, 
                geometry,
                census_colNames) %>% 
  st_transform(LV_proj)
