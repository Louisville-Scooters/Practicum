##########################################################################
# This script:
# 1. Creates a list of census variables to collect for each city with standardized column names
# 2. Defines a helper function for renaming columns after pulling census data
##########################################################################

# List of 2018 ACS variables: https://api.census.gov/data/2018/acs/acs5/variables.html
census_df <- data.frame(vars =     c("B01003_001E", 
                                     "B01001_026E",
                                     "B00002_001E",
                                     "B19013_001E", 
                                     "B01002_001E", 
                                     "B02001_002E",
                                     "B08014_001E",
                                     "B08014_002E",
                                     "B08013_001E",
                                     "B08012_001E",
                                     "B08012_008E",
                                     "B08012_009E",
                                     "B08012_010E",
                                     "B08012_011E",
                                     "B08012_012E",
                                     "B08012_013E",
                                     "B08301_001E",
                                     "B08301_002E",
                                     "B08301_010E",
                                     "B25002_001E",
                                     "B25002_002E",
                                     "B25077_001E",
                                     "B25064_001E"),
                        
                        colNames = c("TotPop",
                                     "TotFemale",
                                     "TotHseUni",
                                     "MdHHInc",
                                     "MdAge",
                                     "White_Pop",
                                     "Vehicle_own_pop",
                                     "No_vehicle",
                                     "Total_Travel_Time",
                                     "Travel_Time_3034",
                                     "Travel_Time_3539",
                                     "Travel_Time_4044",
                                     "Travel_Time_4549",
                                     "Travel_Time_6089",
                                     "Travel_Time_90plus",
                                     "Num_Commuters",
                                     "Means_of_Transport_pop",
                                     "Total_cartruckvan",
                                     "Total_Public_Trans",
                                     "Total_occupancy",
                                     "Occupied",
                                     "MedValue",
                                     "MedRent"),
                        stringsAsFactors = FALSE)

census_vars <- census_df$vars
census_colNames <- census_df$colNames

# Function for renaming columns after collecting census data
rename_census_cols <- function(x){
  
  output <- x %>% 
    rename_at(vars(census_vars), 
              ~ census_colNames)
  
  output
}

# Example of rename_census_cols() function
# ASTCensus <- 
#   get_acs(geography = "tract", 
#           variables = census_vars, 
#           year = 2018, 
#           state = "TX", 
#           geometry = TRUE, 
#           county = c("Travis"),
#           output = "wide") %>%
#   rename_census_cols()