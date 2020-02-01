census_df <- data.frame(vars =     c("B01003_001E", 
                                     "B19013_001E", 
                                     "B01002_001E", 
                                     "B02001_002E",
                                     "B08014_001E",
                                     "B08014_002E",
                                     "B08013_001E",
                                     "B08012_001E",
                                     "B08301_001E",
                                     "B08301_010E"),
                        
                        colNames = c("Total_Pop",
                                     "Med_Inc",
                                     "Med_Age",
                                     "White_Pop",
                                     "Vehicle_own_pop",
                                     "No_vehicle",
                                     "Total_Travel_Time",
                                     "Num_Commuters",
                                     "Means_of_Transport_pop",
                                     "Total_Public_Trans"),
                        stringsAsFactors = FALSE)

census_vars <- census_df$vars
census_colNames <- census_df$colNames

rename_census_cols <- function(x){
  
  output <- x %>% 
    rename_at(vars(census_vars), 
              ~ census_colNames)
  
  output
}