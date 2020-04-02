##########################################################################
# This script is for setting up data files for ML model building. 
# It reads in:
# 1. Model_panel

# This script exports the following data:
# 1. 
##########################################################################

library(rsample)
library(recipes)
library(parsnip)
library(workflows)
library(tune)
library(yardstick)
library(ranger)
library(xgboost)

# Reads in data 
Model_panel_RDS <- file.path(data_directory, "~RData/Model_panel")
# saveRDS(Model_panel,
#         file = Model_panel_RDS)
Model_panel <- readRDS(Model_panel_RDS)

Model_clean <- na.omit(Model_panel)
Model_clean <- Model_clean %>%
  dplyr::select(-c(MEAN_COMMUTE_TIME,GEOID, CENTROID_X, CENTROID_Y, CITY))

Model_clean_RDS <- file.path(data_directory, "~RData/Model_clean")
# saveRDS(Model_clean,
#         file = Model_clean_RDS)
Model_clean <- readRDS(Model_clean_RDS)

### XINYI'S CODE ####
# delete usused osm data KNN. COUNT, DENSITY
Model_clean <- Model_clean %>%
   dplyr::select(-starts_with('DENSITY'), -starts_with('KNN'), -starts_with('COUNT'), -ends_with('LENGTH'))
 
### XINYI'S CODE ends here :] ####