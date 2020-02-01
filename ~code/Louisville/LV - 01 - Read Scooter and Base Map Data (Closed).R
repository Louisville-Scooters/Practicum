##########################################################################
# This script reads in:
# 1. Louisville base map
# 2. Scooter service area
# 3. Scooter rebalance data
# 4. Scooter Open Data
#
# This makes no changes to the data.
##########################################################################

# Read in base map
LV_base_map_raw <- st_read("https://opendata.arcgis.com/datasets/6e3dea8bd9cf49e6a764f7baa9141a95_30.geojson")

# Read in service area
LV_SA_file <- file.path(data_directory,
                        "Dockless Vehicle Service Area/Dockless_Vehicle_Service_Area.shp")

LV_SA_raw <- st_read(LV_SA_file)

# Read rebalance data
LV_rebal_file <- file.path(data_directory, 
                           "/Louisville-MDS-Status-Changes-2019Dec17.csv")

LV_rebal_raw <- read_csv(LV_rebal_file)

# Read open data
LV_open_raw <- read_csv("https://data.louisvilleky.gov/sites/default/files/DocklessTripOpenData_9.csv")
