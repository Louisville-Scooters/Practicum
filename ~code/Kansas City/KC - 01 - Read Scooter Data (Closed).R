##########################################################################
# This script reads in:
# 1. KC Scooter data
#
# This makes no changes to the raw data.
##########################################################################

# Read open data
KSC_file <- paste(data_directory, 
                  "/Microtransit__Scooter_and_Ebike__Trips_ksc.csv",
                  sep = "")

KC_scooter_raw <- read_csv(KSC_file)