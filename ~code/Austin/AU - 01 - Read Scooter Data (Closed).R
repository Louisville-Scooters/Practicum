##########################################################################
# This script reads in:
# 1. Austin scooter data
#
# This makes no changes to the raw data.
##########################################################################

# Read in Austin data
Austin_file <- file.path(data_directory,
                         "/Shared_Micromobility_Vehicle_Trips_austin.csv")

AU_scooter_raw <- vroom(Austin_file,
                        col_types = cols(.default = "c",
                                         "Vehicle Type" = "f",
                                         "Trip Duration" = "d",
                                         "Trip Distance" = "d",
                                         "Start Time" = "c",
                                         "End Time" = "c",
                                         "Modified Date" = "T",
                                         "Month" = "f",
                                         "Hour" = "d",
                                         "Day of Week" = "f",
                                         "Council District (Start)" = "f",
                                         "Council District (End)" = "f",
                                         "Year" = "d",
                                         "Census Tract Start" = "c",
                                         "Census Tract End" = "c"))
