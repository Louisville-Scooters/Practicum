##########################################################################
# This script is for cleaning the MNP scooter data
# It:
# 1. Adds some helper columns: start and end dates and times
# 2. Links geographical information to the data based on streetcenterlineID
##########################################################################

# Add helper columns ----
MNP_scooter_data <- MNP_scooter_data_raw %>% 
  mutate(# this is the start time from the original data. Some data includes the date, but others only have the time
    start_date = date(starttime),
    start_time = hour(starttime),
    end_date = date(endtime),
    end_time = hour(endtime))

