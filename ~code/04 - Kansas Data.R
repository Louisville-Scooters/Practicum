data_directory <- paste(str_remove(here(), "\\/Eugene\\/Eugene - Practicum|\\/Ophelia\\/Ophelia - Practicum|\\/Xinyi\\/Xinyi - Practicum"), 
                        "/~data", 
                        sep = "")

### I have filter out the data on the website
# Read Kansas scooter data (2019) ####
# Austin_file <- paste(data_directory, 
#                      "/Austin.csv",
#                      sep = "")
KSC_file <- paste(data_directory, 
                     "/Microtransit__Scooter_and_Ebike__Trips_ksc.csv",
                     sep = "")

dat_KSC <- read_csv(KSC_file)
