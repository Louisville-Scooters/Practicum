data_directory <- paste(str_remove(here(), "\\/Eugene\\/Eugene - Practicum|\\/Ophelia\\/Ophelia - Practicum|\\/Xinyi\\/Xinyi - Practicum"), 
                        "/~data", 
                        sep = "")

KSC_file <- paste(data_directory, 
                     "/Microtransit__Scooter_and_Ebike__Trips_ksc.csv",
                     sep = "")

dat_KSC <- read_csv(KSC_file)
# the census tract boundary does not cover all the trips points.
dat_KSC$`Start Time` <- as.character(dat_KSC$`Start Time`)
dat_KSC$`Start Time` <- paste(substring(dat_KSC$`Start Date`,1,10),dat_KSC$`Start Time`)

dat_KSC$`End Time` <- as.character(dat_KSC$`End Time`)
dat_KSC$`End Time` <- paste(substring(dat_KSC$`End Date`,1,10),dat_KSC$`End Time`)

dat_KSC$`Start Time` <- as.POSIXct(dat_KSC$`Start Time`, format='%m/%d/%Y %H:%M:%S')
dat_KSC$`End Time` <- as.POSIXct(dat_KSC$`End Time`, format='%m/%d/%Y %H:%M:%S')
dat_KSC <- select(dat_KSC, -`Start Date`, -`End Date`)
