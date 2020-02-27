##########################################################################
# This script is for reading in the 
# 1. Chicago scooter data.

##########################################################################

# Set directory for DC data
CH_directory <- paste(data_directory, 
                       "/Chicago",
                       sep = "")
CH_scooter_raw_file <- file.path(CH_directory, "E-Scooter_Trips_-_2019_Pilot.csv")

# Read scooter raw data
CH_scooter_raw <- read_csv(CH_scooter_raw_file)

CH_scooter_clean <- CH_scooter_raw[!is.na(CH_scooter_raw$`Start Centroid Location`),]
CH_scooter_clean <- CH_scooter_clean[!is.na(CH_scooter_clean$`End Centroid Location`),]
CH_scooter_clean$`Start Time` <- as.POSIXct(CH_scooter_clean$`Start Time`, format='%m/%d/%Y %I:%M:%S %p')
CH_scooter_clean$`End Time` <- as.POSIXct(CH_scooter_clean$`End Time`, format='%m/%d/%Y %I:%M:%S %p')
names(CH_scooter_clean)

CH_scooter_clean_ori <- st_as_sf(CH_scooter_clean, coords = c("Start Centroid Longitude", "Start Centroid Latitude"), 
                 crs = 4326) %>% 
  st_transform(CH_proj)

CH_scooter_0619 <- CH_scooter_clean_ori %>%
  filter(month(`Start Time`) == 6)

CH_scooter_0819 <- CH_scooter_clean_ori %>%
  filter(month(`Start Time`) == 8)

unique(month(CH_scooter_clean_ori$`Start Time`))

# Read city boundary shapefile 
CH_ct_file <- file.path(CH_directory,
                         "Boundaries - Census Tracts - 2010/geo_export_afd3fb8b-c948-4bba-b83c-b2a0ac543749.shp")

CH_ct <- st_read(CH_ct_file) %>%
  st_transform(CH_proj)
