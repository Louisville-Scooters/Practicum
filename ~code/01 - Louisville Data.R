library(here)
library(tidyverse)
library(sf)
library(measurements)


setwd(here::here())

large_file_directory <- paste(str_remove(here(), "\\/Practicum - Louisville Scooters"), 
                              "/~data", 
                              sep = "")

base_map <- st_read("https://opendata.arcgis.com/datasets/6e3dea8bd9cf49e6a764f7baa9141a95_30.geojson")
proj <- 2246 # https://www.spatialreference.org/ref/epsg/2246/

# Read rebalance data ----
rebalance_file <- paste(large_file_directory, 
                        "/Louisville-MDS-Status-Changes-2019Dec17.csv",
                        sep = "")

rebalance_data <- read_csv(rebalance_file)

# What is the distribution of scooter status change activities?

activity_distro_plot <- rebalance_data %>% 
  ggplot(aes(x = reason)) +
  geom_bar(stat = "count", position = "identity") +
  facet_wrap(~ type, scales = "free") +
  coord_flip() +
  labs(x = "Reason for Status Change",
       y = "Count",
       title = "Distribution of Scooter Status Change Activities")

# What is the geographic distribution of rebalancing activities only?

rebalance_data_sf <- st_as_sf(rebalance_data,
                              wkt = "location", 
                              crs = 4326)

rebalance_only <- rebalance_data_sf %>% 
  filter(str_detect(reason, "rebalance"))
rebalance_only <- rebalance_only[base_map,]

ggplot() +
  geom_sf(data = base_map, fill = NA, color = "lightgray") +
  geom_sf(data = rebalance_only, 
          aes(color = reason),
          alpha = 0.3) +
  facet_wrap(~ reason) +
  theme_minimal()

# fishnet
boundary <- st_union(base_map) %>% st_sf()

cell_area <- conv_unit(0.5, from = "mi2", to = "m2")
cell_size <- (cell_area * (2/3^0.5)) ^ 0.5 # the "cellsize" parameter is the distance between the centroids of each hexagonal cell.

fishnet <- st_make_grid(boundary, cellsize = cell_size, square = FALSE) %>% 
  st_sf() %>% 
  mutate(fishnet_ID = row_number())
