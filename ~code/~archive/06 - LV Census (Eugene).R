library(tidycensus)
library(tidyverse)

tidycensus::census_api_key("3ef31f05bc4961d45eaa1d3e4787a9b4be486b9f", install = TRUE, overwrite = TRUE)
proj <- 2246 # https://www.spatialreference.org/ref/epsg/2246/


# Collect data
LV_Census <- 
  get_acs(geography = "tract", 
          variables = c("B01003_001", 
                        "B19013_001", 
                        "B02001_002", 
                        "B08013_001",
                        "B08012_001", 
                        "B08301_001", 
                        "B08301_010", 
                        "B01002_001",
                        "B08014_001", 
                        "B08014_002"), 
          year = 2018, 
          state = "KY", 
          geometry = TRUE, 
          county = c("Jefferson"),
          output = "wide") %>%
  rename(Total_Pop =  B01003_001E,
         Med_Inc = B19013_001E,
         Med_Age = B01002_001E,
         White_Pop = B02001_002E,
         Vehicle_own_pop = B08014_001E,
         No_vehicle = B08014_002E,
         Total_Travel_Time = B08013_001E,
         Num_Commuters = B08012_001E,
         Means_of_Transport_pop = B08301_001E,
         Total_Public_Trans = B08301_010E) %>%
  dplyr::select(Total_Pop, 
                Med_Inc, 
                White_Pop, 
                Total_Travel_Time,
                Means_of_Transport_pop, 
                Total_Public_Trans,
                Num_Commuters,
                Med_Age,
                Vehicle_own_pop,
                No_vehicle,
                GEOID, 
                geometry) %>%
  mutate(Percent_White = White_Pop / Total_Pop,
         Mean_Commute_Time = Total_Travel_Time / Num_Commuters,
         Percent_Taking_Public_Trans = Total_Public_Trans / Means_of_Transport_pop,
         Percent_vehicle_available = 1 - No_vehicle / Vehicle_own_pop) %>% 
  st_transform(proj)

# plots
LV_Census_2 <- LV_Census %>% 
  mutate(Percent_White_quintile = ntile(Percent_White, 5),
         Percent_Taking_Public_Trans_quintile = ntile(Percent_Taking_Public_Trans, 5),
         Percent_vehicle_quintile = ntile(Percent_vehicle_available, 5)) %>%
  dplyr::select(GEOID,
                Percent_White,
                Mean_Commute_Time,
                Percent_Taking_Public_Trans,
                Percent_vehicle_available,
                Percent_White_quintile,
                Percent_Taking_Public_Trans_quintile,
                Percent_vehicle_quintile
                ) %>%
  gather(key = "variable",
         value = "value",
         Percent_White:Percent_vehicle_quintile)

LV_Census_histogram <- LV_Census_2 %>% 
  filter(!str_detect(variable, "quintile")) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ variable, 
             scales = "free")

LV_Census_histogram

# maps
LV_Census_map <- ggplot() +
  geom_sf(data = LV_Census_2 %>% filter(str_detect(variable, "quintile")),
          aes(fill = value)) +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
  facet_wrap(~ variable, ncol = 1)

LV_Census_map


