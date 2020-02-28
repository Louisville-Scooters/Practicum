
# Louisville ####
LV_street <- st_read('https://opendata.arcgis.com/datasets/f36b2c8164714b258840dce66909ba9a_1.geojson') %>%
  st_transform(LV_proj)

LV_street <- LV_street %>% st_join(LV_Census_geoinfo %>% st_intersection(LV_SA))

LV_street_ct_len <- st_intersection(LV_street,LV_Census_geoinfo) %>%
  mutate(length = as.numeric(st_length(.))*0.000189394) %>%
  group_by(GEOID) %>%
  summarise(total_length = sum(length)) %>%
  st_set_geometry(NULL) %>%
  merge(LV_Census_geoinfo, on='GEOID', all.y=T) %>%
  st_as_sf()

LV_street_ct_len$total_length <- replace_na(LV_street_ct_len$total_length,0)

# Austin ####
AU_street <- st_read('https://data.austintexas.gov/api/geospatial/m5w3-uea6?method=export&format=GeoJSON') %>%
  st_transform(AU_proj)

AU_street <- AU_street %>% st_join(AU_Census_geoinfo %>% st_intersection(AU_SA))

AU_street_ct_len <- st_intersection(AU_street,AU_Census_geoinfo) %>%
  mutate(length = as.numeric(st_length(.))*0.000189394) %>%
  group_by(GEOID) %>%
  summarise(total_length = sum(length)) %>%
  st_set_geometry(NULL) %>%
  merge(AU_Census_geoinfo, on='GEOID', all.y=T) %>%
  st_as_sf()

AU_street_ct_len$total_length <- replace_na(AU_street_ct_len$total_length,0)

# Chicago ####
CH_street <- st_read('https://data.cityofchicago.org/api/geospatial/6imu-meau?method=export&format=GeoJSON') %>%
  st_transform(CH_proj)

CH_street <- CH_street %>% st_join(CH_Census_geoinfo %>% st_intersection(CH_SA))

CH_street_ct_len <- st_intersection(CH_street,CH_Census_geoinfo) %>%
  mutate(length = as.numeric(st_length(.))*0.000189394) %>%
  group_by(GEOID) %>%
  summarise(total_length = sum(length)) %>%
  st_set_geometry(NULL) %>%
  merge(CH_Census_geoinfo, on='GEOID', all.y=T) %>%
  st_as_sf()

CH_street_ct_len$total_length <- replace_na(CH_street_ct_len$total_length,0)

# Minneapolis ####
MNP_street <- st_read('https://opendata.arcgis.com/datasets/e68d01d782c04d88876bbd51e1c40702_0.geojson') %>%
  st_transform(MNP_proj)

MNP_street <- MNP_street %>% st_join(MNP_Census_geoinfo %>% st_intersection(MNP_SA))

MNP_street_ct_len <- st_intersection(MNP_street,MNP_Census_geoinfo) %>%
  mutate(length = as.numeric(st_length(.))*0.000189394) %>%
  group_by(GEOID) %>%
  summarise(total_length = sum(length)) %>%
  st_set_geometry(NULL) %>%
  merge(MNP_Census_geoinfo, on='GEOID', all.y=T) %>%
  st_as_sf()

MNP_street_ct_len$total_length <- replace_na(MNP_street_ct_len$total_length,0)

# DC ####
DC_street <- st_read('https://opendata.arcgis.com/datasets/23246020d6894453bdfcee00956df818_41.geojson') %>%
  st_transform(DC_proj)

DC_street <- DC_street %>% st_join(DC_Census_geoinfo %>% st_intersection(DC_SA))

DC_street_ct_len <- st_intersection(DC_street,DC_Census_geoinfo) %>%
  mutate(length = as.numeric(st_length(.))*0.000189394) %>%
  group_by(GEOID) %>%
  summarise(total_length = sum(length)) %>%
  st_set_geometry(NULL) %>%
  merge(DC_Census_geoinfo, on='GEOID', all.y=T) %>%
  st_as_sf()

DC_street_ct_len$total_length <- replace_na(DC_street_ct_len$total_length,0)

# Kansas City ####
KC_street <- st_read('https://opendata.arcgis.com/datasets/23246020d6894453bdfcee00956df818_41.geojson') %>%
  st_transform(KC_proj)

DC_street <- DC_street %>% st_join(DC_Census_geoinfo %>% st_intersection(DC_SA))

DC_street_ct_len <- st_intersection(DC_street,DC_Census_geoinfo) %>%
  mutate(length = as.numeric(st_length(.))*0.000189394) %>%
  group_by(GEOID) %>%
  summarise(total_length = sum(length)) %>%
  st_set_geometry(NULL) %>%
  merge(DC_Census_geoinfo, on='GEOID', all.y=T) %>%
  st_as_sf()

DC_street_ct_len$total_length <- replace_na(DC_street_ct_len$total_length,0)