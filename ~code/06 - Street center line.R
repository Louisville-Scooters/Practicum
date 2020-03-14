
# Louisville ####
LV_street <- st_read('https://opendata.arcgis.com/datasets/f36b2c8164714b258840dce66909ba9a_1.geojson') %>%
  st_transform(LV_proj)

LV_street <- LV_street %>% st_join(LV_Census_geoinfo %>% st_intersection(LV_SA))

LV_street_ct_len <- st_intersection(LV_street,LV_Census_geoinfo) %>%
  mutate(length = as.numeric(st_length(.))*0.000189394) %>%
  group_by(GEOID) %>%
  summarise(street_length = sum(length)) %>%
  st_set_geometry(NULL) %>%
  merge(LV_Census_geoinfo, on='GEOID', all.y=T) %>%
  st_as_sf()

LV_spatial_census <- left_join(LV_spatial_census, LV_street_ct_len%>%st_set_geometry(NULL)%>%dplyr::select(GEOID, street_length), by = 'GEOID')
LV_spatial_census_RDS <- file.path(data_directory, "~RData/Louisville/LV_spatial_census")
saveRDS(LV_spatial_census,
        file = LV_spatial_census_RDS)
LV_spatial_census <- readRDS(LV_spatial_census_RDS)


# Austin ####
AU_street <- st_read('https://data.austintexas.gov/api/geospatial/m5w3-uea6?method=export&format=GeoJSON') %>%
  st_transform(AU_proj)

AU_street <- AU_street %>% st_join(AU_Census_geoinfo)

AU_street_ct_len <- st_intersection(AU_street,AU_Census_geoinfo) %>%
  mutate(length = as.numeric(st_length(.))*0.000189394) %>%
  group_by(GEOID) %>%
  summarise(street_length = sum(length)) %>%
  st_set_geometry(NULL) %>%
  merge(AU_Census_geoinfo, on='GEOID', all.y=T) %>%
  st_as_sf()

AU_street_ct_len$street_length <- replace_na(AU_street_ct_len$street_length,0)

AU_spatial_census <- left_join(AU_spatial_census, AU_street_ct_len%>%st_set_geometry(NULL)%>%dplyr::select(GEOID, street_length), by = 'GEOID')

# Chicago ####
CH_street <- st_read('https://data.cityofchicago.org/api/geospatial/6imu-meau?method=export&format=GeoJSON') %>%
  st_transform(CH_proj)

CH_street <- CH_street %>% st_join(CH_Census_ct)

CH_street_ct_len <- st_intersection(CH_street,CH_Census_ct) %>%
  mutate(length = as.numeric(st_length(.))*0.000189394) %>%
  group_by(geoid10) %>%
  summarise(street_length = sum(length)) %>%
  st_set_geometry(NULL) %>%
  merge(CH_Census_ct, on='geoid10', all.y=T) %>%
  st_as_sf()

CH_street_ct_len$street_length <- replace_na(CH_street_ct_len$street_length,0)

CH_spatial_census <- left_join(CH_spatial_census, CH_street_ct_len%>%st_set_geometry(NULL)%>%dplyr::select(geoid10, street_length), by = 'geoid10')

# Minneapolis ####
MNP_street <- st_read('https://opendata.arcgis.com/datasets/e68d01d782c04d88876bbd51e1c40702_0.geojson') %>%
  st_transform(MNP_proj)

MNP_street <- MNP_street %>% st_join(MNP_Census_geoinfo)

MNP_street_ct_len <- st_intersection(MNP_street,MNP_Census_geoinfo) %>%
  mutate(length = as.numeric(st_length(.))*0.000189394) %>%
  group_by(GEOID) %>%
  summarise(street_length = sum(length)) %>%
  st_set_geometry(NULL) %>%
  merge(MNP_Census_geoinfo, on='GEOID', all.y=T) %>%
  st_as_sf()

MNP_street_ct_len$street_length <- replace_na(MNP_street_ct_len$street_length,0)

MNP_spatial_census <- left_join(MNP_spatial_census, MNP_street_ct_len%>%st_set_geometry(NULL)%>%dplyr::select(GEOID, street_length), by = 'GEOID')

# DC ####
DC_street <- st_read('https://opendata.arcgis.com/datasets/23246020d6894453bdfcee00956df818_41.geojson') %>%
  st_transform(DC_proj)

DC_street <- DC_street %>% st_join(DC_Census_geoinfo)

DC_street_ct_len <- st_intersection(DC_street,DC_Census_geoinfo) %>%
  mutate(length = as.numeric(st_length(.))*0.000189394) %>%
  group_by(GEOID) %>%
  summarise(street_length = sum(length)) %>%
  st_set_geometry(NULL) %>%
  merge(DC_Census_geoinfo, on='GEOID', all.y=T) %>%
  st_as_sf()

DC_street_ct_len$street_length <- replace_na(DC_street_ct_len$street_length,0)

DC_spatial_census <- left_join(DC_spatial_census, DC_street_ct_len%>%st_set_geometry(NULL)%>%dplyr::select(GEOID, street_length), by = 'GEOID')

# Kansas City ####
KC_street <- st_read('E:/GRAD/MUSA801/KC_street/centerline_kansas_city.shp') %>%
  st_set_crs(102698) %>%
  st_transform(KC_proj)

KC_street <- KC_street %>% st_join(KC_Census_geoinfo)

ggplot()+
  geom_sf(data = KC_Census_geoinfo) +
  geom_sf(data = KC_street)

KC_street_osm <- opq ("Kansas City USA") %>%
  add_osm_feature(key = 'highway', value = c("pedestrain", "living_road")) %>%
  osmdata_sf(.)

KC_street_osm <- st_geometry(KC_street_osm$osm_lines) %>%
  st_transform(KC_proj) %>%
  st_sf() %>%
  st_intersection(KC_SA) %>%
  mutate(Legend = 'street',
         City = 'Kansas City') %>%
  dplyr::select(Legend, City, geometry)

ggplot()+
  geom_sf(data = KC_street)

KC_street_ct_len <- st_intersection(KC_street,KC_Census_geoinfo) %>%
  mutate(length = as.numeric(st_length(.))*0.000189394) %>%
  group_by(GEOID) %>%
  summarise(street_legth = sum(length)) %>%
  st_set_geometry(NULL) %>%
  merge(KC_Census_geoinfo, on='GEOID', all.y=T) %>%
  st_as_sf()

KC_street_ct_len$street_length <- replace_na(KC_street_ct_len$street_legth,0)