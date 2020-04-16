LV_Census_geoinfo$area <- as.numeric(st_area(LV_Census_geoinfo))*3.587e-8
CH_Census_geoinfo$area <- as.numeric(st_area(CH_Census_geoinfo))*3.587e-8
KC_Census_geoinfo$area <- as.numeric(st_area(KC_Census_geoinfo))*3.587e-8
DC_Census_geoinfo$area <- as.numeric(st_area(DC_Census_geoinfo))*3.587e-8
AU_Census_geoinfo$area <- as.numeric(st_area(AU_Census_geoinfo))*3.587e-8
MNP_Census_geoinfo$area <- as.numeric(st_area(MNP_Census_geoinfo))*3.587e-8

LV <- Model_clean %>% subset(Model_clean$CITY=='Louisville')
LV <- merge(LV, LV_Census_geoinfo %>% dplyr::select(GEOID, area), by='GEOID')
AU <- Model_clean %>% subset(Model_clean$CITY=='Austin')
AU <- merge(AU, AU_Census_geoinfo %>% dplyr::select(GEOID, area), by='GEOID')
DC <- Model_clean %>% subset(Model_clean$CITY=='Washington DC')
DC <- merge(DC, DC_Census_geoinfo %>% dplyr::select(GEOID, area), by='GEOID')
KC <- Model_clean %>% subset(Model_clean$CITY=='KANSAS CITY')
KC <- merge(KC, KC_Census_geoinfo %>% dplyr::select(GEOID, area), by='GEOID')
CH <- Model_clean %>% subset(Model_clean$CITY=='Chicago')
CH <- merge(CH, CH_Census_geoinfo %>% dplyr::select(GEOID, area), by='GEOID')
MNP <- Model_clean %>% subset(Model_clean$CITY=='Minneapolis')
MNP <- merge(MNP, MNP_Census_geoinfo %>% dplyr::select(GEOID, area), by='GEOID')
Model_clean_area <- rbind(LV, AU, DC, KC, CH, MNP)