AU_Census$city <- 'Austin'
KC_Census$city <- 'Kansas City'
LV_Census$city <- 'Louisville'
MNP_Census$city <- 'Minneapolis'
DC_Census$city <- 'DC'

AU_KC_LV_MNP_DC_Census <- rbind(AU_Census %>% st_set_geometry(NULL), 
                         KC_Census %>% st_set_geometry(NULL), 
                         LV_Census %>% st_set_geometry(NULL),
                         MNP_Census %>% st_set_geometry(NULL),
                         DC_Census %>% st_set_geometry(NULL))

AU_KC_LV_MNP_DC_Census_gather <- AU_KC_LV_MNP_DC_Census %>%
  dplyr::select(-GEOID) %>%
  gather(key='Variable',"Value", -city)

ggplot() +
  geom_boxplot(data = as.data.frame(AU_KC_LV_MNP_DC_Census_gather), aes(x=city, y=Value, fill=city)) +
  scale_fill_viridis(discrete = T)+
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  #labs(title = "Price as a function of continuous variables") +
  theme(axis.text.x = element_blank(), legend.position = 'right', axis.text.x.bottom = element_blank()) +
  plotTheme

