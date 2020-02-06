##########################################################################
# This script:
# 1. Plot LV census data
#
##########################################################################

names(LV_Census_ct)

grid.arrange(
# Population
ggplot() + 
  geom_sf(data = LV_Census_ct, aes(fill = TotPop), color = "white", alpha = 0.8) +
  scale_fill_viridis()+
  labs(title = "Population by census tract (2018)") +
  mapTheme(),

# pWhite
ggplot() + 
  geom_sf(data = LV_Census_ct, aes(fill = pWhite), color = "white", alpha = 0.8) +
  scale_fill_viridis()+
  labs(title = "Percentage of white by census tract (2018)") +
  mapTheme(),

#pFemale
ggplot() + 
  geom_sf(data = LV_Census_ct, aes(fill = pFemale), color = "white", alpha = 0.8) +
  scale_fill_viridis()+
  labs(title = "Percentage of female by census tract (2018)") +
  mapTheme(),

# MdAge
ggplot() + 
  geom_sf(data = LV_Census_ct, aes(fill = MdAge), color = "white", alpha = 0.8) +
  scale_fill_viridis()+
  labs(title = "Median age by census tract (2018)") +
  mapTheme(),

ncol = 2)

grid.arrange(
# MdHHInc
ggplot() + 
  geom_sf(data = LV_Census_ct, aes(fill = MdHHInc), color = "white", alpha = 0.8) +
  scale_fill_viridis()+
  labs(title = "Median Household Income by census tract (2018)") +
  mapTheme(),

# MedRent
ggplot() + 
  geom_sf(data = LV_Census_ct, aes(fill = MedRent), color = "white", alpha = 0.8) +
  scale_fill_viridis()+
  labs(title = "Median rent by census tract (2018)") +
  mapTheme(),

# TotHseUni
ggplot() + 
  geom_sf(data = LV_Census_ct, aes(fill = TotHseUni), color = "white", alpha = 0.8) +
  scale_fill_viridis()+
  labs(title = "Total housing units by census tract (2018)") +
  mapTheme(),

# MedValue
ggplot() + 
  geom_sf(data = LV_Census_ct, aes(fill = MedValue), color = "white", alpha = 0.8) +
  scale_fill_viridis()+
  labs(title = "Median housing value by census tract (2018)") +
  mapTheme(),

ncol = 2)

grid.arrange(
#pDrive
ggplot() + 
  geom_sf(data = LV_Census_ct, aes(fill = pDrive), color = "white", alpha = 0.8) +
  scale_fill_viridis()+
  labs(title = "Percentage of people driving to work by census tract (2018)") +
  mapTheme(),

#pTrans
ggplot() + 
  geom_sf(data = LV_Census_ct, aes(fill = pTrans), color = "white", alpha = 0.8) +
  scale_fill_viridis()+
  labs(title = "Percentage of people taking public transit to work by census tract (2018)") +
  mapTheme(),

#pCom30plus
ggplot() + 
  geom_sf(data = LV_Census_ct, aes(fill = pCom30plus), color = "white", alpha = 0.8) +
  scale_fill_viridis()+
  labs(title = "Percentage of people commuting 30mins or more by census tract (2018)") +
  mapTheme(),

#pVehAvai
ggplot() + 
  geom_sf(data = LV_Census_ct, aes(fill = pVehAvai), color = "white", alpha = 0.8) +
  scale_fill_viridis()+
  labs(title = "Percentage of people having one or more vehicle available by census tract (2018)") +
  mapTheme(),

ncol = 2)



