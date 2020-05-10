# read trimmed data (full tracts are kept if city boundaries cut throw them)
AV_trimmed_RDS <- file.path(data_directory, "~RData/Asheville/AV_trimmed.GeoJSON")
AV_trimmed <- st_read(AV_trimmed_RDS)

HF_trimmed_RDS <- file.path(data_directory, "~RData/Hartford/HF_trimmed.GeoJSON")
HF_trimmed <- st_read(HF_trimmed_RDS)

HS_trimmed_RDS <- file.path(data_directory, "~RData/Houston/HS_trimmed.GeoJSON")
HS_trimmed <- st_read(HS_trimmed_RDS)

JV_trimmed_RDS <- file.path(data_directory, "~RData/Jacksonville/JV_trimmed.GeoJSON")
JV_trimmed <- st_read(JV_trimmed_RDS)

JC_trimmed_RDS <- file.path(data_directory, "~RData/Jersey City/JC_trimmed.GeoJSON")
JC_trimmed <- st_read(JC_trimmed_RDS)

MD_trimmed_RDS <- file.path(data_directory, "~RData/Madison/MD_trimmed.GeoJSON")
MD_trimmed <- st_read(MD_trimmed_RDS)

OM_trimmed_RDS <- file.path(data_directory, "~RData/Omaha/OM_trimmed.GeoJSON")
OM_trimmed <- st_read(OM_trimmed_RDS)

PH_trimmed_RDS <- file.path(data_directory, "~RData/Philadelphia/PH_trimmed.GeoJSON")
PH_trimmed <- st_read(PH_trimmed_RDS)

# RL_trimmed_RDS <- file.path(data_directory, "~RData/Raleigh/RL_trimmed.GeoJSON")
# RL_trimmed <- st_read(RL_trimmed_RDS)

SA_trimmed_RDS <- file.path(data_directory, "~RData/San Antonio/SA_trimmed.GeoJSON")
SA_trimmed <- st_read(SA_trimmed_RDS)

SY_trimmed_RDS <- file.path(data_directory, "~RData/Syracuse/SY_trimmed.GeoJSON")
SY_trimmed <- st_read(SY_trimmed_RDS)

# trim the data
## Asheville
AV_trimmed_model <- AV_model %>%
  filter(AV_model$GEOID %in% AV_trimmed$GEOID)



## Hartford
HF_trimmed_model <- HF_model %>%
  filter(HF_model$GEOID %in% HF_trimmed$GEOID)




HS_trimmed_model <- HS_model %>%
  filter(HS_model$GEOID %in% HS_trimmed$GEOID)



JV_trimmed_model <- JV_model %>%
  filter(JV_model$GEOID %in% JV_trimmed$GEOID)



JC_trimmed_model <- JC_model %>%
  filter(JC_model$GEOID %in% JC_trimmed$GEOID)



MD_trimmed_model <- MD_model %>%
  filter(MD_model$GEOID %in% MD_trimmed$GEOID)




OM_trimmed_model <- OM_model %>%
  filter(OM_model$GEOID %in% OM_trimmed$GEOID)




PH_trimmed_model <- PH_model %>%
  filter(PH_model$GEOID %in% PH_trimmed$GEOID)




SA_trimmed_model <- SA_model %>%
  filter(SA_model$GEOID %in% SA_trimmed$GEOID)




SY_trimmed_model <- SY_model %>%
  filter(SY_model$GEOID %in% SY_trimmed$GEOID)
