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

RL_trimmed_RDS <- file.path(data_directory, "~RData/Raleigh/RL_trimmed.GeoJSON")
RL_trimmed <- st_read(RL_trimmed_RDS)

SA_trimmed_RDS <- file.path(data_directory, "~RData/San Antonio/SA_trimmed.GeoJSON")
SA_trimmed <- st_read(SA_trimmed_RDS)

SY_trimmed_RDS <- file.path(data_directory, "~RData/Syracuse/SY_trimmed.GeoJSON")
SY_trimmed <- st_read(SY_trimmed_RDS)

