# load city boundaries:
# Asheville: 
AV_boundary <- st_read("https://opendata.arcgis.com/datasets/9fdb214ab6544f65b7493eeb5279ac0d_0.geojson")
names(AV_boundary)

AV_trimmed <- AV_model_tract %>%
  .[AV_boundary %>% dplyr::select(geometry),]

ggplot() +
  geom_sf(data = AV_trimmed)+
  geom_sf(data = AV_boundary, fill = "yellow")

# AV_trimmed2 <- st_intersection(AV_boundary %>% dplyr::select(geometry), AV_model_tract)
# ggplot(AV_trimmed2) +
#   geom_sf()

AV_trimmed_RDS <- file.path(data_directory, "~RData/Asheville/AV_trimmed.GeoJSON")
geojsonio::geojson_write(AV_trimmed, file = AV_trimmed_RDS)
AV_trimmed <- st_read(AV_trimmed_RDS)

# Hartford: 
HF_trimmed <- HF_model_tract
HF_trimmed_RDS <- file.path(data_directory, "~RData/Hartford/HF_trimmed.GeoJSON")
geojsonio::geojson_write(HF_trimmed, file = HF_trimmed_RDS)
HF_trimmed <- st_read(HF_trimmed_RDS)

# Houston: 
HS_boundary <- st_read("https://opendata.arcgis.com/datasets/2707cf506dce4879a9312ed0d66d2eb5_0.geojson")
names(HS_boundary)

HS_trimmed <- HS_model_tract %>%
  .[HS_boundary %>% dplyr::select(geometry),]

ggplot() +
  geom_sf(data = HS_trimmed, fill = "white")+
  geom_sf(data = HS_boundary, fill = "yellow")

HS_trimmed_RDS <- file.path(data_directory, "~RData/Houston/HS_trimmed.GeoJSON")
geojsonio::geojson_write(HS_trimmed, file = HS_trimmed_RDS)
HS_trimmed <- st_read(HS_trimmed_RDS)

# Jacksonville: 
JV_trimmed <- JV_model_tract
JV_trimmed_RDS <- file.path(data_directory, "~RData/Jacksonville/JV_trimmed.GeoJSON")
geojsonio::geojson_write(JV_trimmed, file = JV_trimmed_RDS)
JV_trimmed <- st_read(JV_trimmed_RDS)

# Jersey City:
JC_boundary_RDS <- file.path(data_directory, "~RData/Jersey City/police-districts.GeoJSON")
JC_boundary <- st_read(JC_boundary_RDS)
names(JC_boundary)

JC_trimmed <- JC_model_tract %>%
  .[JC_boundary %>% dplyr::select(geometry),]

ggplot() +
  geom_sf(data = JC_trimmed, fill = "white")+
  geom_sf(data = JC_boundary, fill = "yellow")

JC_trimmed_RDS <- file.path(data_directory, "~RData/Jersey City/JC_trimmed.GeoJSON")
geojsonio::geojson_write(JC_trimmed, file = JC_trimmed_RDS)
JC_trimmed <- st_read(JC_trimmed_RDS)

# Madison ####
MD_boundary <- st_read("https://opendata.arcgis.com/datasets/db89adb17d414649a71c0f29ea73e5bf_6.geojson")
names(MD_boundary)

MD_trimmed <- MD_model_tract %>%
  .[MD_boundary %>% dplyr::select(geometry),]

ggplot() +
  geom_sf(data = MD_trimmed)+
  geom_sf(data = MD_boundary, fill = "yellow")

MD_trimmed_RDS <- file.path(data_directory, "~RData/Madison/MD_trimmed.GeoJSON")
geojsonio::geojson_write(MD_trimmed, file = MD_trimmed_RDS)
MD_trimmed <- st_read(MD_trimmed_RDS)

# Omaha ####
OM_trimmed <- OM_model_tract
OM_trimmed_RDS <- file.path(data_directory, "~RData/Omaha/OM_trimmed.GeoJSON")
geojsonio::geojson_write(OM_trimmed, file = OM_trimmed_RDS)
OM_trimmed <- st_read(OM_trimmed_RDS)

# Philly ####
PH_trimmed <- PH_model_tract
PH_trimmed_RDS <- file.path(data_directory, "~RData/Philadelphia/PH_trimmed.GeoJSON")
geojsonio::geojson_write(PH_trimmed, file = PH_trimmed_RDS)
PH_trimmed <- st_read(PH_trimmed_RDS)



# Raleigh
RL_boundary <- st_read("https://opendata.arcgis.com/datasets/9a5733e13dd14e2f80f8517738ce8cc6_2.geojson")
names(RL_boundary)

RL_trimmed <- RL_model_tract %>%
  .[RL_boundary %>% dplyr::select(geometry),]

ggplot() +
  geom_sf(data = RL_trimmed)+
  geom_sf(data = RL_boundary, fill = "yellow")

RL_trimmed_RDS <- file.path(data_directory, "~RData/Raleigh/RL_trimmed.GeoJSON")
geojsonio::geojson_write(RL_trimmed, file = RL_trimmed_RDS)
RL_trimmed <- st_read(RL_trimmed_RDS)

# San Antonio
SA_boundary <- st_read("https://opendata.arcgis.com/datasets/513cfef832df4b7489a2df499972401f_0.geojson")
names(SA_boundary)

SA_trimmed <- SA_model_tract %>%
  .[SA_boundary %>% dplyr::select(geometry),]

ggplot() +
  geom_sf(data = SA_trimmed)+
  geom_sf(data = SA_boundary, fill = "yellow")

SA_trimmed_RDS <- file.path(data_directory, "~RData/San Antonio/SA_trimmed.GeoJSON")
geojsonio::geojson_write(SA_trimmed, file = SA_trimmed_RDS)
SA_trimmed <- st_read(SA_trimmed_RDS)

# Syracuse ####
SY_boundary <- st_read("https://opendata.arcgis.com/datasets/19f7a97333214e76a3bf17b2312befad_0.geojson")
names(SY_boundary)

SY_trimmed <- SY_model_tract %>%
  .[SY_boundary %>% dplyr::select(geometry),]

ggplot() +
  geom_sf(data = SY_trimmed)+
  geom_sf(data = SY_boundary, fill = "yellow")

SY_trimmed_RDS <- file.path(data_directory, "~RData/Syracuse/SY_trimmed.GeoJSON")
geojsonio::geojson_write(SY_trimmed, file = SY_trimmed_RDS)
SY_trimmed <- st_read(SY_trimmed_RDS)