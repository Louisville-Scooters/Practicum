# MNP_spatial_census_RDS <- file.path(data_directory, "~RData/MNP/MNP_spatial_census")
# MNP_spatial_census <- st_read(MNP_spatial_census_RDS)

#MNP_spatial_census <- readRDS()

MNP_spatial_census <- MNP_spatial_census %>% na.omit()
MNP_spatial_census$cntpc <- MNP_spatial_census$origins_cnt/MNP_spatial_census$TotPop#/MNP_spatial_census$area

MNP_top30_pc <- MNP_spatial_census %>% subset(MNP_spatial_census$cntpc>=quantile(MNP_spatial_census$cntpc,c(0.3,0.7))[2])
MNP_last30_pc <- MNP_spatial_census %>% subset(MNP_spatial_census$cntpc<=quantile(MNP_spatial_census$cntpc,c(0.3,0.7))[1])

mean_MEDHHINC <- abs(dim(MNP_spatial_census %>% filter(MdHHInc<mean(MNP_top30_pc$MdHHInc)))[1]/dim(MNP_spatial_census)[1] - dim(MNP_spatial_census %>% filter(MdHHInc<mean(MNP_last30_pc$MdHHInc)))[1]/dim(MNP_spatial_census)[1])
mean_PWHITE <- abs(dim(MNP_spatial_census %>% filter(pWhite<mean(MNP_top30_pc$pWhite)))[1]/dim(MNP_spatial_census)[1] - dim(MNP_spatial_census %>% filter(pWhite<mean(MNP_last30_pc$pWhite)))[1]/dim(MNP_spatial_census)[1])
# mean_PFEMALE <- abs(dim(MNP_spatial_census %>% filter(PFEMALE<mean(MNP_top30_pc$PFEMALE)))[1]/dim(MNP_spatial_census)[1] - dim(MNP_spatial_census %>% filter(PFEMALE<mean(MNP_last30_pc$PFEMALE)))[1]/dim(MNP_spatial_census)[1])
# mean_PBAL <- abs(dim(MNP_spatial_census %>% filter(PFEMALE<mean(MNP_top30_pc$PFEMALE)))[1]/dim(MNP_spatial_census)[1] - dim(MNP_spatial_census %>% filter(PFEMALE<mean(MNP_last30_pc$PFEMALE)))[1]/dim(MNP_spatial_census)[1])
# mean_MEDVALUE <- abs(dim(MNP_spatial_census %>% filter(MEDVALUE<mean(MNP_top30_pc$MEDVALUE)))[1]/dim(MNP_spatial_census)[1] - dim(MNP_spatial_census %>% filter(MEDVALUE<mean(MNP_last30_pc$MEDVALUE)))[1]/dim(MNP_spatial_census)[1])
# mean_MEDRENT <- dim(MNP_spatial_census %>% filter(MEDRENT<mean(MNP_top30_pc$MEDRENT)))[1]/dim(MNP_spatial_census)[1] - dim(MNP_spatial_census %>% filter(MEDRENT<mean(MNP_last30_pc$MEDRENT)))[1]/dim(MNP_spatial_census)[1]
# mean_PVEHAVAI <- abs(dim(MNP_spatial_census %>% filter(PVEHAVAI<mean(MNP_top30_pc$PVEHAVAI)))[1]/dim(MNP_spatial_census)[1] - dim(MNP_spatial_census %>% filter(PVEHAVAI<mean(MNP_last30_pc$PVEHAVAI)))[1]/dim(MNP_spatial_census)[1])
mean_MDAGE <- abs(dim(MNP_spatial_census %>% filter(MdAge<mean(MNP_top30_pc$MdAge)))[1]/dim(MNP_spatial_census)[1] - dim(MNP_spatial_census %>% filter(MdAge<mean(MNP_last30_pc$MdAge)))[1]/dim(MNP_spatial_census)[1])

#dim(MNP_spatial_census %>% filter(MDHHINC<mean(MNP_top30_pc$MDHHINC)))[1]/dim(MNP_spatial_census)[1] - dim(MNP_spatial_census %>% filter(MDHHINC<mean(MNP_last30_pc$MDHHINC)))[1]/dim(MNP_spatial_census)[1]

#calculate the equity index
1-sum(mean_MEDHHINC, mean_PWHITE, mean_MDAGE)/3
