# CH_spatial_census_RDS <- file.path(data_directory, "~RData/Chicago/CH_spatial_census")
# CH_spatial_census <- st_read(CH_spatial_census_RDS)

#CH_spatial_census <- readRDS()

CH_spatial_census <- CH_spatial_census %>% na.omit()
CH_spatial_census$cntpc <- CH_spatial_census$origins_cnt/CH_spatial_census$TotPop#/CH_spatial_census$area

CH_top30_pc <- CH_spatial_census %>% subset(CH_spatial_census$cntpc>=quantile(CH_spatial_census$cntpc,c(0.3,0.7))[2])
CH_last30_pc <- CH_spatial_census %>% subset(CH_spatial_census$cntpc<=quantile(CH_spatial_census$cntpc,c(0.3,0.7))[1])

mean_MEDHHINC <- abs(dim(CH_spatial_census %>% filter(MdHHInc<mean(CH_top30_pc$MdHHInc)))[1]/dim(CH_spatial_census)[1] - dim(CH_spatial_census %>% filter(MdHHInc<mean(CH_last30_pc$MdHHInc)))[1]/dim(CH_spatial_census)[1])
mean_PWHITE <- abs(dim(CH_spatial_census %>% filter(pWhite<mean(CH_top30_pc$pWhite)))[1]/dim(CH_spatial_census)[1] - dim(CH_spatial_census %>% filter(pWhite<mean(CH_last30_pc$pWhite)))[1]/dim(CH_spatial_census)[1])
# mean_PFEMALE <- abs(dim(CH_spatial_census %>% filter(PFEMALE<mean(CH_top30_pc$PFEMALE)))[1]/dim(CH_spatial_census)[1] - dim(CH_spatial_census %>% filter(PFEMALE<mean(CH_last30_pc$PFEMALE)))[1]/dim(CH_spatial_census)[1])
# mean_PBAL <- abs(dim(CH_spatial_census %>% filter(PFEMALE<mean(CH_top30_pc$PFEMALE)))[1]/dim(CH_spatial_census)[1] - dim(CH_spatial_census %>% filter(PFEMALE<mean(CH_last30_pc$PFEMALE)))[1]/dim(CH_spatial_census)[1])
# mean_MEDVALUE <- abs(dim(CH_spatial_census %>% filter(MEDVALUE<mean(CH_top30_pc$MEDVALUE)))[1]/dim(CH_spatial_census)[1] - dim(CH_spatial_census %>% filter(MEDVALUE<mean(CH_last30_pc$MEDVALUE)))[1]/dim(CH_spatial_census)[1])
# mean_MEDRENT <- dim(CH_spatial_census %>% filter(MEDRENT<mean(CH_top30_pc$MEDRENT)))[1]/dim(CH_spatial_census)[1] - dim(CH_spatial_census %>% filter(MEDRENT<mean(CH_last30_pc$MEDRENT)))[1]/dim(CH_spatial_census)[1]
# mean_PVEHAVAI <- abs(dim(CH_spatial_census %>% filter(PVEHAVAI<mean(CH_top30_pc$PVEHAVAI)))[1]/dim(CH_spatial_census)[1] - dim(CH_spatial_census %>% filter(PVEHAVAI<mean(CH_last30_pc$PVEHAVAI)))[1]/dim(CH_spatial_census)[1])
mean_MDAGE <- abs(dim(CH_spatial_census %>% filter(MdAge<mean(CH_top30_pc$MdAge)))[1]/dim(CH_spatial_census)[1] - dim(CH_spatial_census %>% filter(MdAge<mean(CH_last30_pc$MdAge)))[1]/dim(CH_spatial_census)[1])

#dim(CH_spatial_census %>% filter(MDHHINC<mean(CH_top30_pc$MDHHINC)))[1]/dim(CH_spatial_census)[1] - dim(CH_spatial_census %>% filter(MDHHINC<mean(CH_last30_pc$MDHHINC)))[1]/dim(CH_spatial_census)[1]

#calculate the equity index
1-sum(mean_MEDHHINC, mean_PWHITE, mean_MDAGE)/3

