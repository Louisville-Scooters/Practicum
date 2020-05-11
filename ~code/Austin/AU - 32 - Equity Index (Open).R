AU_spatial_census_RDS <- file.path(data_directory, "~RData/Austin/AU_spatial_census")
AU_spatial_census <- st_read(AU_spatial_census_RDS)

#AU_spatial_census <- readRDS()

AU_spatial_census <- AU_spatial_census %>% na.omit()
AU_spatial_census$cntpc <- AU_spatial_census$origins_ct/AU_spatial_census$TotPop#/AU_spatial_census$area

AU_top30_pc <- AU_spatial_census %>% subset(AU_spatial_census$cntpc>=quantile(AU_spatial_census$cntpc,c(0.3,0.7))[2])
AU_last30_pc <- AU_spatial_census %>% subset(AU_spatial_census$cntpc<=quantile(AU_spatial_census$cntpc,c(0.3,0.7))[1])

mean_MEDHHINC <- abs(dim(AU_spatial_census %>% filter(MdHHInc<mean(AU_top30_pc$MdHHInc)))[1]/dim(AU_spatial_census)[1] - dim(AU_spatial_census %>% filter(MdHHInc<mean(AU_last30_pc$MdHHInc)))[1]/dim(AU_spatial_census)[1])
mean_PWHITE <- abs(dim(AU_spatial_census %>% filter(pWhite<mean(AU_top30_pc$pWhite)))[1]/dim(AU_spatial_census)[1] - dim(AU_spatial_census %>% filter(pWhite<mean(AU_last30_pc$pWhite)))[1]/dim(AU_spatial_census)[1])
# mean_PFEMALE <- abs(dim(AU_spatial_census %>% filter(PFEMALE<mean(AU_top30_pc$PFEMALE)))[1]/dim(AU_spatial_census)[1] - dim(AU_spatial_census %>% filter(PFEMALE<mean(AU_last30_pc$PFEMALE)))[1]/dim(AU_spatial_census)[1])
# mean_PBAL <- abs(dim(AU_spatial_census %>% filter(PFEMALE<mean(AU_top30_pc$PFEMALE)))[1]/dim(AU_spatial_census)[1] - dim(AU_spatial_census %>% filter(PFEMALE<mean(AU_last30_pc$PFEMALE)))[1]/dim(AU_spatial_census)[1])
# mean_MEDVALUE <- abs(dim(AU_spatial_census %>% filter(MEDVALUE<mean(AU_top30_pc$MEDVALUE)))[1]/dim(AU_spatial_census)[1] - dim(AU_spatial_census %>% filter(MEDVALUE<mean(AU_last30_pc$MEDVALUE)))[1]/dim(AU_spatial_census)[1])
# mean_MEDRENT <- dim(AU_spatial_census %>% filter(MEDRENT<mean(AU_top30_pc$MEDRENT)))[1]/dim(AU_spatial_census)[1] - dim(AU_spatial_census %>% filter(MEDRENT<mean(AU_last30_pc$MEDRENT)))[1]/dim(AU_spatial_census)[1]
# mean_PVEHAVAI <- abs(dim(AU_spatial_census %>% filter(PVEHAVAI<mean(AU_top30_pc$PVEHAVAI)))[1]/dim(AU_spatial_census)[1] - dim(AU_spatial_census %>% filter(PVEHAVAI<mean(AU_last30_pc$PVEHAVAI)))[1]/dim(AU_spatial_census)[1])
mean_MDAGE <- abs(dim(AU_spatial_census %>% filter(MdAge<mean(AU_top30_pc$MdAge)))[1]/dim(AU_spatial_census)[1] - dim(AU_spatial_census %>% filter(MdAge<mean(AU_last30_pc$MdAge)))[1]/dim(AU_spatial_census)[1])

#dim(AU_spatial_census %>% filter(MDHHINC<mean(AU_top30_pc$MDHHINC)))[1]/dim(AU_spatial_census)[1] - dim(AU_spatial_census %>% filter(MDHHINC<mean(AU_last30_pc$MDHHINC)))[1]/dim(AU_spatial_census)[1]

#calculate the equity index
1-sum(mean_MEDHHINC, mean_PWHITE, mean_MDAGE)/3
