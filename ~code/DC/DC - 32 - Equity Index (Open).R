# DC_spatial_census_RDS <- file.path(data_directory, "~RData/DC/DC_spatial_census")
# DC_spatial_census <- st_read(DC_spatial_census_RDS)

#DC_spatial_census <- readRDS()

DC_spatial_census <- DC_spatial_census %>% na.omit()
DC_spatial_census$cntpc <- DC_spatial_census$origins_cnt/DC_spatial_census$TotPop#/DC_spatial_census$area

DC_top30_pc <- DC_spatial_census %>% subset(DC_spatial_census$cntpc>=quantile(DC_spatial_census$cntpc,c(0.3,0.7))[2])
DC_last30_pc <- DC_spatial_census %>% subset(DC_spatial_census$cntpc<=quantile(DC_spatial_census$cntpc,c(0.3,0.7))[1])

mean_MEDHHINC <- abs(dim(DC_spatial_census %>% filter(MdHHInc<mean(DC_top30_pc$MdHHInc)))[1]/dim(DC_spatial_census)[1] - dim(DC_spatial_census %>% filter(MdHHInc<mean(DC_last30_pc$MdHHInc)))[1]/dim(DC_spatial_census)[1])
mean_PWHITE <- abs(dim(DC_spatial_census %>% filter(pWhite<mean(DC_top30_pc$pWhite)))[1]/dim(DC_spatial_census)[1] - dim(DC_spatial_census %>% filter(pWhite<mean(DC_last30_pc$pWhite)))[1]/dim(DC_spatial_census)[1])
# mean_PFEMALE <- abs(dim(DC_spatial_census %>% filter(PFEMALE<mean(DC_top30_pc$PFEMALE)))[1]/dim(DC_spatial_census)[1] - dim(DC_spatial_census %>% filter(PFEMALE<mean(DC_last30_pc$PFEMALE)))[1]/dim(DC_spatial_census)[1])
# mean_PBAL <- abs(dim(DC_spatial_census %>% filter(PFEMALE<mean(DC_top30_pc$PFEMALE)))[1]/dim(DC_spatial_census)[1] - dim(DC_spatial_census %>% filter(PFEMALE<mean(DC_last30_pc$PFEMALE)))[1]/dim(DC_spatial_census)[1])
# mean_MEDVALUE <- abs(dim(DC_spatial_census %>% filter(MEDVALUE<mean(DC_top30_pc$MEDVALUE)))[1]/dim(DC_spatial_census)[1] - dim(DC_spatial_census %>% filter(MEDVALUE<mean(DC_last30_pc$MEDVALUE)))[1]/dim(DC_spatial_census)[1])
# mean_MEDRENT <- dim(DC_spatial_census %>% filter(MEDRENT<mean(DC_top30_pc$MEDRENT)))[1]/dim(DC_spatial_census)[1] - dim(DC_spatial_census %>% filter(MEDRENT<mean(DC_last30_pc$MEDRENT)))[1]/dim(DC_spatial_census)[1]
# mean_PVEHAVAI <- abs(dim(DC_spatial_census %>% filter(PVEHAVAI<mean(DC_top30_pc$PVEHAVAI)))[1]/dim(DC_spatial_census)[1] - dim(DC_spatial_census %>% filter(PVEHAVAI<mean(DC_last30_pc$PVEHAVAI)))[1]/dim(DC_spatial_census)[1])
mean_MDAGE <- abs(dim(DC_spatial_census %>% filter(MdAge<mean(DC_top30_pc$MdAge)))[1]/dim(DC_spatial_census)[1] - dim(DC_spatial_census %>% filter(MdAge<mean(DC_last30_pc$MdAge)))[1]/dim(DC_spatial_census)[1])

#dim(DC_spatial_census %>% filter(MDHHINC<mean(DC_top30_pc$MDHHINC)))[1]/dim(DC_spatial_census)[1] - dim(DC_spatial_census %>% filter(MDHHINC<mean(DC_last30_pc$MDHHINC)))[1]/dim(DC_spatial_census)[1]

#calculate the equity index
1-sum(mean_MEDHHINC, mean_PWHITE, mean_MDAGE)/3
