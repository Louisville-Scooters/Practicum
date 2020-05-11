LV_spatial_census <- LV_spatial_census %>% na.omit()
LV_spatial_census$cntpc <- LV_spatial_census$origins_cnt/LV_spatial_census$TotPop#/LV_spatial_census$area

LV_top30_pc <- LV_spatial_census %>% subset(LV_spatial_census$cntpc>quantile(LV_spatial_census$cntpc,c(0.3,0.7))[2])
LV_last30_pc <- LV_spatial_census %>% subset(LV_spatial_census$cntpc<quantile(LV_spatial_census$cntpc,c(0.3,0.7))[1])

mean_MEDHHINC <- abs(dim(LV_spatial_census %>% filter(MdHHInc<mean(LV_top30_pc$MdHHInc)))[1]/dim(LV_spatial_census)[1] - dim(LV_spatial_census %>% filter(MdHHInc<mean(LV_last30_pc$MdHHInc)))[1]/dim(LV_spatial_census)[1])
mean_PWHITE <- abs(dim(LV_spatial_census %>% filter(pWhite<mean(LV_top30_pc$pWhite)))[1]/dim(LV_spatial_census)[1] - dim(LV_spatial_census %>% filter(pWhite<mean(LV_last30_pc$pWhite)))[1]/dim(LV_spatial_census)[1])
# mean_PFEMALE <- abs(dim(LV_spatial_census %>% filter(PFEMALE<mean(LV_top30_pc$PFEMALE)))[1]/dim(LV_spatial_census)[1] - dim(LV_spatial_census %>% filter(PFEMALE<mean(LV_last30_pc$PFEMALE)))[1]/dim(LV_spatial_census)[1])
# mean_PBAL <- abs(dim(LV_spatial_census %>% filter(PFEMALE<mean(LV_top30_pc$PFEMALE)))[1]/dim(LV_spatial_census)[1] - dim(LV_spatial_census %>% filter(PFEMALE<mean(LV_last30_pc$PFEMALE)))[1]/dim(LV_spatial_census)[1])
# mean_MEDVALUE <- abs(dim(LV_spatial_census %>% filter(MEDVALUE<mean(LV_top30_pc$MEDVALUE)))[1]/dim(LV_spatial_census)[1] - dim(LV_spatial_census %>% filter(MEDVALUE<mean(LV_last30_pc$MEDVALUE)))[1]/dim(LV_spatial_census)[1])
# mean_MEDRENT <- dim(LV_spatial_census %>% filter(MEDRENT<mean(LV_top30_pc$MEDRENT)))[1]/dim(LV_spatial_census)[1] - dim(LV_spatial_census %>% filter(MEDRENT<mean(LV_last30_pc$MEDRENT)))[1]/dim(LV_spatial_census)[1]
# mean_PVEHAVAI <- abs(dim(LV_spatial_census %>% filter(PVEHAVAI<mean(LV_top30_pc$PVEHAVAI)))[1]/dim(LV_spatial_census)[1] - dim(LV_spatial_census %>% filter(PVEHAVAI<mean(LV_last30_pc$PVEHAVAI)))[1]/dim(LV_spatial_census)[1])
mean_MDAGE <- abs(dim(LV_spatial_census %>% filter(MdAge<mean(LV_top30_pc$MdAge)))[1]/dim(LV_spatial_census)[1] - dim(LV_spatial_census %>% filter(MdAge<mean(LV_last30_pc$MdAge)))[1]/dim(LV_spatial_census)[1])

#dim(LV_spatial_census %>% filter(MDHHINC<mean(LV_top30_pc$MDHHINC)))[1]/dim(LV_spatial_census)[1] - dim(LV_spatial_census %>% filter(MDHHINC<mean(LV_last30_pc$MDHHINC)))[1]/dim(LV_spatial_census)[1]

#calculate the equity index
sum(mean_MEDHHINC, mean_PWHITE, mean_MDAGE)/3
