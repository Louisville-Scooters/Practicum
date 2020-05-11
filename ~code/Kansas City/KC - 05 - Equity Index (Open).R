KC_spatial_census <- KC_spatial_census %>% na.omit()
KC_spatial_census$cntpc <- KC_spatial_census$origins_cnt/KC_spatial_census$TotPop#/KC_spatial_census$area

KC_top30_pc <- KC_spatial_census %>% subset(KC_spatial_census$cntpc>=quantile(KC_spatial_census$cntpc,c(0.3,0.7))[2])
KC_last30_pc <- KC_spatial_census %>% subset(KC_spatial_census$cntpc<=quantile(KC_spatial_census$cntpc,c(0.3,0.7))[1])

mean_MEDHHINC <- abs(dim(KC_spatial_census %>% filter(MdHHInc<mean(KC_top30_pc$MdHHInc)))[1]/dim(KC_spatial_census)[1] - dim(KC_spatial_census %>% filter(MdHHInc<mean(KC_last30_pc$MdHHInc)))[1]/dim(KC_spatial_census)[1])
mean_PWHITE <- abs(dim(KC_spatial_census %>% filter(pWhite<mean(KC_top30_pc$pWhite)))[1]/dim(KC_spatial_census)[1] - dim(KC_spatial_census %>% filter(pWhite<mean(KC_last30_pc$pWhite)))[1]/dim(KC_spatial_census)[1])
# mean_PFEMALE <- abs(dim(KC_spatial_census %>% filter(PFEMALE<mean(KC_top30_pc$PFEMALE)))[1]/dim(KC_spatial_census)[1] - dim(KC_spatial_census %>% filter(PFEMALE<mean(KC_last30_pc$PFEMALE)))[1]/dim(KC_spatial_census)[1])
# mean_PBAL <- abs(dim(KC_spatial_census %>% filter(PFEMALE<mean(KC_top30_pc$PFEMALE)))[1]/dim(KC_spatial_census)[1] - dim(KC_spatial_census %>% filter(PFEMALE<mean(KC_last30_pc$PFEMALE)))[1]/dim(KC_spatial_census)[1])
# mean_MEDVALUE <- abs(dim(KC_spatial_census %>% filter(MEDVALUE<mean(KC_top30_pc$MEDVALUE)))[1]/dim(KC_spatial_census)[1] - dim(KC_spatial_census %>% filter(MEDVALUE<mean(KC_last30_pc$MEDVALUE)))[1]/dim(KC_spatial_census)[1])
# mean_MEDRENT <- dim(KC_spatial_census %>% filter(MEDRENT<mean(KC_top30_pc$MEDRENT)))[1]/dim(KC_spatial_census)[1] - dim(KC_spatial_census %>% filter(MEDRENT<mean(KC_last30_pc$MEDRENT)))[1]/dim(KC_spatial_census)[1]
# mean_PVEHAVAI <- abs(dim(KC_spatial_census %>% filter(PVEHAVAI<mean(KC_top30_pc$PVEHAVAI)))[1]/dim(KC_spatial_census)[1] - dim(KC_spatial_census %>% filter(PVEHAVAI<mean(KC_last30_pc$PVEHAVAI)))[1]/dim(KC_spatial_census)[1])
mean_MDAGE <- abs(dim(KC_spatial_census %>% filter(MdAge<mean(KC_top30_pc$MdAge)))[1]/dim(KC_spatial_census)[1] - dim(KC_spatial_census %>% filter(MdAge<mean(KC_last30_pc$MdAge)))[1]/dim(KC_spatial_census)[1])

#dim(KC_spatial_census %>% filter(MDHHINC<mean(KC_top30_pc$MDHHINC)))[1]/dim(KC_spatial_census)[1] - dim(KC_spatial_census %>% filter(MDHHINC<mean(KC_last30_pc$MDHHINC)))[1]/dim(KC_spatial_census)[1]

#calculate the equity index
1-sum(mean_MEDHHINC, mean_PWHITE, mean_MDAGE)/3
