SY_trimmed_model <- SY_trimmed_model %>% na.omit()
SY_trimmed_model$cntpc <- SY_trimmed_model$Predicted.CNT/SY_trimmed_model$TOTPOP#/SY_trimmed_model$area

SY_top30_pc <- SY_trimmed_model %>% subset(SY_trimmed_model$cntpc>quantile(SY_trimmed_model$cntpc,c(0.3,0.7))[2])
SY_last30_pc <- SY_trimmed_model %>% subset(SY_trimmed_model$cntpc<quantile(SY_trimmed_model$cntpc,c(0.3,0.7))[1])

mean_MEDHHINC <- abs(dim(SY_trimmed_model %>% filter(MDHHINC<mean(SY_top30_pc$MDHHINC)))[1]/dim(SY_trimmed_model)[1] - dim(SY_trimmed_model %>% filter(MDHHINC<mean(SY_last30_pc$MDHHINC)))[1]/dim(SY_trimmed_model)[1])
mean_PWHITE <- abs(dim(SY_trimmed_model %>% filter(PWHITE<mean(SY_top30_pc$PWHITE)))[1]/dim(SY_trimmed_model)[1] - dim(SY_trimmed_model %>% filter(PWHITE<mean(SY_last30_pc$PWHITE)))[1]/dim(SY_trimmed_model)[1])
# mean_PFEMALE <- abs(dim(SY_trimmed_model %>% filter(PFEMALE<mean(SY_top30_pc$PFEMALE)))[1]/dim(SY_trimmed_model)[1] - dim(SY_trimmed_model %>% filter(PFEMALE<mean(SY_last30_pc$PFEMALE)))[1]/dim(SY_trimmed_model)[1])
# mean_PBAL <- abs(dim(SY_trimmed_model %>% filter(PFEMALE<mean(SY_top30_pc$PFEMALE)))[1]/dim(SY_trimmed_model)[1] - dim(SY_trimmed_model %>% filter(PFEMALE<mean(SY_last30_pc$PFEMALE)))[1]/dim(SY_trimmed_model)[1])
# mean_MEDVALUE <- abs(dim(SY_trimmed_model %>% filter(MEDVALUE<mean(SY_top30_pc$MEDVALUE)))[1]/dim(SY_trimmed_model)[1] - dim(SY_trimmed_model %>% filter(MEDVALUE<mean(SY_last30_pc$MEDVALUE)))[1]/dim(SY_trimmed_model)[1])
# mean_MEDRENT <- dim(SY_trimmed_model %>% filter(MEDRENT<mean(SY_top30_pc$MEDRENT)))[1]/dim(SY_trimmed_model)[1] - dim(SY_trimmed_model %>% filter(MEDRENT<mean(SY_last30_pc$MEDRENT)))[1]/dim(SY_trimmed_model)[1]
# mean_PVEHAVAI <- abs(dim(SY_trimmed_model %>% filter(PVEHAVAI<mean(SY_top30_pc$PVEHAVAI)))[1]/dim(SY_trimmed_model)[1] - dim(SY_trimmed_model %>% filter(PVEHAVAI<mean(SY_last30_pc$PVEHAVAI)))[1]/dim(SY_trimmed_model)[1])
mean_MDAGE <- abs(dim(SY_trimmed_model %>% filter(MDAGE<mean(SY_top30_pc$MDAGE)))[1]/dim(SY_trimmed_model)[1] - dim(SY_trimmed_model %>% filter(MDAGE<mean(SY_last30_pc$MDAGE)))[1]/dim(SY_trimmed_model)[1])

#dim(SY_trimmed_model %>% filter(MDHHINC<mean(SY_top30_pc$MDHHINC)))[1]/dim(SY_trimmed_model)[1] - dim(SY_trimmed_model %>% filter(MDHHINC<mean(SY_last30_pc$MDHHINC)))[1]/dim(SY_trimmed_model)[1]

#calculate the equity index
sum(mean_MEDHHINC, mean_PWHITE, mean_MDAGE)/3

