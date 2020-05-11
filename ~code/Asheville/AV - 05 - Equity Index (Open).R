AV_trimmed_model$cntpc <- AV_trimmed_model$Predicted.CNT/AV_trimmed_model$TOTPOP#/AV_trimmed_model$area

AV_top30_pc <- AV_trimmed_model %>% subset(AV_trimmed_model$cntpc>=quantile(AV_trimmed_model$cntpc,c(0.3,0.7))[2])
AV_last30_pc <- AV_trimmed_model %>% subset(AV_trimmed_model$cntpc<=quantile(AV_trimmed_model$cntpc,c(0.3,0.7))[1])

mean_MEDHHINC <- abs(dim(AV_trimmed_model %>% filter(MDHHINC<mean(AV_top30_pc$MDHHINC)))[1]/dim(AV_trimmed_model)[1] - dim(AV_trimmed_model %>% filter(MDHHINC<mean(AV_last30_pc$MDHHINC)))[1]/dim(AV_trimmed_model)[1])
mean_PWHITE <- abs(dim(AV_trimmed_model %>% filter(PWHITE<mean(AV_top30_pc$PWHITE)))[1]/dim(AV_trimmed_model)[1] - dim(AV_trimmed_model %>% filter(PWHITE<mean(AV_last30_pc$PWHITE)))[1]/dim(AV_trimmed_model)[1])
# mean_PFEMALE <- abs(dim(AV_trimmed_model %>% filter(PFEMALE<mean(AV_top30_pc$PFEMALE)))[1]/dim(AV_trimmed_model)[1] - dim(AV_trimmed_model %>% filter(PFEMALE<mean(AV_last30_pc$PFEMALE)))[1]/dim(AV_trimmed_model)[1])
# mean_PBAL <- abs(dim(AV_trimmed_model %>% filter(PFEMALE<mean(AV_top30_pc$PFEMALE)))[1]/dim(AV_trimmed_model)[1] - dim(AV_trimmed_model %>% filter(PFEMALE<mean(AV_last30_pc$PFEMALE)))[1]/dim(AV_trimmed_model)[1])
# mean_MEDVALUE <- abs(dim(AV_trimmed_model %>% filter(MEDVALUE<mean(AV_top30_pc$MEDVALUE)))[1]/dim(AV_trimmed_model)[1] - dim(AV_trimmed_model %>% filter(MEDVALUE<mean(AV_last30_pc$MEDVALUE)))[1]/dim(AV_trimmed_model)[1])
# mean_MEDRENT <- dim(AV_trimmed_model %>% filter(MEDRENT<mean(AV_top30_pc$MEDRENT)))[1]/dim(AV_trimmed_model)[1] - dim(AV_trimmed_model %>% filter(MEDRENT<mean(AV_last30_pc$MEDRENT)))[1]/dim(AV_trimmed_model)[1]
# mean_PVEHAVAI <- abs(dim(AV_trimmed_model %>% filter(PVEHAVAI<mean(AV_top30_pc$PVEHAVAI)))[1]/dim(AV_trimmed_model)[1] - dim(AV_trimmed_model %>% filter(PVEHAVAI<mean(AV_last30_pc$PVEHAVAI)))[1]/dim(AV_trimmed_model)[1])
mean_MDAGE <- abs(dim(AV_trimmed_model %>% filter(MDAGE<mean(AV_top30_pc$MDAGE)))[1]/dim(AV_trimmed_model)[1] - dim(AV_trimmed_model %>% filter(MDAGE<mean(AV_last30_pc$MDAGE)))[1]/dim(AV_trimmed_model)[1])

#dim(AV_trimmed_model %>% filter(MDHHINC<mean(AV_top30_pc$MDHHINC)))[1]/dim(AV_trimmed_model)[1] - dim(AV_trimmed_model %>% filter(MDHHINC<mean(AV_last30_pc$MDHHINC)))[1]/dim(AV_trimmed_model)[1]
1-sum(mean_MEDHHINC, mean_PWHITE, mean_MDAGE)/3

