PH_trimmed_model <- PH_trimmed_model %>% na.omit()
PH_trimmed_model$cntpc <- PH_trimmed_model$Predicted.CNT/PH_trimmed_model$TOTPOP#/PH_trimmed_model$area

PH_top30_pc <- PH_trimmed_model %>% subset(PH_trimmed_model$cntpc>=quantile(PH_trimmed_model$cntpc,c(0.3,0.7))[2])
PH_last30_pc <- PH_trimmed_model %>% subset(PH_trimmed_model$cntpc<=quantile(PH_trimmed_model$cntpc,c(0.3,0.7))[1])

mean_MEDHHINC <- abs(dim(PH_trimmed_model %>% filter(MDHHINC<mean(PH_top30_pc$MDHHINC)))[1]/dim(PH_trimmed_model)[1] - dim(PH_trimmed_model %>% filter(MDHHINC<mean(PH_last30_pc$MDHHINC)))[1]/dim(PH_trimmed_model)[1])
mean_PWHITE <- abs(dim(PH_trimmed_model %>% filter(PWHITE<mean(PH_top30_pc$PWHITE)))[1]/dim(PH_trimmed_model)[1] - dim(PH_trimmed_model %>% filter(PWHITE<mean(PH_last30_pc$PWHITE)))[1]/dim(PH_trimmed_model)[1])
# mean_PFEMALE <- abs(dim(PH_trimmed_model %>% filter(PFEMALE<mean(PH_top30_pc$PFEMALE)))[1]/dim(PH_trimmed_model)[1] - dim(PH_trimmed_model %>% filter(PFEMALE<mean(PH_last30_pc$PFEMALE)))[1]/dim(PH_trimmed_model)[1])
# mean_PBAL <- abs(dim(PH_trimmed_model %>% filter(PFEMALE<mean(PH_top30_pc$PFEMALE)))[1]/dim(PH_trimmed_model)[1] - dim(PH_trimmed_model %>% filter(PFEMALE<mean(PH_last30_pc$PFEMALE)))[1]/dim(PH_trimmed_model)[1])
# mean_MEDVALUE <- abs(dim(PH_trimmed_model %>% filter(MEDVALUE<mean(PH_top30_pc$MEDVALUE)))[1]/dim(PH_trimmed_model)[1] - dim(PH_trimmed_model %>% filter(MEDVALUE<mean(PH_last30_pc$MEDVALUE)))[1]/dim(PH_trimmed_model)[1])
# mean_MEDRENT <- dim(PH_trimmed_model %>% filter(MEDRENT<mean(PH_top30_pc$MEDRENT)))[1]/dim(PH_trimmed_model)[1] - dim(PH_trimmed_model %>% filter(MEDRENT<mean(PH_last30_pc$MEDRENT)))[1]/dim(PH_trimmed_model)[1]
# mean_PVEHAVAI <- abs(dim(PH_trimmed_model %>% filter(PVEHAVAI<mean(PH_top30_pc$PVEHAVAI)))[1]/dim(PH_trimmed_model)[1] - dim(PH_trimmed_model %>% filter(PVEHAVAI<mean(PH_last30_pc$PVEHAVAI)))[1]/dim(PH_trimmed_model)[1])
mean_MDAGE <- abs(dim(PH_trimmed_model %>% filter(MDAGE<mean(PH_top30_pc$MDAGE)))[1]/dim(PH_trimmed_model)[1] - dim(PH_trimmed_model %>% filter(MDAGE<mean(PH_last30_pc$MDAGE)))[1]/dim(PH_trimmed_model)[1])

#dim(PH_trimmed_model %>% filter(MDHHINC<mean(PH_top30_pc$MDHHINC)))[1]/dim(PH_trimmed_model)[1] - dim(PH_trimmed_model %>% filter(MDHHINC<mean(PH_last30_pc$MDHHINC)))[1]/dim(PH_trimmed_model)[1]

#calculate the equity index
sum(mean_MEDHHINC, mean_PWHITE, mean_MDAGE)/3

