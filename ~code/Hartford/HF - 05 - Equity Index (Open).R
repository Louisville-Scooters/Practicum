HF_trimmed_model <- HF_trimmed_model %>% na.omit()
HF_trimmed_model$cntpc <- HF_trimmed_model$Predicted.CNT/HF_trimmed_model$TOTPOP#/HF_trimmed_model$area

HF_top30_pc <- HF_trimmed_model %>% subset(HF_trimmed_model$cntpc>quantile(HF_trimmed_model$cntpc,c(0.3,0.7))[2])
HF_last30_pc <- HF_trimmed_model %>% subset(HF_trimmed_model$cntpc<quantile(HF_trimmed_model$cntpc,c(0.3,0.7))[1])

mean_MEDHHINC <- abs(dim(HF_trimmed_model %>% filter(MDHHINC<mean(HF_top30_pc$MDHHINC)))[1]/dim(HF_trimmed_model)[1] - dim(HF_trimmed_model %>% filter(MDHHINC<mean(HF_last30_pc$MDHHINC)))[1]/dim(HF_trimmed_model)[1])
mean_PWHITE <- abs(dim(HF_trimmed_model %>% filter(PWHITE<mean(HF_top30_pc$PWHITE)))[1]/dim(HF_trimmed_model)[1] - dim(HF_trimmed_model %>% filter(PWHITE<mean(HF_last30_pc$PWHITE)))[1]/dim(HF_trimmed_model)[1])
# mean_PFEMALE <- abs(dim(HF_trimmed_model %>% filter(PFEMALE<mean(HF_top30_pc$PFEMALE)))[1]/dim(HF_trimmed_model)[1] - dim(HF_trimmed_model %>% filter(PFEMALE<mean(HF_last30_pc$PFEMALE)))[1]/dim(HF_trimmed_model)[1])
# mean_PBAL <- abs(dim(HF_trimmed_model %>% filter(PFEMALE<mean(HF_top30_pc$PFEMALE)))[1]/dim(HF_trimmed_model)[1] - dim(HF_trimmed_model %>% filter(PFEMALE<mean(HF_last30_pc$PFEMALE)))[1]/dim(HF_trimmed_model)[1])
# mean_MEDVALUE <- abs(dim(HF_trimmed_model %>% filter(MEDVALUE<mean(HF_top30_pc$MEDVALUE)))[1]/dim(HF_trimmed_model)[1] - dim(HF_trimmed_model %>% filter(MEDVALUE<mean(HF_last30_pc$MEDVALUE)))[1]/dim(HF_trimmed_model)[1])
# mean_MEDRENT <- dim(HF_trimmed_model %>% filter(MEDRENT<mean(HF_top30_pc$MEDRENT)))[1]/dim(HF_trimmed_model)[1] - dim(HF_trimmed_model %>% filter(MEDRENT<mean(HF_last30_pc$MEDRENT)))[1]/dim(HF_trimmed_model)[1]
# mean_PVEHAVAI <- abs(dim(HF_trimmed_model %>% filter(PVEHAVAI<mean(HF_top30_pc$PVEHAVAI)))[1]/dim(HF_trimmed_model)[1] - dim(HF_trimmed_model %>% filter(PVEHAVAI<mean(HF_last30_pc$PVEHAVAI)))[1]/dim(HF_trimmed_model)[1])
mean_MDAGE <- abs(dim(HF_trimmed_model %>% filter(MDAGE<mean(HF_top30_pc$MDAGE)))[1]/dim(HF_trimmed_model)[1] - dim(HF_trimmed_model %>% filter(MDAGE<mean(HF_last30_pc$MDAGE)))[1]/dim(HF_trimmed_model)[1])

#dim(HF_trimmed_model %>% filter(MDHHINC<mean(HF_top30_pc$MDHHINC)))[1]/dim(HF_trimmed_model)[1] - dim(HF_trimmed_model %>% filter(MDHHINC<mean(HF_last30_pc$MDHHINC)))[1]/dim(HF_trimmed_model)[1]

#calculate the equity index
sum(mean_MEDHHINC, mean_PWHITE, mean_MDAGE)/3
