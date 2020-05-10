JC_trimmed_model <- JC_trimmed_model %>% na.omit()
JC_trimmed_model$cntpc <- JC_trimmed_model$Predicted.CNT/JC_trimmed_model$TOTPOP#/JC_trimmed_model$area

JC_top30_pc <- JC_trimmed_model %>% subset(JC_trimmed_model$cntpc>quantile(JC_trimmed_model$cntpc,c(0.3,0.7))[2])
JC_last30_pc <- JC_trimmed_model %>% subset(JC_trimmed_model$cntpc<quantile(JC_trimmed_model$cntpc,c(0.3,0.7))[1])

mean_MEDHHINC <- abs(dim(JC_trimmed_model %>% filter(MDHHINC<mean(JC_top30_pc$MDHHINC)))[1]/dim(JC_trimmed_model)[1] - dim(JC_trimmed_model %>% filter(MDHHINC<mean(JC_last30_pc$MDHHINC)))[1]/dim(JC_trimmed_model)[1])
mean_PWHITE <- abs(dim(JC_trimmed_model %>% filter(PWHITE<mean(JC_top30_pc$PWHITE)))[1]/dim(JC_trimmed_model)[1] - dim(JC_trimmed_model %>% filter(PWHITE<mean(JC_last30_pc$PWHITE)))[1]/dim(JC_trimmed_model)[1])
# mean_PFEMALE <- abs(dim(JC_trimmed_model %>% filter(PFEMALE<mean(JC_top30_pc$PFEMALE)))[1]/dim(JC_trimmed_model)[1] - dim(JC_trimmed_model %>% filter(PFEMALE<mean(JC_last30_pc$PFEMALE)))[1]/dim(JC_trimmed_model)[1])
# mean_PBAL <- abs(dim(JC_trimmed_model %>% filter(PFEMALE<mean(JC_top30_pc$PFEMALE)))[1]/dim(JC_trimmed_model)[1] - dim(JC_trimmed_model %>% filter(PFEMALE<mean(JC_last30_pc$PFEMALE)))[1]/dim(JC_trimmed_model)[1])
# mean_MEDVALUE <- abs(dim(JC_trimmed_model %>% filter(MEDVALUE<mean(JC_top30_pc$MEDVALUE)))[1]/dim(JC_trimmed_model)[1] - dim(JC_trimmed_model %>% filter(MEDVALUE<mean(JC_last30_pc$MEDVALUE)))[1]/dim(JC_trimmed_model)[1])
# mean_MEDRENT <- dim(JC_trimmed_model %>% filter(MEDRENT<mean(JC_top30_pc$MEDRENT)))[1]/dim(JC_trimmed_model)[1] - dim(JC_trimmed_model %>% filter(MEDRENT<mean(JC_last30_pc$MEDRENT)))[1]/dim(JC_trimmed_model)[1]
# mean_PVEHAVAI <- abs(dim(JC_trimmed_model %>% filter(PVEHAVAI<mean(JC_top30_pc$PVEHAVAI)))[1]/dim(JC_trimmed_model)[1] - dim(JC_trimmed_model %>% filter(PVEHAVAI<mean(JC_last30_pc$PVEHAVAI)))[1]/dim(JC_trimmed_model)[1])
mean_MDAGE <- abs(dim(JC_trimmed_model %>% filter(MDAGE<mean(JC_top30_pc$MDAGE)))[1]/dim(JC_trimmed_model)[1] - dim(JC_trimmed_model %>% filter(MDAGE<mean(JC_last30_pc$MDAGE)))[1]/dim(JC_trimmed_model)[1])

#dim(JC_trimmed_model %>% filter(MDHHINC<mean(JC_top30_pc$MDHHINC)))[1]/dim(JC_trimmed_model)[1] - dim(JC_trimmed_model %>% filter(MDHHINC<mean(JC_last30_pc$MDHHINC)))[1]/dim(JC_trimmed_model)[1]

#calculate the equity index
sum(mean_MEDHHINC, mean_PWHITE, mean_MDAGE)/3

