JV_trimmed_model <- JV_trimmed_model %>% na.omit()
JV_trimmed_model$cntpc <- JV_trimmed_model$Predicted.CNT/JV_trimmed_model$TOTPOP#/JV_trimmed_model$area

JV_top30_pc <- JV_trimmed_model %>% subset(JV_trimmed_model$cntpc>quantile(JV_trimmed_model$cntpc,c(0.3,0.7))[2])
JV_last30_pc <- JV_trimmed_model %>% subset(JV_trimmed_model$cntpc<quantile(JV_trimmed_model$cntpc,c(0.3,0.7))[1])

mean_MEDHHINC <- abs(dim(JV_trimmed_model %>% filter(MDHHINC<mean(JV_top30_pc$MDHHINC)))[1]/dim(JV_trimmed_model)[1] - dim(JV_trimmed_model %>% filter(MDHHINC<mean(JV_last30_pc$MDHHINC)))[1]/dim(JV_trimmed_model)[1])
mean_PWHITE <- abs(dim(JV_trimmed_model %>% filter(PWHITE<mean(JV_top30_pc$PWHITE)))[1]/dim(JV_trimmed_model)[1] - dim(JV_trimmed_model %>% filter(PWHITE<mean(JV_last30_pc$PWHITE)))[1]/dim(JV_trimmed_model)[1])
# mean_PFEMALE <- abs(dim(JV_trimmed_model %>% filter(PFEMALE<mean(JV_top30_pc$PFEMALE)))[1]/dim(JV_trimmed_model)[1] - dim(JV_trimmed_model %>% filter(PFEMALE<mean(JV_last30_pc$PFEMALE)))[1]/dim(JV_trimmed_model)[1])
# mean_PBAL <- abs(dim(JV_trimmed_model %>% filter(PFEMALE<mean(JV_top30_pc$PFEMALE)))[1]/dim(JV_trimmed_model)[1] - dim(JV_trimmed_model %>% filter(PFEMALE<mean(JV_last30_pc$PFEMALE)))[1]/dim(JV_trimmed_model)[1])
# mean_MEDVALUE <- abs(dim(JV_trimmed_model %>% filter(MEDVALUE<mean(JV_top30_pc$MEDVALUE)))[1]/dim(JV_trimmed_model)[1] - dim(JV_trimmed_model %>% filter(MEDVALUE<mean(JV_last30_pc$MEDVALUE)))[1]/dim(JV_trimmed_model)[1])
# mean_MEDRENT <- dim(JV_trimmed_model %>% filter(MEDRENT<mean(JV_top30_pc$MEDRENT)))[1]/dim(JV_trimmed_model)[1] - dim(JV_trimmed_model %>% filter(MEDRENT<mean(JV_last30_pc$MEDRENT)))[1]/dim(JV_trimmed_model)[1]
# mean_PVEHAVAI <- abs(dim(JV_trimmed_model %>% filter(PVEHAVAI<mean(JV_top30_pc$PVEHAVAI)))[1]/dim(JV_trimmed_model)[1] - dim(JV_trimmed_model %>% filter(PVEHAVAI<mean(JV_last30_pc$PVEHAVAI)))[1]/dim(JV_trimmed_model)[1])
mean_MDAGE <- abs(dim(JV_trimmed_model %>% filter(MDAGE<mean(JV_top30_pc$MDAGE)))[1]/dim(JV_trimmed_model)[1] - dim(JV_trimmed_model %>% filter(MDAGE<mean(JV_last30_pc$MDAGE)))[1]/dim(JV_trimmed_model)[1])

#dim(JV_trimmed_model %>% filter(MDHHINC<mean(JV_top30_pc$MDHHINC)))[1]/dim(JV_trimmed_model)[1] - dim(JV_trimmed_model %>% filter(MDHHINC<mean(JV_last30_pc$MDHHINC)))[1]/dim(JV_trimmed_model)[1]

#calculate the equity index
sum(mean_MEDHHINC, mean_PWHITE, mean_MDAGE)/3
