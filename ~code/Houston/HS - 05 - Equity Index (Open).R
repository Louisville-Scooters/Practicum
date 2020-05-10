HS_trimmed_model <- HS_trimmed_model %>% na.omit()
HS_trimmed_model$cntpc <- HS_trimmed_model$Predicted.CNT/HS_trimmed_model$TOTPOP#/HS_trimmed_model$area

HS_top30_pc <- HS_trimmed_model %>% subset(HS_trimmed_model$cntpc>quantile(HS_trimmed_model$cntpc,c(0.3,0.7))[2])
HS_last30_pc <- HS_trimmed_model %>% subset(HS_trimmed_model$cntpc<quantile(HS_trimmed_model$cntpc,c(0.3,0.7))[1])

mean_MEDHHINC <- abs(dim(HS_trimmed_model %>% filter(MDHHINC<mean(HS_top30_pc$MDHHINC)))[1]/dim(HS_trimmed_model)[1] - dim(HS_trimmed_model %>% filter(MDHHINC<mean(HS_last30_pc$MDHHINC)))[1]/dim(HS_trimmed_model)[1])
mean_PWHITE <- abs(dim(HS_trimmed_model %>% filter(PWHITE<mean(HS_top30_pc$PWHITE)))[1]/dim(HS_trimmed_model)[1] - dim(HS_trimmed_model %>% filter(PWHITE<mean(HS_last30_pc$PWHITE)))[1]/dim(HS_trimmed_model)[1])
# mean_PFEMALE <- abs(dim(HS_trimmed_model %>% filter(PFEMALE<mean(HS_top30_pc$PFEMALE)))[1]/dim(HS_trimmed_model)[1] - dim(HS_trimmed_model %>% filter(PFEMALE<mean(HS_last30_pc$PFEMALE)))[1]/dim(HS_trimmed_model)[1])
# mean_PBAL <- abs(dim(HS_trimmed_model %>% filter(PFEMALE<mean(HS_top30_pc$PFEMALE)))[1]/dim(HS_trimmed_model)[1] - dim(HS_trimmed_model %>% filter(PFEMALE<mean(HS_last30_pc$PFEMALE)))[1]/dim(HS_trimmed_model)[1])
# mean_MEDVALUE <- abs(dim(HS_trimmed_model %>% filter(MEDVALUE<mean(HS_top30_pc$MEDVALUE)))[1]/dim(HS_trimmed_model)[1] - dim(HS_trimmed_model %>% filter(MEDVALUE<mean(HS_last30_pc$MEDVALUE)))[1]/dim(HS_trimmed_model)[1])
# mean_MEDRENT <- dim(HS_trimmed_model %>% filter(MEDRENT<mean(HS_top30_pc$MEDRENT)))[1]/dim(HS_trimmed_model)[1] - dim(HS_trimmed_model %>% filter(MEDRENT<mean(HS_last30_pc$MEDRENT)))[1]/dim(HS_trimmed_model)[1]
# mean_PVEHAVAI <- abs(dim(HS_trimmed_model %>% filter(PVEHAVAI<mean(HS_top30_pc$PVEHAVAI)))[1]/dim(HS_trimmed_model)[1] - dim(HS_trimmed_model %>% filter(PVEHAVAI<mean(HS_last30_pc$PVEHAVAI)))[1]/dim(HS_trimmed_model)[1])
mean_MDAGE <- abs(dim(HS_trimmed_model %>% filter(MDAGE<mean(HS_top30_pc$MDAGE)))[1]/dim(HS_trimmed_model)[1] - dim(HS_trimmed_model %>% filter(MDAGE<mean(HS_last30_pc$MDAGE)))[1]/dim(HS_trimmed_model)[1])

#dim(HS_trimmed_model %>% filter(MDHHINC<mean(HS_top30_pc$MDHHINC)))[1]/dim(HS_trimmed_model)[1] - dim(HS_trimmed_model %>% filter(MDHHINC<mean(HS_last30_pc$MDHHINC)))[1]/dim(HS_trimmed_model)[1]

#calculate the equity index
sum(mean_MEDHHINC, mean_PWHITE, mean_MDAGE)/3
