OM_trimmed_model <- OM_trimmed_model %>% na.omit()
OM_trimmed_model$cntpc <- OM_trimmed_model$Predicted.CNT/OM_trimmed_model$TOTPOP#/OM_trimmed_model$area

OM_top30_pc <- OM_trimmed_model %>% subset(OM_trimmed_model$cntpc>quantile(OM_trimmed_model$cntpc,c(0.3,0.7))[2])
OM_last30_pc <- OM_trimmed_model %>% subset(OM_trimmed_model$cntpc<quantile(OM_trimmed_model$cntpc,c(0.3,0.7))[1])

mean_MEDHHINC <- abs(dim(OM_trimmed_model %>% filter(MDHHINC<mean(OM_top30_pc$MDHHINC)))[1]/dim(OM_trimmed_model)[1] - dim(OM_trimmed_model %>% filter(MDHHINC<mean(OM_last30_pc$MDHHINC)))[1]/dim(OM_trimmed_model)[1])
mean_PWHITE <- abs(dim(OM_trimmed_model %>% filter(PWHITE<mean(OM_top30_pc$PWHITE)))[1]/dim(OM_trimmed_model)[1] - dim(OM_trimmed_model %>% filter(PWHITE<mean(OM_last30_pc$PWHITE)))[1]/dim(OM_trimmed_model)[1])
# mean_PFEMALE <- abs(dim(OM_trimmed_model %>% filter(PFEMALE<mean(OM_top30_pc$PFEMALE)))[1]/dim(OM_trimmed_model)[1] - dim(OM_trimmed_model %>% filter(PFEMALE<mean(OM_last30_pc$PFEMALE)))[1]/dim(OM_trimmed_model)[1])
# mean_PBAL <- abs(dim(OM_trimmed_model %>% filter(PFEMALE<mean(OM_top30_pc$PFEMALE)))[1]/dim(OM_trimmed_model)[1] - dim(OM_trimmed_model %>% filter(PFEMALE<mean(OM_last30_pc$PFEMALE)))[1]/dim(OM_trimmed_model)[1])
# mean_MEDVALUE <- abs(dim(OM_trimmed_model %>% filter(MEDVALUE<mean(OM_top30_pc$MEDVALUE)))[1]/dim(OM_trimmed_model)[1] - dim(OM_trimmed_model %>% filter(MEDVALUE<mean(OM_last30_pc$MEDVALUE)))[1]/dim(OM_trimmed_model)[1])
# mean_MEDRENT <- dim(OM_trimmed_model %>% filter(MEDRENT<mean(OM_top30_pc$MEDRENT)))[1]/dim(OM_trimmed_model)[1] - dim(OM_trimmed_model %>% filter(MEDRENT<mean(OM_last30_pc$MEDRENT)))[1]/dim(OM_trimmed_model)[1]
# mean_PVEHAVAI <- abs(dim(OM_trimmed_model %>% filter(PVEHAVAI<mean(OM_top30_pc$PVEHAVAI)))[1]/dim(OM_trimmed_model)[1] - dim(OM_trimmed_model %>% filter(PVEHAVAI<mean(OM_last30_pc$PVEHAVAI)))[1]/dim(OM_trimmed_model)[1])
mean_MDAGE <- abs(dim(OM_trimmed_model %>% filter(MDAGE<mean(OM_top30_pc$MDAGE)))[1]/dim(OM_trimmed_model)[1] - dim(OM_trimmed_model %>% filter(MDAGE<mean(OM_last30_pc$MDAGE)))[1]/dim(OM_trimmed_model)[1])

#dim(OM_trimmed_model %>% filter(MDHHINC<mean(OM_top30_pc$MDHHINC)))[1]/dim(OM_trimmed_model)[1] - dim(OM_trimmed_model %>% filter(MDHHINC<mean(OM_last30_pc$MDHHINC)))[1]/dim(OM_trimmed_model)[1]

#calculate the equity index
sum(mean_MEDHHINC, mean_PWHITE, mean_MDAGE)/3

