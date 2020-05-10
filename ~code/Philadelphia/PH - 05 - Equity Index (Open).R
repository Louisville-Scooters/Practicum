SA_trimmed_model <- SA_trimmed_model %>% na.omit()
SA_trimmed_model$cntpc <- SA_trimmed_model$Predicted.CNT/SA_trimmed_model$TOTPOP#/SA_trimmed_model$area

SA_top30_pc <- SA_trimmed_model %>% subset(SA_trimmed_model$cntpc>quantile(SA_trimmed_model$cntpc,c(0.3,0.7))[2])
SA_last30_pc <- SA_trimmed_model %>% subset(SA_trimmed_model$cntpc<quantile(SA_trimmed_model$cntpc,c(0.3,0.7))[1])

mean_MEDHHINC <- abs(dim(SA_trimmed_model %>% filter(MDHHINC<mean(SA_top30_pc$MDHHINC)))[1]/dim(SA_trimmed_model)[1] - dim(SA_trimmed_model %>% filter(MDHHINC<mean(SA_last30_pc$MDHHINC)))[1]/dim(SA_trimmed_model)[1])
mean_PWHITE <- abs(dim(SA_trimmed_model %>% filter(PWHITE<mean(SA_top30_pc$PWHITE)))[1]/dim(SA_trimmed_model)[1] - dim(SA_trimmed_model %>% filter(PWHITE<mean(SA_last30_pc$PWHITE)))[1]/dim(SA_trimmed_model)[1])
# mean_PFEMALE <- abs(dim(SA_trimmed_model %>% filter(PFEMALE<mean(SA_top30_pc$PFEMALE)))[1]/dim(SA_trimmed_model)[1] - dim(SA_trimmed_model %>% filter(PFEMALE<mean(SA_last30_pc$PFEMALE)))[1]/dim(SA_trimmed_model)[1])
# mean_PBAL <- abs(dim(SA_trimmed_model %>% filter(PFEMALE<mean(SA_top30_pc$PFEMALE)))[1]/dim(SA_trimmed_model)[1] - dim(SA_trimmed_model %>% filter(PFEMALE<mean(SA_last30_pc$PFEMALE)))[1]/dim(SA_trimmed_model)[1])
# mean_MEDVALUE <- abs(dim(SA_trimmed_model %>% filter(MEDVALUE<mean(SA_top30_pc$MEDVALUE)))[1]/dim(SA_trimmed_model)[1] - dim(SA_trimmed_model %>% filter(MEDVALUE<mean(SA_last30_pc$MEDVALUE)))[1]/dim(SA_trimmed_model)[1])
# mean_MEDRENT <- dim(SA_trimmed_model %>% filter(MEDRENT<mean(SA_top30_pc$MEDRENT)))[1]/dim(SA_trimmed_model)[1] - dim(SA_trimmed_model %>% filter(MEDRENT<mean(SA_last30_pc$MEDRENT)))[1]/dim(SA_trimmed_model)[1]
# mean_PVEHAVAI <- abs(dim(SA_trimmed_model %>% filter(PVEHAVAI<mean(SA_top30_pc$PVEHAVAI)))[1]/dim(SA_trimmed_model)[1] - dim(SA_trimmed_model %>% filter(PVEHAVAI<mean(SA_last30_pc$PVEHAVAI)))[1]/dim(SA_trimmed_model)[1])
mean_MDAGE <- abs(dim(SA_trimmed_model %>% filter(MDAGE<mean(SA_top30_pc$MDAGE)))[1]/dim(SA_trimmed_model)[1] - dim(SA_trimmed_model %>% filter(MDAGE<mean(SA_last30_pc$MDAGE)))[1]/dim(SA_trimmed_model)[1])

#dim(SA_trimmed_model %>% filter(MDHHINC<mean(SA_top30_pc$MDHHINC)))[1]/dim(SA_trimmed_model)[1] - dim(SA_trimmed_model %>% filter(MDHHINC<mean(SA_last30_pc$MDHHINC)))[1]/dim(SA_trimmed_model)[1]

#calculate the equity index
sum(mean_MEDHHINC, mean_PWHITE, mean_MDAGE)/3

