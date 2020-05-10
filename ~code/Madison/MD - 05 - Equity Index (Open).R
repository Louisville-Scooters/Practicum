MD_trimmed_model <- MD_trimmed_model %>% na.omit()
MD_trimmed_model$cntpc <- MD_trimmed_model$Predicted.CNT/MD_trimmed_model$TOTPOP#/MD_trimmed_model$area

MD_top30_pc <- MD_trimmed_model %>% subset(MD_trimmed_model$cntpc>quantile(MD_trimmed_model$cntpc,c(0.3,0.7))[2])
MD_last30_pc <- MD_trimmed_model %>% subset(MD_trimmed_model$cntpc<quantile(MD_trimmed_model$cntpc,c(0.3,0.7))[1])

mean_MEDHHINC <- abs(dim(MD_trimmed_model %>% filter(MDHHINC<mean(MD_top30_pc$MDHHINC)))[1]/dim(MD_trimmed_model)[1] - dim(MD_trimmed_model %>% filter(MDHHINC<mean(MD_last30_pc$MDHHINC)))[1]/dim(MD_trimmed_model)[1])
mean_PWHITE <- abs(dim(MD_trimmed_model %>% filter(PWHITE<mean(MD_top30_pc$PWHITE)))[1]/dim(MD_trimmed_model)[1] - dim(MD_trimmed_model %>% filter(PWHITE<mean(MD_last30_pc$PWHITE)))[1]/dim(MD_trimmed_model)[1])
# mean_PFEMALE <- abs(dim(MD_trimmed_model %>% filter(PFEMALE<mean(MD_top30_pc$PFEMALE)))[1]/dim(MD_trimmed_model)[1] - dim(MD_trimmed_model %>% filter(PFEMALE<mean(MD_last30_pc$PFEMALE)))[1]/dim(MD_trimmed_model)[1])
# mean_PBAL <- abs(dim(MD_trimmed_model %>% filter(PFEMALE<mean(MD_top30_pc$PFEMALE)))[1]/dim(MD_trimmed_model)[1] - dim(MD_trimmed_model %>% filter(PFEMALE<mean(MD_last30_pc$PFEMALE)))[1]/dim(MD_trimmed_model)[1])
# mean_MEDVALUE <- abs(dim(MD_trimmed_model %>% filter(MEDVALUE<mean(MD_top30_pc$MEDVALUE)))[1]/dim(MD_trimmed_model)[1] - dim(MD_trimmed_model %>% filter(MEDVALUE<mean(MD_last30_pc$MEDVALUE)))[1]/dim(MD_trimmed_model)[1])
# mean_MEDRENT <- dim(MD_trimmed_model %>% filter(MEDRENT<mean(MD_top30_pc$MEDRENT)))[1]/dim(MD_trimmed_model)[1] - dim(MD_trimmed_model %>% filter(MEDRENT<mean(MD_last30_pc$MEDRENT)))[1]/dim(MD_trimmed_model)[1]
# mean_PVEHAVAI <- abs(dim(MD_trimmed_model %>% filter(PVEHAVAI<mean(MD_top30_pc$PVEHAVAI)))[1]/dim(MD_trimmed_model)[1] - dim(MD_trimmed_model %>% filter(PVEHAVAI<mean(MD_last30_pc$PVEHAVAI)))[1]/dim(MD_trimmed_model)[1])
mean_MDAGE <- abs(dim(MD_trimmed_model %>% filter(MDAGE<mean(MD_top30_pc$MDAGE)))[1]/dim(MD_trimmed_model)[1] - dim(MD_trimmed_model %>% filter(MDAGE<mean(MD_last30_pc$MDAGE)))[1]/dim(MD_trimmed_model)[1])

#dim(MD_trimmed_model %>% filter(MDHHINC<mean(MD_top30_pc$MDHHINC)))[1]/dim(MD_trimmed_model)[1] - dim(MD_trimmed_model %>% filter(MDHHINC<mean(MD_last30_pc$MDHHINC)))[1]/dim(MD_trimmed_model)[1]

#calculate the equity index
sum(mean_MEDHHINC, mean_PWHITE, mean_MDAGE)/3

