##########################################################################
# This script is for setting up data files for ML model building. 
# It reads in:
# 1. LV_spatial_census (has 0619 trip origin count by tract + census var + spatial var)
# 2. MNP_
# 3. CH_
# 4. KS_
# 5. 

# This script exports the following data:
# 1. 
##########################################################################

# Reads in data 
LV_spatial_census_RDS <- file.path(data_directory, "~RData/Louisville/LV_spatial_census")
LV_spatial_census <- readRDS(LV_spatial_census_RDS)
MNP_spatial_census_RDS <- file.path(data_directory, "~RData/Louisville/MNP_spatial_census")
MNP_spatial_census <- readRDS(MNP_spatial_census_RDS)

#MNP does not have dest_cnt
LV_spatial_census <- LV_spatial_census %>%
  dplyr::select(-dests_cnt)

# Concat data from all cities together
ML_model_panel <- rbind(LV_spatial_census%>% st_set_geometry(NULL), 
                        MNP_spatial_census%>% st_set_geometry(NULL))

# Try linear regression model

reg1 <- 
  lm(origins_cnt ~  count_college + count_leisure + count_office + count_pubtran
     + count_restaurant + count_retail + count_tourism + TotPop + TotHseUni
     + MdHHInc + MdAge + MedValue + MedRent + pWhite + pTrans + pDrive + pFemale
     + pCom30plus + pVehAvai + city, data= as.data.frame(ML_model_panel))

summary(reg1)

# Split train/test set
inTrain <- createDataPartition (
  y = paste(ML_model_panel$TotHseUni), 
  p = .60, list = FALSE)
ML.train <- ML_model_panel[inTrain,] 
ML.test <- ML_model_panel[-inTrain,]  


