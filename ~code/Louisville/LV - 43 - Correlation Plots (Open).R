##########################################################################
# This script contains our correlation plots:
# 1. Open Data origins and destinations and census variables
##########################################################################

# Open Data Origins and Destinations and Census Variables
LV_corr_mat <- cor(LV_open_ct %>% 
                  st_drop_geometry() %>% 
                    na.omit() %>% 
                    dplyr::select(-centroid_X, -centroid_Y) %>% 
                    dplyr::select_if(is.numeric))

LV_open_ct_corrplot <- corrplot(LV_corr_mat,
                                method = "color")

