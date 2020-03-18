##########################################################
# This script follows after Model - 01 - Setting up

# Some notes:
# I didn't change much from Matt's code, BUT I did get Inf values for MAPE of all models
# Still figuring out why - note that I didn't log(ORIGINS_CNT) so I muted the code to exp(pred) and to exp(ORIGINS_CNT) back
##########################################################

# Pull best hyperparam preds from out-of-fold predictions
lm_best_OOF_preds <- collect_predictions(lm_tuned) 

glmnet_best_OOF_preds <- collect_predictions(glmnet_tuned) %>% 
  filter(penalty  == glmnet_best_params$penalty[1] & mixture == glmnet_best_params$mixture[1])

rf_best_OOF_preds <- collect_predictions(rf_tuned) %>% 
  filter(mtry  == rf_best_params$mtry[1] & min_n == rf_best_params$min_n[1])

xgb_best_OOF_preds <- collect_predictions(xgb_tuned) %>% 
  filter(mtry  == xgb_best_params$mtry[1] & min_n == xgb_best_params$min_n[1])

# collect validation set predictions from last_fit model
lm_val_pred_geo     <- collect_predictions(lm_val_fit_geo)
glmnet_val_pred_geo <- collect_predictions(glmnet_val_fit_geo)
rf_val_pred_geo     <- collect_predictions(rf_val_fit_geo)
xgb_val_pred_geo    <- collect_predictions(xgb_val_fit_geo)

# Aggregate OOF predictions (they do not overlap with Validation prediction set)
OOF_preds <- rbind(data.frame(dplyr::select(lm_best_OOF_preds, .pred, ORIGINS_CNT), model = "lm"),
                   data.frame(dplyr::select(glmnet_best_OOF_preds, .pred, ORIGINS_CNT), model = "glmnet"),
                   data.frame(dplyr::select(rf_best_OOF_preds, .pred, ORIGINS_CNT), model = "RF"),
                   data.frame(dplyr::select(xgb_best_OOF_preds, .pred, ORIGINS_CNT), model = "xgb")) %>% 
  group_by(model) %>% 
  mutate(#.pred = exp(.pred),
         # ORIGINS_CNT = exp(ORIGINS_CNT),
         RMSE = yardstick::rmse_vec(ORIGINS_CNT, .pred),
         MAE  = yardstick::mae_vec(ORIGINS_CNT, .pred),
         MAPE = yardstick::mape_vec(ORIGINS_CNT, .pred)) %>% 
  ungroup()

#install.packages('ggsn')
library(ggsn)

# average error for each model
ggplot(data = OOF_preds %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "red") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  theme_bw()

# OOF predicted versus actual
ggplot(OOF_preds, aes(x =.pred, y = ORIGINS_CNT, group = model)) +
  geom_point(alpha = 0.3) +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  coord_equal() +
  facet_wrap(~model, nrow = 2) +
  theme_bw()


# Aggregate predictions from Validation set
val_preds <- rbind(data.frame(lm_val_pred_geo, model = "lm"),
                   data.frame(glmnet_val_pred_geo, model = "glmnet"),
                   data.frame(rf_val_pred_geo, model = "rf"),
                   data.frame(xgb_val_pred_geo, model = "xgb")) %>% 
  left_join(., Model_clean_2 %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(cvID, .row), 
            by = ".row") %>% 
  group_by(model) %>%
  mutate(# .pred = exp(.pred),
         # ORIGINS_CNT = exp(ORIGINS_CNT),
         RMSE = yardstick::rmse_vec(ORIGINS_CNT, .pred),
         MAE  = yardstick::mae_vec(ORIGINS_CNT, .pred),
         MAPE = yardstick::mape_vec(ORIGINS_CNT, .pred)) %>% 
  ungroup()

# plot MAPE by model type
ggplot(data = val_preds %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "red") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  theme_bw()

# Validation Predicted vs. actual
ggplot(val_preds, aes(x =.pred, y = ORIGINS_CNT, group = model)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
 # coord_equal() +
  facet_wrap(~model, nrow = 2) +
  theme_bw()

# # join test data back to make spatial
# val_pred_sf <- val_preds %>% 
#   rowwise() %>% 
#   mutate(RMSE = yardstick::rmse_vec(ORIGINS_CNT, .pred),
#          MAE  = yardstick::mae_vec(ORIGINS_CNT, .pred),
#          MAPE = yardstick::mape_vec(ORIGINS_CNT, .pred)) %>% 
#   st_as_sf(., coords = c("Longitude", "Latitude"),
#            remove = FALSE,
#            crs = 4326)

# map errors by point
# mapview(filter(val_pred_sf, model == "rf"), zcol = "MAPE")

# # aggregate val error to Neighborhood 
# val_MAPE_by_hood <- val_preds %>% 
#   group_by(Neighborhood, model) %>% 
#   summarise(RMSE = yardstick::rmse_vec(ORIGINS_CNT, .pred),
#             MAE  = yardstick::mae_vec(ORIGINS_CNT, .pred),
#             MAPE = yardstick::mape_vec(ORIGINS_CNT, .pred)) %>% 
#   ungroup() 
# 
# # plot MAPE by Hood
# ggplot(filter(val_MAPE_by_hood, model == "rf") %>% 
#          mutate(Neighborhood = fct_reorder(Neighborhood, MAPE)),
#        aes(x = Neighborhood, y = MAPE)) +
#   geom_bar(stat = "identity") +
#   scale_y_continuous(breaks = seq(0,75,5)) +
#   theme_bw() +
#   theme(
#     axis.text.x = element_text(angle = -45, hjust = 0)
#   )







