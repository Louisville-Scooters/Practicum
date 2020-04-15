census_var <- colnames(Model_clean)[2:16]
osm_var <- colnames(Model_clean)[c(15:27)]

Model_clean_census <- Model_clean %>% dplyr::select(census_var)
Model_clean_osm <- Model_clean %>% dplyr::select(osm_var)

# train the model based on each of the model data
# I only focus on the rf model
#### FOR CENSUS DATA ####
# split the data into training and test
data_split <- initial_split(Model_clean, 
                            strata = "ORIGINS_CNT", prop = 0.75)
train.set.census <- training(data_split) %>% dplyr::select(census_var)
test.set.census  <- testing(data_split) %>% dplyr::select(census_var)


### Cross Validation
## LOGOCV on City with group_vfold_cv()
cv_splits_geo <- group_vfold_cv(train.set.census,  strata = "ORIGINS_CNT", group = "CITY")

### Create Recipes ###
# Feature Creation
model_rec <- recipe(ORIGINS_CNT ~ ., data = train.set.census) %>%
  update_role(CITY, new_role = "CITY") %>%
  step_other(CITY, threshold = 0.005) %>% #pool infrequently occurrin values into an "other" category.
  step_dummy(all_nominal(), -CITY) %>%
  #  step_log(ORIGINS_CNT) %>%  #has zero, cannot log 
  step_zv(all_predictors()) %>% #remove variables that contain only a single value.
  step_center(all_predictors(), -ORIGINS_CNT) %>% #normalize numeric data to have a mean of zero.
  step_scale(all_predictors(), -ORIGINS_CNT)  #normalize numeric data to have a standard deviation of one.
#  %>% step_ns(Latitude, Longitude, options = list(df = 4)) #create new columns that are basis expan- sions of variables using natural splines.


rf_plan <- 
  rand_forest() %>%
  set_args(mtry  = tune()) %>%
  set_args(min_n = tune()) %>%
  set_args(trees = 1000) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")

rf_grid <- expand.grid(mtry = c(2,5), 
                       min_n = c(1,5)) 

rf_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(rf_plan)

# fit model to workflow and calculate metrics
control <- control_resamples(save_pred = TRUE, verbose = TRUE)


rf_tuned <- rf_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))
show_best(rf_tuned, metric = "rmse", n = 15, maximize = FALSE)
rf_best_params     <- select_best(rf_tuned, metric = "rmse", maximize = FALSE)

rf_best_wf <- finalize_workflow(rf_wf, rf_best_params)

rf_val_fit_geo_census <- rf_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_best_OOF_preds_census <- collect_predictions(rf_tuned) %>% 
  filter(mtry  == rf_best_params$mtry[1] & min_n == rf_best_params$min_n[1])

rf_val_pred_geo_census <- collect_predictions(rf_val_fit_geo_census)

# compare the pred and actual data
OOF_preds_census <- data.frame(dplyr::select(rf_best_OOF_preds_census, .pred, ORIGINS_CNT)) %>%
  mutate(#.pred = exp(.pred),
    # ORIGINS_CNT = exp(ORIGINS_CNT),
    RMSE = yardstick::rmse_vec(ORIGINS_CNT, .pred),
    MAE  = yardstick::mae_vec(ORIGINS_CNT, .pred))
    #MAPE = yardstick::mape_vec(ORIGINS_CNT, .pred))

# OOF predicted versus actual
ggplot(OOF_preds_census, aes(x =.pred, y = ORIGINS_CNT)) +
  geom_point(alpha = 0.3) +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "orange") +
  coord_equal() +
  xlim(0,60000)+
  ylim(0,60000)+
  labs(title="Model Performance (OOF predictions, Cross-validation based on city)")+
  theme_bw()

val_preds_census <- data.frame(rf_val_pred_geo_census, model = "rf") %>% 
  left_join(., Model_clean_census %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(CITY, .row), 
            by = ".row") %>%
  mutate(# .pred = exp(.pred),
    # ORIGINS_CNT = exp(ORIGINS_CNT),
    RMSE = yardstick::rmse_vec(ORIGINS_CNT, .pred),
    MAE  = yardstick::mae_vec(ORIGINS_CNT, .pred))
#MAPE = yardstick::mape_vec(ORIGINS_CNT, .pred))

# Validation Predicted vs. actual
ggplot(val_preds_census, aes(x =.pred, y = ORIGINS_CNT)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "orange") +
  coord_equal() +
  xlim(0,35000)+
  ylim(0,35000)+
  labs(title="Model Performance (Validation predictions, Cross-validation based on city)")+
  theme_bw()

#========================================================================================================#
#========================================================================================================#
#========================================================================================================#

#### FOR OSM DATA ####
# split the data into training and test
train.set.osm <- training(data_split) %>% dplyr::select(osm_var)
test.set.osm  <- testing(data_split) %>% dplyr::select(osm_var)


### Cross Validation
## LOGOCV on City with group_vfold_cv()
cv_splits_geo <- group_vfold_cv(train.set.osm,  strata = "ORIGINS_CNT", group = "CITY")

### Create Recipes ###
# Feature Creation
model_rec <- recipe(ORIGINS_CNT ~ ., data = train.set.osm) %>%
  update_role(CITY, new_role = "CITY") %>%
  step_other(CITY, threshold = 0.005) %>% #pool infrequently occurrin values into an "other" category.
  step_dummy(all_nominal(), -CITY) %>%
  #  step_log(ORIGINS_CNT) %>%  #has zero, cannot log 
  step_zv(all_predictors()) %>% #remove variables that contain only a single value.
  step_center(all_predictors(), -ORIGINS_CNT) %>% #normalize numeric data to have a mean of zero.
  step_scale(all_predictors(), -ORIGINS_CNT)  #normalize numeric data to have a standard deviation of one.
#  %>% step_ns(Latitude, Longitude, options = list(df = 4)) #create new columns that are basis expan- sions of variables using natural splines.


rf_plan <- 
  rand_forest() %>%
  set_args(mtry  = tune()) %>%
  set_args(min_n = tune()) %>%
  set_args(trees = 1000) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")

rf_grid <- expand.grid(mtry = c(2,5), 
                       min_n = c(1,5)) 

rf_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(rf_plan)

# fit model to workflow and calculate metrics
control <- control_resamples(save_pred = TRUE, verbose = TRUE)


rf_tuned <- rf_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))
show_best(rf_tuned, metric = "rmse", n = 15, maximize = FALSE)
rf_best_params     <- select_best(rf_tuned, metric = "rmse", maximize = FALSE)

rf_best_wf     <- finalize_workflow(rf_wf, rf_best_params)

rf_val_fit_geo_osm <- rf_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_pred_geo_osm  <- collect_predictions(rf_val_fit_geo_osm)

rf_best_OOF_preds_osm <- collect_predictions(rf_tuned) %>% 
  filter(mtry  == rf_best_params$mtry[1] & min_n == rf_best_params$min_n[1])

# compare the pred and actual data
OOF_preds_osm <- data.frame(dplyr::select(rf_best_OOF_preds_osm, .pred, ORIGINS_CNT)) %>%
  mutate(#.pred = exp(.pred),
    # ORIGINS_CNT = exp(ORIGINS_CNT),
    RMSE = yardstick::rmse_vec(ORIGINS_CNT, .pred),
    MAE  = yardstick::mae_vec(ORIGINS_CNT, .pred))
    #MAPE = yardstick::mape_vec(ORIGINS_CNT, .pred))
    
# OOF predicted versus actual
ggplot(OOF_preds_osm, aes(x =.pred, y = ORIGINS_CNT)) +
  geom_point(alpha = 0.3) +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "orange") +
  coord_equal() +
  xlim(0,60000)+
  ylim(0,60000)+
  labs(title="Model Performance (OOF predictions, Cross-validation based on city)")+
  theme_bw()

val_preds_osm <- data.frame(rf_val_pred_geo_osm, model = "rf") %>% 
  left_join(., Model_clean_osm %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(CITY, .row), 
            by = ".row") %>%
  mutate(# .pred = exp(.pred),
    # ORIGINS_CNT = exp(ORIGINS_CNT),
    RMSE = yardstick::rmse_vec(ORIGINS_CNT, .pred),
    MAE  = yardstick::mae_vec(ORIGINS_CNT, .pred))
    #MAPE = yardstick::mape_vec(ORIGINS_CNT, .pred))

# Validation Predicted vs. actual
ggplot(val_preds_osm, aes(x =.pred, y = ORIGINS_CNT)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "orange") +
  coord_equal() +
  xlim(0,50000)+
  ylim(0,50000)+
  labs(title="Model Performance (Validation predictions, Cross-validation based on city)")+
  theme_bw()


## random forest done by library 
# combine the two prediction result to predict the final count
final_data <- data.frame(rf_val_pred_geo_osm) %>% dplyr::select(.pred, ORIGINS_CNT)
final_data$.pred_census <- data.frame(rf_val_pred_geo_census)$.pred

data_split <- initial_split(final_data, strata = "ORIGINS_CNT", prop = 0.75)
train.set <- training(data_split)
test.set  <- testing(data_split)

model <- randomForest(ORIGINS_CNT ~ ., data = train.set,
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)

Model_test <- test.set %>%
  mutate(Predicted.CNT = predict(model, test.set, type = "class"), 
         AE = abs(Predicted.CNT - ORIGINS_CNT), 
         Error = Predicted.CNT - ORIGINS_CNT)
mean(Model_test$AE)
mean(Model_test$ORIGINS_CNT)

Model_train <- train.set %>%
  mutate(Predicted.CNT = predict(model, train.set, type = "class"), 
         AE = abs(Predicted.CNT - ORIGINS_CNT), 
         Error = Predicted.CNT - ORIGINS_CNT)
mean(Model_train$AE)
mean(Model_train$ORIGINS_CNT)


