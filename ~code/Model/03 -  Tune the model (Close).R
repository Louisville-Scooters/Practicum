## normalization
Model_clean_area$ORIGINS_CNT <- Model_clean_area$ORIGINS_CNT/Model_clean_area$area
Model_clean_area$TOTPOP <- Model_clean_area$TOTPOP/Model_clean_area$area
# Model_clean_area <- Model_clean_area%>% st_set_geometry(NULL)

osm_features <- c("RATIO_RETAIL","RATIO_OFFICE", "RATIO_RESTAURANT", "RATIO_PUBLIC_TRANSPORT",
                  "RATIO_LEISURE","RATIO_TOURISM", "RATIO_COLLEGE","RATIO_CYCLEWAY","RATIO_STREET",
                  "JOBS_IN_TRACT", "WORKERS_IN_TRACT")

census_features <- c("TOTPOP", "TOTHSEUNI", "MDHHINC",              
                     "MDAGE", "MEDVALUE",          
                     "MEDRENT", "PWHITE",                
                     "PTRANS", "PDRIVE",                
                     "PFEMALE", "PCOM30PLUS",            
                     "POCCUPIED", "PVEHAVAI")

# with 0
# one model ####
data_split <- initial_split(Model_clean %>% dplyr::select(-GEOID), strata = "ORIGINS_CNT", prop = 0.75)
train.set <- training(data_split)
test.set  <- testing(data_split)

model1 <- randomForest(ORIGINS_CNT ~ ., data = train.set %>% dplyr::select(-CITY),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)

# Predicting on train set
train.set$pred_rf <- predict(model1, train.set, type = "class")
train.set$AE_rf <- abs(train.set$pred_rf - train.set$ORIGINS_CNT)
train.set$Error_rf <- train.set$pred_rf - train.set$ORIGINS_CNT
mean(train.set$AE_rf)
mean(train.set$ORIGINS_CNT)

test.set$pred_rf <- predict(model1, test.set, type = "class")
test.set$AE_rf <- abs(test.set$pred_rf - test.set$ORIGINS_CNT)
test.set$Error_rf <- test.set$pred_rf - test.set$ORIGINS_CNT
mean(test.set$AE_rf)
mean(test.set$ORIGINS_CNT)


# two models ####
data_split <- initial_split(Model_clean %>% dplyr::select(-GEOID), strata = "ORIGINS_CNT", prop = 0.75)
train.set <- training(data_split)
test.set  <- testing(data_split)


model1 <- randomForest(ORIGINS_CNT ~ ., data = as.data.frame(train.set) %>% dplyr::select(osm_features, ORIGINS_CNT),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)

model2 <- randomForest(ORIGINS_CNT ~ ., data = as.data.frame(train.set) %>% dplyr::select(census_features, ORIGINS_CNT),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)

# Predicting on train set
train.set$pred_OSM <- predict(model1, train.set, type = "class")
train.set$pred_census <- predict(model2, train.set, type = "class")
test.set$pred_OSM <- predict(model1, test.set, type = "class")
test.set$pred_census <- predict(model2, test.set, type = "class")

model3 <- randomForest(ORIGINS_CNT ~ ., data = as.data.frame(train.set) %>% dplyr::select(pred_OSM, pred_census, ORIGINS_CNT),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)
train.set$pred_final <- predict(model3, train.set, type = "class")
train.set$AE_rf <- abs(train.set$pred_final - train.set$ORIGINS_CNT)
train.set$Error_rf <- train.set$pred_final - train.set$ORIGINS_CNT
mean(train.set$AE_rf)
mean(train.set$ORIGINS_CNT)

test.set$pred_final <- predict(model3, test.set, type = "class")
test.set$AE_rf <- abs(test.set$pred_final - test.set$ORIGINS_CNT)
test.set$Error_rf <- test.set$pred_final - test.set$ORIGINS_CNT
mean(test.set$AE_rf)
mean(test.set$ORIGINS_CNT)


# WITHOUT 0
# one model ####
data_split <- initial_split(Model_clean %>% dplyr::select(-GEOID), strata = "ORIGINS_CNT", prop = 0.75)
train.set <- training(data_split)
test.set  <- testing(data_split)

train.set <- train.set %>% subset(train.set$ORIGINS_CNT!=0)
test.set <- test.set %>% subset(test.set$ORIGINS_CNT!=0)

model1 <- randomForest(ORIGINS_CNT ~ ., data = train.set %>% dplyr::select(-CITY),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)

# Predicting on train set
train.set$pred_rf <- predict(model1, train.set, type = "class")
train.set$AE_rf <- abs(train.set$pred_rf - train.set$ORIGINS_CNT)
train.set$Error_rf <- train.set$pred_rf - train.set$ORIGINS_CNT
mean(train.set$AE_rf)
mean(train.set$ORIGINS_CNT)

test.set$pred_rf <- predict(model1, test.set, type = "class")
test.set$AE_rf <- abs(test.set$pred_rf - test.set$ORIGINS_CNT)
test.set$Error_rf <- test.set$pred_rf - test.set$ORIGINS_CNT
mean(test.set$AE_rf)
mean(test.set$ORIGINS_CNT)


# two models ####
data_split <- initial_split(Model_clean %>% dplyr::select(-GEOID), strata = "ORIGINS_CNT", prop = 0.75)
train.set <- training(data_split)
test.set  <- testing(data_split)

train.set <- train.set %>% subset(train.set$ORIGINS_CNT!=0)
test.set <- test.set %>% subset(test.set$ORIGINS_CNT!=0)

model1 <- randomForest(ORIGINS_CNT ~ ., data = as.data.frame(train.set) %>% dplyr::select(osm_features, ORIGINS_CNT),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)

model2 <- randomForest(ORIGINS_CNT ~ ., data = as.data.frame(train.set) %>% dplyr::select(census_features, ORIGINS_CNT),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)

# Predicting on train set
train.set$pred_OSM <- predict(model1, train.set, type = "class")
train.set$pred_census <- predict(model2, train.set, type = "class")
test.set$pred_OSM <- predict(model1, test.set, type = "class")
test.set$pred_census <- predict(model2, test.set, type = "class")

model3 <- randomForest(ORIGINS_CNT ~ ., data = as.data.frame(train.set) %>% dplyr::select(pred_OSM, pred_census, ORIGINS_CNT),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)
train.set$pred_final <- predict(model3, train.set, type = "class")
train.set$AE_rf <- abs(train.set$pred_final - train.set$ORIGINS_CNT)
train.set$Error_rf <- train.set$pred_final - train.set$ORIGINS_CNT
mean(train.set$AE_rf)
mean(train.set$ORIGINS_CNT)

test.set$pred_final <- predict(model3, test.set, type = "class")
test.set$AE_rf <- abs(test.set$pred_final - test.set$ORIGINS_CNT)
test.set$Error_rf <- test.set$pred_final - test.set$ORIGINS_CNT
mean(test.set$AE_rf)
mean(test.set$ORIGINS_CNT)


# LOGO - CITY
## ALL DATA
data_split <- initial_split(Model_clean %>% dplyr::select(-GEOID), strata = "ORIGINS_CNT", prop = 0.75)
train.set <- training(data_split)
test.set  <- testing(data_split)
train.set <- train.set %>% dplyr::select(osm_features, CITY, ORIGINS_CNT)

model_rec <- recipe(ORIGINS_CNT ~ ., data = train.set) %>%
  update_role(CITY, new_role = "CITY") %>%
  step_other(CITY, threshold = 0.005) %>% #pool infrequently occurrin values into an "other" category.
  step_dummy(all_nominal(), -CITY) %>%
  #  step_log(ORIGINS_CNT) %>%  #has zero, cannot log 
  step_zv(all_predictors()) %>% #remove variables that contain only a single value.
  step_center(all_predictors(), -ORIGINS_CNT) %>% #normalize numeric data to have a mean of zero.
  step_scale(all_predictors(), -ORIGINS_CNT)  #normalize numeric data to have a standard deviation of one.

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
control <- control_resamples(save_pred = TRUE, verbose = TRUE)

rf_tuned <- rf_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

rf_best_params     <- select_best(rf_tuned, metric = "rmse", maximize = FALSE)

rf_best_wf     <- finalize_workflow(rf_wf, rf_best_params)

rf_val_fit_geo <- rf_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_best_OOF_preds <- collect_predictions(rf_tuned) %>% 
  filter(mtry  == rf_best_params$mtry[1] & min_n == rf_best_params$min_n[1])

rf_val_pred_geo     <- collect_predictions(rf_val_fit_geo)

OOF_preds <- data.frame(dplyr::select(rf_best_OOF_preds, .pred, ORIGINS_CNT), model = "RF") %>%
  mutate(#.pred = exp(.pred),
    # ORIGINS_CNT = exp(ORIGINS_CNT),
    RMSE = yardstick::rmse_vec(ORIGINS_CNT, .pred),
    MAE  = yardstick::mae_vec(ORIGINS_CNT, .pred),
    MAPE = yardstick::mape_vec(ORIGINS_CNT, .pred))

val_preds <- data.frame(rf_val_pred_geo, model = "rf") %>%
  left_join(., Model_clean %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(CITY, .row), 
            by = ".row") %>%
  mutate(# .pred = exp(.pred),
    # ORIGINS_CNT = exp(ORIGINS_CNT),
    RMSE = yardstick::rmse_vec(ORIGINS_CNT, .pred),
    MAE  = yardstick::mae_vec(ORIGINS_CNT, .pred),
    MAPE = yardstick::mape_vec(ORIGINS_CNT, .pred))