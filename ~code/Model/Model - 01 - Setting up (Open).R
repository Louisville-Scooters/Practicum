##########################################################################
# This script is for setting up data files for ML model building. 
# It reads in:
# 1. Model_panel

# This script exports the following data:
# 1. 
##########################################################################

# Reads in data 
Model_panel_RDS <- file.path(data_directory, "~RData/Model_panel")
saveRDS(Model_panel,
        file = Model_panel_RDS)
Model_panel <- readRDS(Model_panel_RDS)

Model_clean <- na.omit(Model_panel)
Model_clean <- Model_clean %>%
  dplyr::select(-c(MEAN_COMMUTE_TIME,GEOID, CENTROID_X, CENTROID_Y, CITY))

Model_clean_RDS <- file.path(data_directory, "~RData/Model_clean")
saveRDS(Model_clean,
        file = Model_clean_RDS)
Model_clean <- readRDS(Model_clean_RDS)

# Try linear regression model
reg1 <- 
  lm(ORIGINS_CNT ~ ., data= as.data.frame(Model_clean))

summary(reg1)

### TidyModel from Matt's class ####
set.seed(717)
theme_set(theme_bw())
"%!in%" <- Negate("%in%")

#create 20 cvID for later 20-fold cross-validation
Model_clean_2 <- Model_clean %>%
  mutate(cvID = sample(round(nrow(Model_clean) / 78), size=nrow(Model_clean), replace = TRUE))

### Initial Split for Training and Test ####
data_split <- initial_split(Model_clean_2, strata = "ORIGINS_CNT", prop = 0.75)
train.set <- training(data_split)
test.set  <- testing(data_split)

### Cross Validation
## LOGOCV on Neighborhood with group_vfold_cv()
cv_splits_geo <- group_vfold_cv(train.set,  strata = "ORIGINS_CNT", group = "cvID")
print(cv_splits_geo)

### Create Recipes ####
# Feature Creation
model_rec <- recipe(ORIGINS_CNT ~ ., data = train.set) %>%
  update_role(cvID, new_role = "cvID") %>%
  step_other(cvID, threshold = 0.005) %>% #pool infrequently occurrin values into an "other" category.
#  step_dummy(all_nominal()) %>%
#  step_log(ORIGINS_CNT) %>%  #has zero, cannot log 
  step_zv(all_predictors()) %>% #remove variables that contain only a single value.
  step_center(all_predictors(), -ORIGINS_CNT) %>% #normalize numeric data to have a mean of zero.
  step_scale(all_predictors(), -ORIGINS_CNT)  #normalize numeric data to have a standard deviation of one.
#  %>% step_ns(Latitude, Longitude, options = list(df = 4)) #create new columns that are basis expan- sions of variables using natural splines.

# See the data after all transformations
glimpse(model_rec %>% prep() %>% juice())  #juice: extract finalized training set
model_rec
linear_reg

# Model specifications
lm_plan <- 
  linear_reg() %>% 
  set_engine("lm") # kerast

glmnet_plan <- 
  linear_reg() %>% 
  set_args(penalty  = tune()) %>%
  set_args(mixture  = tune()) %>%
  set_engine("glmnet")

rf_plan <- 
  rand_forest() %>%
  set_args(mtry  = tune()) %>%
  set_args(min_n = tune()) %>%
  set_args(trees = 1000) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")

#XGB: Extreme Gradient Boosting
XGB_plan <- 
  boost_tree() %>%
  set_args(mtry  = tune()) %>%
  set_args(min_n = tune()) %>%
  set_args(trees = 100) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")


# Hyperparameter grid for glmnet (penalization)
glmnet_grid <- expand.grid(penalty = seq(0, 1, by = .25), 
                           mixture = seq(0,1,0.25))
rf_grid <- expand.grid(mtry = c(2,5), 
                       min_n = c(1,5))  #randowm forest, 
xgb_grid <- expand.grid(mtry = c(3,5), 
                        min_n = c(1,5))

rf_grid

# create workflow
lm_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(lm_plan)
glmnet_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(glmnet_plan)
rf_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(rf_plan)
xgb_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(XGB_plan)

# fit model to workflow and calculate metrics
control <- control_resamples(save_pred = TRUE, verbose = TRUE)

lm_tuned <- lm_wf %>%
  tune::fit_resamples(.,
                      resamples = cv_splits_geo,
                      control   = control,
                      metrics   = metric_set(rmse, rsq))

glmnet_tuned <- glmnet_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo,
                  grid      = glmnet_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

rf_tuned <- rf_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

xgb_tuned <- xgb_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo,
                  grid      = xgb_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

lm_tuned
rf_tuned

## metrics across grid
# autoplot(xgb_tuned)
# collect_metrics(xgb_tuned)
## 'Best' by some metric and margin
show_best(lm_tuned, metric = "rmse", n = 15, maximize = FALSE)
show_best(glmnet_tuned, metric = "rmse", n = 15, maximize = FALSE)
show_best(rf_tuned, metric = "rmse", n = 15, maximize = FALSE)
#min_n = 5, less complex, tree grows down to the level with having 5 obsrvations
#min_n = 1, more complex, tree grows down to the very bottom level 
show_best(xgb_tuned, metric = "rmse", n = 15, maximize = FALSE)

lm_best_params     <- select_best(lm_tuned, metric = "rmse", maximize = FALSE)
glmnet_best_params <- select_best(glmnet_tuned, metric = "rmse", maximize = FALSE)
rf_best_params     <- select_best(rf_tuned, metric = "rmse", maximize = FALSE)
xgb_best_params    <- select_best(xgb_tuned, metric = "rmse", maximize = FALSE)

## Final workflow
lm_best_wf     <- finalize_workflow(lm_wf, lm_best_params)
glmnet_best_wf <- finalize_workflow(glmnet_wf, glmnet_best_params)
rf_best_wf     <- finalize_workflow(rf_wf, rf_best_params)
xgb_best_wf    <- finalize_workflow(xgb_wf, xgb_best_params)


# last_fit() emulates the process where, after determining the best model, the final fit on the entire training set is needed and is then evaluated on the test set.
lm_val_fit_geo <- lm_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))

glmnet_val_fit_geo <- glmnet_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_fit_geo <- rf_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))

xgb_val_fit_geo <- xgb_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))
