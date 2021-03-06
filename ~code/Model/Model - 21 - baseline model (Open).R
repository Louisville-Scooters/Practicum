############################################
# This script build the random forest model using library randomForest and 
# do the cross validation (group by city).
# Input data:
# 1. Model_clean, obtained from Model - 10
############################################

# install.packages("randomForest")
library(randomForest)
Model_panel_RDS <- file.path(data_directory, "~RData/Model_panel")
#saveRDS(Model_panel,
#        file = Model_panel_RDS)
Model_panel <- readRDS(Model_panel_RDS)

Model_clean <- na.omit(Model_panel)
Model_clean <- Model_clean %>%
  dplyr::select(-c(MEAN_COMMUTE_TIME, CENTROID_X, CENTROID_Y),-starts_with('DENSITY'), -starts_with('KNN'), -starts_with('COUNT'), -ends_with('LENGTH'))

Model_clean <- Model_clean %>% mutate(race = ifelse(PWHITE > .5, "Majority_White", "Majority_Non_White"))

model1 <- randomForest(ORIGINS_CNT ~ ., data = Model_clean %>% dplyr::select(-GEOID, -CITY, -race),
                       ntree = 1000, 
                       mtry = 2, engine = 'ranger', importance = TRUE)

# Predicting on train set
Model_rf <- Model_clean %>%
  mutate(Predicted.CNT = predict(model1, Model_clean, type = "class"), 
         AE = abs(Predicted.CNT - ORIGINS_CNT), 
         Error = Predicted.CNT - ORIGINS_CNT)

mean(Model_rf$AE)

basic_model <- Model_rf %>%
  dplyr::select(GEOID, ORIGINS_CNT, Predicted.CNT, AE, CITY, race, Error)

model_evaluation <- basic_model %>%
  group_by(CITY) %>%
  summarise(Mean_AE = mean(AE),
            mean_CNT = mean(ORIGINS_CNT),
            median_Error = median(AE),
            max_Error = max(AE)
            )

race_content <- basic_model %>%
  group_by(CITY,race) %>%
  summarise(MeanError = mean(Error),
            mean_CNT = mean(ORIGINS_CNT)
  )

grid.arrange(
model_evaluation %>%
  ggplot(aes(CITY, Mean_AE)) + 
  geom_bar(aes(fill = CITY), position = "dodge", stat="identity") +
 # scale_fill_manual(values = palette5) +
  labs(title = "Mean Absolute Errors by city") +
  theme(axis.text.x = element_text(size=10, angle= 45))+
  plotTheme,

model_evaluation %>%
  ggplot(aes(CITY, median_Error)) + 
  geom_bar(aes(fill = CITY), position = "dodge", stat="identity") +
  # scale_fill_manual(values = palette5) +
  theme(axis.text.x = element_text(size=10, angle= 45))+
  labs(title = "Median Errors by city") +
  plotTheme,
ncol = 2)

race_content %>% #specify data
  dplyr::select(-mean_CNT) %>%
  dcast(CITY~race, Value.var = "MeanError") %>%
  kable(caption = "Mean Error by racial context in each city") %>%
  kable_styling("striped", full_width = F)

### glmnet ####
library(glmnet)
model2 <- glmnet(as.matrix(Model_clean %>% dplyr::select(-GEOID, -CITY, -race, -ORIGINS_CNT)), as.matrix(Model_clean['ORIGINS_CNT']), penalty=0,mixture=0)

Model_glm <- Model_clean 

Model_glm$Predicted.CNT <- predict(model2, as.matrix(Model_clean %>% dplyr::select(-GEOID, -CITY, -race, -ORIGINS_CNT)))
Model_glm$AE<- abs(Model_glm$Predicted.CNT - Model_glm$ORIGINS_CNT)
Model_glm$Error <- Model_glm$Predicted.CNT - Model_glm$ORIGINS_CNT
mean(Model_glm$AE)

basic_model_glm <- Model_glm %>%
  dplyr::select(GEOID, ORIGINS_CNT, Predicted.CNT, AE, CITY, race, Error)

model_evaluation_glm <- basic_model_glm %>%
  group_by(CITY) %>%
  summarise(Mean_AE = mean(AE),
            mean_CNT = mean(ORIGINS_CNT),
            median_Error = median(AE),
            max_Error = max(AE)
  )

race_content_glm <- basic_model_glm %>%
  group_by(CITY,race) %>%
  summarise(MeanError = mean(Error),
            mean_CNT = mean(ORIGINS_CNT)
  )

####### LOGO #### BY CITY ####
library(rsample)
library(recipes)
library(parsnip)
library(workflows)
library(tune)
library(yardstick)

set.seed(717)
theme_set(theme_bw())
"%!in%" <- Negate("%in%")

### Initial Split for Training and Test ###
data_split <- initial_split(Model_clean %>% 
                              dplyr::select(-GEOID, -race,
                                            -starts_with('DENSITY'), 
                                            -starts_with('KNN'), -starts_with('COUNT'), 
                                            -ends_with('LENGTH')), 
                            strata = "ORIGINS_CNT", prop = 0.75)
train.set <- training(data_split)
test.set  <- testing(data_split)
names(train.set)

### Cross Validation
## LOGOCV on Neighborhood with group_vfold_cv()
cv_splits_geo <- group_vfold_cv(train.set,  strata = "ORIGINS_CNT", group = "CITY")
print(cv_splits_geo)

### Create Recipes ###
# Feature Creation
model_rec <- recipe(ORIGINS_CNT ~ ., data = train.set) %>%
  update_role(CITY, new_role = "CITY") %>%
  step_other(CITY, threshold = 0.005) %>% #pool infrequently occurrin values into an "other" category.
  step_dummy(all_nominal(), -CITY) %>%
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
xgb_tuned

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
RMSEbyModels_OOF <- ggplot(data = OOF_preds %>% 
         dplyr::select(model, RMSE) %>% 
         distinct() , 
       aes(x = model, y = RMSE, group = 1)) +
  geom_path(color = "red") +
  geom_label(aes(label = round(RMSE,1))) +
  labs(title='RMSE by model type') +
  theme_bw()

MAEbyModels_OOF <- ggplot(data = OOF_preds %>% 
         dplyr::select(model, MAE) %>% 
         distinct() , 
       aes(x = model, y = MAE, group = 1)) +
  geom_path(color = "red") +
  geom_label(aes(label = round(MAE,1))) +
  geom_hline(yintercept=mean(val_preds$ORIGINS_CNT), linetype="dashed") +
  labs(title='MAE by model type') +
  theme_bw()

ggsave(file.path(plot_directory,
                 "5.2 RMSEbyModels_OOF.png"),
       plot = RMSEbyModels_OOF,
       height = 6,
       width = 10,
       units = "in")

ggsave(file.path(plot_directory,
                 "5.2 MAEbyModels_OOF.png"),
       plot = MAEbyModels_OOF,
       height = 6,
       width = 10,
       units = "in")


# OOF predicted versus actual
ggplot(OOF_preds, aes(x =.pred, y = ORIGINS_CNT, group = model)) +
  geom_point(alpha = 0.3) +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "orange") +
  coord_equal() +
  facet_wrap(~model, nrow = 2) +
  xlim(0,60000)+
  ylim(0,60000)+
  labs(title="Model Performance (OOF predictions, Cross-validation based on city)")+
  theme_bw()

# distribution of observation VS pred
as.data.frame(OOF_preds) %>%
  dplyr::select(.pred,ORIGINS_CNT) %>%
  gather(Variable, Value) %>%
  ggplot(aes(Value, fill = Variable)) + 
  geom_density(alpha = 0.5) + 
  scale_fill_manual(values = c("#FFD364", "#D16BA5")) +
  labs(title="Distribution of trip counts and predicted trip counts", 
       x = "Trip counts/Prediction", y = "Density of observations") +
  xlim(0, 10000)+
  plotTheme

median(OOF_preds$ORIGINS_CNT)

# Aggregate predictions from Validation set
val_preds <- rbind(data.frame(lm_val_pred_geo, model = "lm"),
                   data.frame(glmnet_val_pred_geo, model = "glmnet"),
                   data.frame(rf_val_pred_geo, model = "rf"),
                   data.frame(xgb_val_pred_geo, model = "xgb")) %>% 
  left_join(., Model_clean %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(CITY, .row), 
            by = ".row") %>% 
  group_by(model) %>%
  mutate(# .pred = exp(.pred),
    # ORIGINS_CNT = exp(ORIGINS_CNT),
    RMSE = yardstick::rmse_vec(ORIGINS_CNT, .pred),
    MAE  = yardstick::mae_vec(ORIGINS_CNT, .pred),
    MAPE = yardstick::mape_vec(ORIGINS_CNT, .pred)) %>% 
  ungroup()

# plot MAPE by model type
RMSEbyModels_valiadation_set <- ggplot(data = val_preds %>% 
         dplyr::select(model, RMSE) %>% 
         distinct() , 
       aes(x = model, y = RMSE, group = 1)) +
  geom_path(color = "red") +
  geom_label(aes(label = round(RMSE,1))) +
  labs(title='RMSE by model type') +
  theme_bw()

MAEbyModels_valiadation_set <- ggplot(data = val_preds %>% 
                                         dplyr::select(model, MAE) %>% 
                                         distinct() , 
                                       aes(x = model, y = MAE, group = 1)) +
  geom_path(color = "red") +
  geom_label(aes(label = round(MAE,1))) +
  labs(title='MAE by model type') +
  geom_hline(yintercept=mean(val_preds$ORIGINS_CNT), linetype="dashed") +
  theme_bw()

ggsave(file.path(plot_directory,
             "5.2 RMSEbyModels_valiadation_set.png"),
       plot = RMSEbyModels_valiadation_set,
       height = 6,
       width = 10,
       units = "in")

ggsave(file.path(plot_directory,
                 "5.2 MAEbyModels_valiadation_set.png"),
       plot = MAEbyModels_valiadation_set,
       height = 6,
       width = 10,
       units = "in")

# Validation Predicted vs. actual
ggplot(val_preds, aes(x =.pred, y = ORIGINS_CNT, group = model)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "orange") +
  coord_equal() +
  xlim(0,60000)+
  ylim(0,60000)+
  labs(title="Model Performance (Validation predictions, Cross-validation based on city)")+
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


val_preds$AE <- abs(val_preds$ORIGINS_CNT - val_preds$.pred)
evaluation_by_model_city <- val_preds %>%
  group_by(model, CITY) %>%
  summarise(mean_AE = mean(AE),
            mean_CNT = mean(ORIGINS_CNT),
            RMSE = mean(RMSE))
val_preds %>%
  group_by(CITY) %>%
  summarise(MAE = mean(AE),
            mean_CNT = mean(ORIGINS_CNT),
            median_Error = median(AE),
            max_Error = max(AE)
  )



############################################
##### race content
Model_clean  <-  Model_clean %>% na.omit()
Model_clean<- Model_clean %>%
  mutate(race = ifelse(PWHITE > .5, "Majority_White", "Majority_Non_White"))

