# Loading libraries -------------------------------------------------------
library(tidyverse)
library(naniar)
library(vtreat)
library(ranger)
library(xgboost)
library(caret)
library(tictoc)
library(h2o)

# Loading data ------------------------------------------------------------
df <- read_rds("data/modified/compact_data.rds") %>% 
  select(-subs_value) # temporary 

# Cleaning data for feature table #   
df <- select(df, -SUBID:-Concentration, -valid_measurement, -sub_groups, -censored)  # -subs_value, -censored

# first test removing all missing values (for now) #
df <- na.omit(df)

# create test and training data #
set.seed(1234)
N <- nrow(df)
target <- round(0.75 * N)
gp <- runif(N)

# splitting the data
df_train <- df[gp < 0.75, ] 
df_test <- df[gp >= 0.75, ]

# Cross validation Plan #
nRows <- nrow(df)
splitPlan <- kWayCrossValidation(nRows, 3, NULL, NULL)


fmla <- as.formula("subs_value_dl ~ .")

# test model  random forrest #
(subs_model_rf <-
  ranger(
    formula = fmla,
    data = df_train,
    num.trees = 500,
    respect.unordered.factors = "order"
  ))

df_test$rf_pred <- predict(subs_model_rf, df_test)$predictions

# RMSE on test data
mutate(df_test, residual = subs_value_dl - rf_pred) %>% 
  summarise(rmse = sqrt(mean(residual ^2)))

# plot model performance
ggplot(df_test, aes(x = rf_pred, y = subs_value_dl)) + 
  geom_point() + 
  geom_abline() +
  labs(x ="prediction_rf", y ="concentration (μg/l)")

rf_pred <- df_test$rf_pred
df_test <- select(df_test, -rf_pred)



# test model xgboost # 
df_train_xg <- select(df_train, -subs_value_dl)
df_test_xg <- select(df_test, -subs_value_dl)

cv <-
  xgb.cv(
    data = as.matrix(df_train_xg),
    label = df_train$subs_value_dl,
    nrounds = 100,
    nfold = 5,
    objective = "reg:linear",
    eta = 0.3,
    max_depth = 6,
    early_stopping_rounds = 10,
    verbose = 0
  )

elog <- cv$evaluation_log

elog %>% 
  summarize(ntrees.train = which.min(train_rmse_mean),   # find the index of min (train_rmse_mean)
            ntrees.test  = which.min(test_rmse_mean)) 

subs_model_xgb <- xgboost(data = as.matrix(df_train_xg),
                          label = df_train$subs_value_dl,
                          nrounds = 15,
                          objective = "reg:linear",
                          eta = 0.3,
                          depth = 6,
                          verbose = 0)

df_test$xgb_pred <- predict(subs_model_xgb, as.matrix(df_test_xg))


mutate(df_test, residual = subs_value_dl - xgb_pred) %>% 
  summarise(rmse = sqrt(mean(residual ^2)))

ggplot(df_test, aes(x = xgb_pred, y = subs_value_dl)) + 
  geom_point() + 
  geom_abline() +
  labs(x ="prediction_xgb", y ="concentration (μg/l)")


df_test$rf_pred <- rf_pred 

# check both models #
mutate(df_test, residual_rf = subs_value_dl - rf_pred,
       residual_xgb = subs_value_dl - xgb_pred) %>% 
  summarise(rmse_rf = sqrt(mean(residual_rf ^2)),
            rmse_xgb = sqrt(mean(residual_xgb ^2)))


# Models with caret -------------------------------------------------------

my_folds <- createFolds(df_train$subs_value_dl, k = 5)

# glmnet #
fit_control <- trainControl(
  method = "adaptive_cv",
  adaptive = list(
    min = 5,
    alpha = 0.05,
    method = "BT",
    complete = TRUE
  ),
  repeats = 3,
  search = "random",
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = my_folds,
  allowParallel = TRUE
 )

glmnet_grid <- expand.grid(
  alpha = seq(0, 1, 0.1),
  lambda = seq(0.0001, 0.1, length = 10)
)


tic()
glmnet_model <- train(
  subs_value_dl ~ ., 
  data = df_train,
  method = "glmnet",
  preProcess = "pca",
  tuneLength = 100,
  trControl = fit_control
)
toc()

# rndom forrest #
tic()
rf_model <- train(
  subs_value_dl ~ ., 
  data = df_train,
  method = "ranger",
  tuneLength = 100,
  trControl = fit_control
)
toc()

# gradient boosting machine #
tic()
gbm_model <- train(
  subs_value_dl ~ ., 
  data = df_train,
  method = "gbm",
  tuneLength = 300,
  trControl = fit_control
)
toc()


# XGBoost #

xgb_control <- trainControl(
  method = "adaptive_cv",
  adaptive = list(
    min = 5,
    alpha = 0.05,
    method = "BT",
    complete = TRUE
  ),
  number = 10,
  repeats = 3,
  search = "random",
  verboseIter = TRUE,
  allowParallel = TRUE
)



tic()
xgb_model <- train(
  subs_value_dl ~ ., 
  data = as.matrix(df_train),
  method = "xgbTree",
  tuneLength = 300,
  trControl = fit_control
)
toc()

# comparing #
model_list <- list(glmnet = glmnet_model, rf = rf_model, gbm = gbm_model, xgb = xgb_model)

resamps <- resamples(model_list)

summary(resamps)


dotplot(resamps, metric = "RMSE")
bwplot(resamps, metric = "RMSE")


# AutoML With H20 ---------------------------------------------------------

h2o.init()

df_h2o <- as.h2o(df)

y <- "subs_value"
x <- setdiff(colnames(df_h2o), y)

sframe <- h2o.splitFrame(data = df_h2o, ratios = c(0.7, 0.15), seed = 1234)

train <- sframe[[1]]
valid <- sframe[[2]]
test <- sframe[[3]]


automl_model <- h2o.automl(x = x, 
                           y = y,
                           training_frame = train,
                           validation_frame = valid,
                           max_models = 50,
                           sort_metric = "RMSE",
                           nfolds = 5,
                           seed = 1234)




