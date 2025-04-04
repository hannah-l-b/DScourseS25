library(tidyverse)
library(tidymodels)
library(magrittr)
library(glmnet)

# Load Housing Data
housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", header = FALSE)
names(housing) <- c("crim", "zn", "indus", "chas", "nox", "rm", "age", "dis", 
                    "rad", "tax", "ptratio", "b", "lstat", "medv")

set.seed(123456)

# Create two datasets for training and testing
housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

# Create a recipe that will be used to preprocess the data
housing_recipe <- recipe ( medv ~ . , data = housing ) %>%
  # convert outcome variable to logs
  step_log ( all_outcomes() ) %>%
  # convert 0/1 chas to a factor
  step_bin2factor ( chas ) %>%
  # create interaction term between crime and nox
  step_interact ( terms = ~ crim : zn : indus :rm: age : rad : tax :
                      ptratio : b : lstat : dis : nox ) %>%
  # create square terms of some continuous variables
  step_poly (crim, zn, indus, rm, age, rad, tax, ptratio, b,
                lstat, dis, nox, degree=6)
# Run the recipe
housing_prep <- housing_recipe %>% prep ( housing_train, retain= TRUE )
housing_train_prepped <- housing_prep %>% juice
housing_test_prepped <- housing_prep %>% bake (new_data = housing_test)
# create x and y training and test data
housing_train_x <- housing_train_prepped %>% select ( - medv )
housing_test_x <- housing_test_prepped %>% select ( - medv )
housing_train_y <- housing_train_prepped %>% select ( medv )
housing_test_y <- housing_test_prepped %>% select ( medv )

# Dimension of training data
dim(housing_train_x) # 74 X variables

# Estiamte LASSO model
## Model Specifications
lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

## Cross-validation folds
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

## Grid for lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

## Tune lambda using 6-fold CV
lasso_tune <- tune_grid(
  lasso_spec,
  medv ~ .,
  resamples = rec_folds,
  grid = lambda_grid
)

## Select best lambda
best_lasso <- select_best(lasso_tune, metric = "rmse")

## Final LASSO model with best lambda
lasso_final_spec <- finalize_model(lasso_spec, best_lasso)

## Fit LASSO model
lasso_fit <- lasso_final_spec %>%
  fit(medv ~ ., data = housing_train_prepped)

## In-sample RMSE
lasso_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rmse(truth, .pred) %>%
  print()

## Out-of-sample RMSE
lasso_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rmse(truth, .pred) %>%
  print()

## Results
### best lambda was 0.00222
### in-sample RMSE was 0.138
### out-of-sample RMSE is 0.184


# Estimating Ridge model
## Model Specifications
ridge_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

## 6-fold cross-validation
rec_folds_ridge <- vfold_cv(housing_train_prepped, v = 6)

## Grid of lambda values
lambda_grid_ridge <- grid_regular(penalty(), levels = 50)

## Tune over grid
ridge_tune <- tune_grid(
  ridge_spec,
  medv ~ .,
  resamples = rec_folds_ridge,
  grid = lambda_grid_ridge
)

## Select best lambda
best_ridge <- select_best(ridge_tune, metric = "rmse")

## Finalize model with best lambda
ridge_final_spec <- finalize_model(ridge_spec, best_ridge)

## Fit model to training data
ridge_fit <- ridge_final_spec %>%
  fit(medv ~ ., data = housing_train_prepped)

## In-sample RMSE
ridge_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rmse(truth, .pred) %>%
  print()

## Out-of-sample RMSE
ridge_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rmse(truth, .pred) %>%
  print()

## Results
### best lambda was 0.0000000001
### in-sample RMSE was 0.140
### out-of-sample RMSE is 0.181


