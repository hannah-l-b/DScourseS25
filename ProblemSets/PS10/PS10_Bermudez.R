library(tidyverse)
library(tidymodels)
library(magrittr)
library(modelsummary)
library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(kernlab)
library(yardstick)

set.seed(100)

income <- read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", col_names = FALSE)
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# Clean Data
## Drop unnecessary columns
income %<>% select(-native.country, -fnlwgt, education.num)
## Make sure continuous variables are formatted as numeric
income %<>% mutate(across(c(age,hours,education.num,capital.gain,capital.loss), as.numeric))
## Make sure discrete variables are formatted as factors
income %<>% mutate(across(c(high.earner,education,marital.status,race,workclass,occupation,relationship,sex), as.factor))
## Combine levels of factor variables that currently have too many levels
income %<>% mutate(education = fct_collapse(education,
                                            Advanced    = c("Masters","Doctorate","Prof-school"), 
                                            Bachelors   = c("Bachelors"), 
                                            SomeCollege = c("Some-college","Assoc-acdm","Assoc-voc"),
                                            HSgrad      = c("HS-grad","12th"),
                                            HSdrop      = c("11th","9th","7th-8th","1st-4th","10th","5th-6th","Preschool") 
),
marital.status = fct_collapse(marital.status,
                              Married      = c("Married-civ-spouse","Married-spouse-absent","Married-AF-spouse"), 
                              Divorced     = c("Divorced","Separated"), 
                              Widowed      = c("Widowed"), 
                              NeverMarried = c("Never-married")
), 
race = fct_collapse(race,
                    White = c("White"), 
                    Black = c("Black"), 
                    Asian = c("Asian-Pac-Islander"), 
                    Other = c("Other","Amer-Indian-Eskimo")
), 
workclass = fct_collapse(workclass,
                         Private = c("Private"), 
                         SelfEmp = c("Self-emp-not-inc","Self-emp-inc"), 
                         Gov     = c("Federal-gov","Local-gov","State-gov"), 
                         Other   = c("Without-pay","Never-worked","?")
), 
occupation = fct_collapse(occupation,
                          BlueCollar  = c("?","Craft-repair","Farming-fishing","Handlers-cleaners","Machine-op-inspct","Transport-moving"), 
                          WhiteCollar = c("Adm-clerical","Exec-managerial","Prof-specialty","Sales","Tech-support"), 
                          Services    = c("Armed-Forces","Other-service","Priv-house-serv","Protective-serv")
)
)

# Train, CV, & Compute Test Set
## Split data into training and test sets
income_split <- initial_split(income, prop = 0.8)
income_train <- training(income_split)
income_test  <- testing(income_split)

# Logistic Regression
tune_logit_spec <- logistic_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("classification")

## Define a grid over which to try different values of the regularization parameter lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

## 3-fold CV
logit_folds <- vfold_cv(income_train, v = 3)

## Define Workflow
logit_wf <- workflow() %>%
  add_model(tune_logit_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
logit_res <- logit_wf %>%
  tune_grid(
    resamples = logit_folds,
    grid = lambda_grid
  )

## Extract best value
logit_top_acc  <- show_best(logit_res, metric = "accuracy")
logit_best_acc <- select_best(logit_res, metric = "accuracy")
final_logit_lasso <- finalize_workflow(logit_wf,
                                       logit_best_acc
)

## Fit the model
logit_test <- last_fit(final_logit_lasso,income_split) %>%
  collect_metrics()

logit_test %>% print(n = 1)
logit_top_acc %>% print(n = 1)

## Combine results
logit_ans <- logit_top_acc %>% slice(1)
logit_ans %<>% left_join(logit_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "logit") %>% select(-starts_with(".config"))

# Decision Tree
tune_tree_spec <- decision_tree(
  min_n = tune(), # tuning parameter
  tree_depth = tune(), # tuning parameter
  cost_complexity = tune(), # tuning parameter
) %>% 
  set_engine("rpart") %>%
  set_mode("classification")

## Define a set over which to try different values of the regularization parameter (complexity, depth, etc.)
tree_parm_df1 <- tibble(cost_complexity = seq(.001,.2,by=.05))
tree_parm_df2 <- tibble(min_n = seq(10,100,by=10))
tree_parm_df3 <- tibble(tree_depth = seq(5,20,by=5))

## Create a grid of all combinations of the parameters
tree_parm_df  <- full_join(tree_parm_df1,tree_parm_df2,by=character()) %>% full_join(.,tree_parm_df3,by=character())

## 3-fold CV
tree_folds <- vfold_cv(income_train, v = 3)

## Define Workflow
tree_wf <- workflow() %>%
  add_model(tune_tree_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
tree_res <- tree_wf %>%
  tune_grid(
    resamples = tree_folds,
    grid = tree_parm_df
  )

## Extract best value
tree_top_acc  <- show_best(tree_res, metric = "accuracy")
tree_best_acc <- select_best(tree_res, metric = "accuracy")
final_tree <- finalize_workflow(tree_wf,
                                  tree_best_acc
)

## Fit the model
tree_test <- last_fit(final_tree,income_split) %>%
  collect_metrics()

tree_test %>% print(n = 1)
tree_top_acc %>% print(n = 1)

## Combine results
tree_ans <- tree_top_acc %>% slice(1)
tree_ans %<>% left_join(tree_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "tree") %>% select(-starts_with(".config"))

# Neural Network
tune_nnet_spec <- mlp(
  hidden_units = tune(), # tuning parameter
  penalty = tune()
) %>% 
  set_engine("nnet") %>%
  set_mode("classification")

## Define a grid over which to try different values of the regularization parameter (complexity, depth, etc.)
nnet_parm_df1 <- tibble(hidden_units = seq(1,10))
lambda_grid   <- grid_regular(penalty(), levels = 10)

## Create a grid of all combinations of the parameters
nnet_parm_df  <- full_join(nnet_parm_df1,lambda_grid,by=character())

## 3-fold CV
nnet_folds <- vfold_cv(income_train, v = 3)

## Define Workflow
nnet_wf <- workflow() %>%
  add_model(tune_nnet_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
nnet_res <- nnet_wf %>%
  tune_grid(
    resamples = nnet_folds,
    grid = nnet_parm_df
  )

## Extract best value
nnet_top_acc  <- show_best(nnet_res, metric = "accuracy")
nnet_best_acc <- select_best(nnet_res, metric = "accuracy")
final_nnet <- finalize_workflow(nnet_wf,
                                  nnet_best_acc
)

## Fit the model
nnet_test <- last_fit(final_nnet,income_split) %>%
  collect_metrics()

nnet_test %>% print(n = 1)
nnet_top_acc %>% print(n = 1)

## Combine results
nnet_ans <- nnet_top_acc %>% slice(1)
nnet_ans %<>% left_join(nnet_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "nnet") %>% select(-starts_with(".config"))

# kNN
tune_knn_spec <- nearest_neighbor(
  neighbors = tune() # tuning parameter
) %>% 
  set_engine("kknn") %>%
  set_mode("classification")

## Define a set over which to try different values of the regularization parameter (number of neighbors)
knn_parm_df <- tibble(neighbors = seq(1,30))

## 3-fold CV
knn_folds <- vfold_cv(income_train, v = 3)

## Define Workflow
knn_wf <- workflow() %>%
  add_model(tune_knn_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
knn_res <- knn_wf %>%
  tune_grid(
    resamples = knn_folds,
    grid = knn_parm_df
  )

## Extract best value
knn_top_acc  <- show_best(knn_res, metric = "accuracy")
knn_best_acc <- select_best(knn_res, metric = "accuracy")
final_knn <- finalize_workflow(knn_wf,
                                  knn_best_acc
)

## Fit the model
knn_test <- last_fit(final_knn,income_split) %>%
  collect_metrics()

knn_test %>% print(n = 1)
knn_top_acc %>% print(n = 1)

## Combine results
knn_ans <- knn_top_acc %>% slice(1)
knn_ans %<>% left_join(knn_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "knn") %>% select(-starts_with(".config"))

# SVM
tune_svm_spec <- svm_rbf(
  cost = tune(), 
  rbf_sigma = tune()
) %>% 
  set_engine("kernlab") %>%
  set_mode("classification")

## Define a set over which to try different values of the regularization parameter (number of neighbors)
svm_parm_df1 <- tibble(cost = c(2^(-1), 2^0, 2^1))
svm_parm_df2 <- tibble(rbf_sigma = c(2^(-1), 2^0, 2^1))

## Create a grid of all combinations of the parameters
svm_parm_df  <- full_join(svm_parm_df1,svm_parm_df2,by=character())

## 3-fold CV
svm_folds <- vfold_cv(income_train, v = 3)

## Define Workflow
svm_wf <- workflow() %>%
  add_model(tune_svm_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
svm_res <- svm_wf %>%
  tune_grid(
    resamples = svm_folds,
    grid = svm_parm_df, 
    metrics = metric_set(accuracy),
    control = control_grid(verbose=T)
  )

## Extract best value
svm_top_acc  <- show_best(svm_res, metric = "accuracy")
svm_best_acc <- select_best(svm_res, metric = "accuracy")
final_svm <- finalize_workflow(svm_wf,
                                  svm_best_acc
)

## Fit the model
svm_test <- last_fit(final_svm,income_split) %>%
  collect_metrics()

svm_test %>% print(n = 1)
svm_top_acc %>% print(n = 1)

## Combine results
svm_ans <- svm_top_acc %>% slice(1)
svm_ans %<>% left_join(svm_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "svm") %>% select(-starts_with(".config"))

# Combine all results into a single tibble
all_ans <- bind_rows(logit_ans,tree_ans,nnet_ans,knn_ans,svm_ans)
datasummary_df(all_ans %>% select(-.metric,-.estimator,-mean,-n,-std_err),output="markdown") %>% print

# Output the results as Latex table
datasummary_df(all_ans %>% select(-.metric,-.estimator,-mean,-n,-std_err),output="latex") %>% print


