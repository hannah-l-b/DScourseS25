library(mice)
library(modelsummary)
library(tidyverse)
library(dplyr)

# Load data
df <- read.csv("C:\\Users\\berm0006\\Documents\\ECON 5253\\DScourseS25\\ProblemSets\\PS7\\wages.csv")

# Drop observations where either hgc or tenure are missing.
df <- df %>% drop_na(hgc, tenure)

# Use modelsummary to summarize the data
datasummary_skim(df, output = "latex", histogram = FALSE)

# Calculate the missing rate for logwage
missing_rate <- mean(is.na(df$logwage))
missing_rate_percentage <- missing_rate * 100

# Print the missing rate as a percentage
cat("The logwage variable is missing", round(missing_rate_percentage, 2), "% of the observations.\n")

# Regressions
# Model 1: Listwise
# Remove rows with missing logwage (Listwise Deletion)
df_l <- df %>% drop_na(logwage)

# Run the linear regression model
df_l_lm <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = df_l)

# Display regression results
summary(df_l_lm)

# Model 2: Mean Imputation
df_mi <- df

# Replace missing values in logwage with the mean of logwage
df_mi$logwage[is.na(df_mi$logwage)] <- mean(df$logwage, na.rm = TRUE)

# Run the linear regression model
df_mi_lm <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = df_mi)

# Display regression results
summary(df_mi_lm)

# Model 3: Impute missing log wages as their predicted values
# Impute missing logwage values using the complete cases regression model
df_imp <- df

# Predict missing logwage values using the complete cases regression model
df_imp$logwage[is.na(df_imp$logwage)] <- predict(df_l_lm, newdata = df_imp[is.na(df_imp$logwage), c("hgc", "college", "tenure", "age", "married")])

# Run the linear regression model
df_imp_lm <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = df_imp)

# Display regression results
summary(df_imp_lm)

# Model 4: Multiple Imputation using mice package
# Perform Multiple Imputation with 5 datasets
df_mice <- mice(df, m = 5, printFlag = FALSE)

# Run the linear regression model on each imputed dataset
df_mice_lm <- with(df_mice, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))

# Combine the regression results from the imputed datasets
df_mice_summary <- pool(df_mice_lm)

# Display the combined regression results
summary(df_mice_summary)

# Use the fixed model in the summary
model_comparison <- modelsummary(
  list("Listwise Deletion" = df_l_lm, 
       "Mean Imputation" = df_mi_lm, 
       "Regression Imputation" = df_imp_lm, 
       "Multiple Imputation" = df_mice_lm), 
  stars = TRUE, 
  gof_omit = "all", 
  output = "latex"
)

# Print the model comparison table
model_comparison

