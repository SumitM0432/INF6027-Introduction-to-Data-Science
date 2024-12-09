# LINEAR REGRESSION MODEL

# Training a linear regression model (Baseline)
linear_model <- lm(popularity ~ ., data = df_train)

# View model summary
summary(linear_model)

# Predicting using the trained model (Linear Regression)
linear_predictions <- predict(linear_model, newdata = df_test)

# Making a tibble for evaluation and passing through the custom evaluation function
results_data = tibble(target_values = df_test$popularity,
                      predicted_values = linear_predictions)

# Evaluation of the trained model
evaluation_metrics(results_data)

# RIDGE REGRESSION MODEL

# Changing the data table to matrix for ridge regression and separating the predictors and target values
X_train = as.matrix(df_train %>% select(-c(popularity)))
y_train = df_train$popularity

X_test = as.matrix(df_test %>% select(-c(popularity)))
y_test = df_test$popularity

# Fit Ridge Regression model
ridge_model <- cv.glmnet(test_matrix, df_test$popularity, alpha = 0)  # alpha = 0 for Ridge Regression

# Get the best lambda
best_lambda_ridge <- ridge_model$lambda.min

# Prediction using the trained model (Ridge Regression)
ridge_predictions <- predict(ridge_model, s = best_lambda_ridge, newx = X_test)

# Making a tibble for evaluation and passing through the custom evaluation function
results_data = tibble(target_values = y_test,
                      predicted_values = ridge_predictions)

# Evaluation of the trained model
evaluation_metrics(results_data)

# Saving the model
saveRDS(linear_model, "Results/lm_model_est.rds")