# LINEAR REGRESSION MODEL ------------------------------------------------------

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

# RIDGE REGRESSION MODEL ------------------------------------------------------

# Changing the data table to matrix for ridge regression and separating the predictors and target values
X_train = as.matrix(df_train %>% select(-c(popularity)))
y_train = df_train$popularity

X_test = as.matrix(df_test %>% select(-c(popularity)))
y_test = df_test$popularity

# Fit Ridge Regression model
ridge_model <- cv.glmnet(X_train,
                         y_train,
                         alpha = 0)  # alpha = 0 for Ridge Regression

# Model Summary
print (ridge_model)

# Getting the minimum lambda (best lambda for the model)
best_lambda_ridge <- ridge_model$lambda.min
print(paste("Best lambda:", best_lambda_ridge))

# Prediction using the trained model (Ridge Regression)
ridge_predictions <- predict(ridge_model,
                             s = best_lambda_ridge,
                             newx = X_test)

# Making a tibble for evaluation and passing through the custom evaluation function
results_data = tibble(target_values = y_test,
                      predicted_values = data.table(ridge_predictions)$s1) %>%
  mutate(predicted_values = as.numeric(predicted_values))

# Evaluation of the trained model
evaluation_metrics(results_data)

# LASSO REGRESSION MODEL ------------------------------------------------------

# Fit Ridge Regression model
lasso_model <- cv.glmnet(X_train,
                         y_train,
                         alpha = 1)  # alpha = 1 for Lasso Regression

# Model Summary
print (lasso_model)

# Getting the minimum lambda (best lambda for the model)
best_lambda_lasso <- lasso_model$lambda.min
print(paste("Best lambda:", best_lambda_lasso))

# Prediction using the trained model (Lasso Regression)
lasso_predictions <- predict(lasso_model,
                             s = best_lambda_lasso,
                             newx = X_test)

# Making a tibble for evaluation and passing through the custom evaluation function
results_data = tibble(target_values = y_test,
                      predicted_values = data.table(lasso_predictions)$s1) %>%
  mutate(predicted_values = as.numeric(predicted_values))

# Evaluation of the trained model
evaluation_metrics(results_data)

# RANDOM FOREST REGRESSION MODEL ------------------------------------------------------

# Getting the train data and test data separated
X_train = df_train %>% select(-c(popularity))
y_train = df_train$popularity

X_test = df_test %>% select(-c(popularity))
y_test = df_test$popularity

# Train Random Forest model
rf_model <- randomForest(
  x = X_train,
  y = y_train,
  ntree = 1000,
  mtry = sqrt(ncol(X_train)) 
)

# View model summary
print(rf_model)

# Predict on test data
rf_predictions <- predict(rf_model, newdata = X_test)

# Making a tibble for evaluation and passing through the custom evaluation function
results_data = tibble(target_values = y_test,
                      predicted_values = data.table(rf_predictions)$rf_predictions) %>%
  mutate(predicted_values = as.numeric(predicted_values))

# Evaluation of the trained model
evaluation_metrics(results_data)

# SAVING THE MODELS ------------------------------------------------------

# Saving the Linear Regression Model
saveRDS(linear_model, "Results/lm_model_est.rds")

# Saving the Ridge Regression Model
saveRDS(ridge_model, "Results/ridge_model_est.rds")

# Saving the Lasso Regression Model
saveRDS(lasso_model, "Results/lasso_model_est.rds")

# Saving the Random Forest Model
saveRDS(rf_model, "Results/rf_model_est.rds")

actual = data.table(df_test$popularity)
predicted = data.table(linear_predictions)
res = tibble(actual = df_test$popularity, predicted = data.table(linear_predictions))

ggplot(res) +
  geom_point(aes(x = actual, y = linear_predictions, color = )) +
  scale_colour_identity()

ggplot(x = df_test$popularity, y = predicted$linear_predictions, main = "Actual vs. Predicted", xlab = "Actual Values", ylab = "Predicted Values")
abline(a = 0, b = 1, col = "red")

plot(x = predicted, y = residuals, main = "Residual Plot", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red")
