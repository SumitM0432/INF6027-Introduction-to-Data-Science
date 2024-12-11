print(paste('--------------------------------', Sys.time(), 'LINEAR REGRESSION', '----------'))

# Training the linear regression model
linear_model = linear_reg(linear_regression_train_cols, df_train)

# Model Definition Summary
summary(linear_model)

# Predicting using the trained model (Linear Regression)
linear_predictions = predict(linear_model, newdata = df_test)

# Making a tibble for evaluation and passing through the custom evaluation function
results_data = tibble(target_values = df_test$popularity,
                      predicted_values = linear_predictions)

# Evaluation of the trained model
evaluation_metrics(results_data)

print(paste('--------------------------------', Sys.time(), 'RIDGE REGRESSION', '-----------'))

# Training the ridge regression model
ridge_model = regularization_reg(X_train = as.matrix(X_train), y_train = y_train, alpha = 0)

# Model Definition Summary
print (ridge_model)

# Prediction using the trained model (Ridge Regression)
ridge_predictions <- predict(ridge_model,
                             s = ridge_model$lambda.min, # Getting the minimum lambda (best lambda for the model)
                             newx = as.matrix(X_test))

# Making a tibble for evaluation and passing through the custom evaluation function
results_data = tibble(target_values = y_test,
                      predicted_values = data.table(ridge_predictions)$s1) %>%
  mutate(predicted_values = as.numeric(predicted_values))

# Evaluation of the trained model
evaluation_metrics(results_data)

print(paste('--------------------------------', Sys.time(), 'LASSO REGRESSION', '-----------'))

# Training the lasso regression model
lasso_model = regularization_reg(X_train = as.matrix(X_train), y_train = y_train, alpha = 1)

# Model Definition Summary
print (lasso_model)

# Prediction using the trained model (Lasso Regression)
lasso_predictions <- predict(lasso_model,
                             s = lasso_model$lambda.min, # Getting the minimum lambda (best lambda for the model)
                             newx = X_test)

# Making a tibble for evaluation and passing through the custom evaluation function
results_data = tibble(target_values = y_test,
                      predicted_values = data.table(lasso_predictions)$s1) %>%
  mutate(predicted_values = as.numeric(predicted_values))

# Evaluation of the trained model
evaluation_metrics(results_data)

print(paste('--------------------------------', Sys.time(), 'RANDOM FOREST REGRESSION', '---'))

# Training the random forest regression model
rf_model = random_forest_reg(X_train, y_train, n_tree = 1000)

# Model Definition Summary
print(rf_model)

# Prediction using the trained model (Random Forest Regression)
rf_predictions = predict(rf_model, newdata = X_test)

# Making a tibble for evaluation and passing through the custom evaluation function
results_data = tibble(target_values = y_test,
                      predicted_values = data.table(rf_predictions)$rf_predictions) %>%
  mutate(predicted_values = as.numeric(predicted_values))

# Evaluation of the trained model
evaluation_metrics(results_data)

print(paste('--------------------------------', Sys.time(), 'XGB REGRESSION', '-------------'))

# XGBOOST Parameters
params <- list(
  objective = "reg:squarederror", 
  eta = 0.1,                       
  max_depth = 6,                   
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Training the XGB regression model
xgb_model = xgb_reg(params, X_train, y_train, nbounds = 150)

# Prediction using the trained model (XGB Regression)
xgb_predictions <- predict(xgb_model, newdata = as.matrix(X_test))

# Making a tibble for evaluation and passing through the custom evaluation function
results_data = tibble(target_values = y_test,
                      predicted_values = xgb_predictions)

# Evaluation of the trained model
evaluation_metrics(results_data)

print(paste('--------------------------------', Sys.time(), 'SAVING MODELS', '--------------'))

saveRDS(linear_model, "Results/lm_model_est.rds")
saveRDS(ridge_model, "Results/ridge_model_est.rds")
saveRDS(lasso_model, "Results/lasso_model_est.rds")
saveRDS(rf_model, "Results/rf_model_est.rds")
saveRDS(rf_model, "Results/xgb_model_est.rds")






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