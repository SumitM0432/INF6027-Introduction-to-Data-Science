print(paste('--------------------------------', Sys.time(), 'LINEAR REGRESSION', '----------'))

# Formula specific for the linear regression model
linear_regression_train_cols = popularity_scaled ~ .

# Training the linear regression model
linear_model = linear_reg(formula = linear_regression_train_cols, data = df_train)

# Model Definition Summary
print(linear_model)

# Predicting using the trained model (Linear Regression) and also descaling it than constraining the predictions for interpretability
linear_predictions_scaled = predict(linear_model, newdata = df_test)
linear_predictions = pmax(pmin(linear_predictions_scaled * 100, 100), 0)

# Making a tibble for evaluation and passing through the custom evaluation function
results_data = tibble(target_values = df_test$popularity,
                      predicted_values = linear_predictions)

# Evaluation of the trained model
evaluation_metrics(results_data)

print(paste('--------------------------------', Sys.time(), 'RIDGE REGRESSION', '-----------'))

# Training the ridge regression model
ridge_model = regularization_reg(X_train = X_train, y_train = y_train_scaled, alpha = 0)

# Model Definition Summary
print (ridge_model)

# Prediction using the trained model (Ridge Regression) and also descaling it than constraining the predictions for interpretability
ridge_predictions_scaled = predict(ridge_model,
                                    s = ridge_model$lambda.min, # Getting the minimum lambda (best lambda for the model)
                                    newx = as.matrix(X_test))
ridge_predictions = pmax(pmin(ridge_predictions_scaled * 100, 100), 0)

# Making a tibble for evaluation and passing through the custom evaluation function
results_data = tibble(target_values = y_test,
                      predicted_values = data.table(ridge_predictions)$s1) %>%
              # Converting the predicted values to numeric
              mutate(predicted_values = as.numeric(predicted_values))

# Evaluation of the trained model
evaluation_metrics(results_data)

print(paste('--------------------------------', Sys.time(), 'LASSO REGRESSION', '-----------'))

# Training the lasso regression model
lasso_model = regularization_reg(X_train = X_train, y_train = y_train_scaled, alpha = 1)

# Model Definition Summary
print (lasso_model)

# Prediction using the trained model (Lasso Regression) and also descaling it than constraining the predictions for interpretability
lasso_predictions_scaled <- predict(lasso_model,
                             s = lasso_model$lambda.min, # Getting the minimum lambda (best lambda for the model)
                             newx = as.matrix(X_test))
lasso_predictions = pmax(pmin(lasso_predictions_scaled * 100, 100), 0)

# Making a tibble for evaluation and passing through the custom evaluation function
results_data = tibble(target_values = y_test,
                      predicted_values = data.table(lasso_predictions)$s1) %>%
              # Converting the predicted values to numeric
              mutate(predicted_values = as.numeric(predicted_values))

# Evaluation of the trained model
evaluation_metrics(results_data)

print(paste('--------------------------------', Sys.time(), 'RANDOM FOREST REGRESSION', '---'))

# Training the random forest regression model
rf_model = random_forest_reg(X_train = X_train, y_train = y_train_scaled, n_tree = 1500)

# Model Definition Summary
print(rf_model)

# Prediction using the trained model (Random Forest Regression) and also descaling it than constraining the predictions for interpretability
rf_predictions_scaled = predict(rf_model, newdata = X_test)
rf_predictions_scaled = data.table(rf_predictions_scaled)$rf_predictions_scaled
rf_predictions = pmax(pmin(rf_predictions_scaled * 100, 100), 0)

# Making a tibble for evaluation and passing through the custom evaluation function
results_data = tibble(target_values = y_test,
                      predicted_values = rf_predictions)

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
xgb_model = xgb_reg(params = params, X_train = X_train, y_train = y_train_scaled, nrounds = 200)

# Model Definition Summary
print(xgb_model)

# Prediction using the trained model (XGB Regression) and also descaling it than constraining the predictions for interpretability
xgb_predictions_scaled = predict(xgb_model, newdata = as.matrix(X_test))
xgb_predictions = pmax(pmin(xgb_predictions_scaled * 100, 100), 0)

# Making a tibble for evaluation and passing through the custom evaluation function
results_data = tibble(target_values = y_test,
                      predicted_values = xgb_predictions)

# Evaluation of the trained model
evaluation_metrics(results_data)

print(paste('--------------------------------', Sys.time(), 'SAVING MODELS', '--------------'))

saveRDS(linear_model, "Results/lm_model_est_lyrics.rds")
saveRDS(ridge_model, "Results/ridge_model_est_lyrics.rds")
saveRDS(lasso_model, "Results/lasso_model_est_lyrics.rds")
saveRDS(rf_model, "Results/rf_model_est_lyrics.rds")
saveRDS(rf_model, "Results/xgb_model_est_lyrics.rds")






# actual = data.table(df_test$popularity)
# predicted = data.table(linear_predictions)
# res = tibble(actual = df_test$popularity, predicted = data.table(linear_predictions))
# 
# ggplot(res) +
#   geom_point(aes(x = actual, y = linear_predictions, color = )) +
#   scale_colour_identity()
# 
# ggplot(x = df_test$popularity, y = predicted$linear_predictions, main = "Actual vs. Predicted", xlab = "Actual Values", ylab = "Predicted Values")
# abline(a = 0, b = 1, col = "red")
# 
# plot(x = predicted, y = residuals, main = "Residual Plot", xlab = "Predicted Values", ylab = "Residuals")
# abline(h = 0, col = "red")