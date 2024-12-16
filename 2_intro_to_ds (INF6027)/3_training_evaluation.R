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

# Plotting and Saving Prediction and Actual Plot
pred_vs_actual_plot(results_data = results_data, model_name = 'linear_reg')
residual_plot(results_data = results_data, model_name = 'linear_reg')

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

# Plotting and Saving Prediction and Actual Plot
pred_vs_actual_plot(results_data = results_data, model_name = 'rf_reg')
residual_plot(results_data = results_data, model_name = 'rf_reg')

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

# Plotting and Saving Prediction and Actual Plot
pred_vs_actual_plot(results_data = results_data, model_name = 'xgb_reg')
residual_plot(results_data = results_data, model_name = 'xgb_reg')

print(paste('--------------------------------', Sys.time(), 'SAVING MODELS', '--------------'))

saveRDS(linear_model, "Results/lm_model_est_lyrics.rds")
saveRDS(rf_model, "Results/rf_model_est_lyrics.rds")
saveRDS(rf_model, "Results/xgb_model_est_lyrics.rds")