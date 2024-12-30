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

# PLOTTING FEATURE IMPORTANCE
coefficients = coef(linear_model)

feature_importance_linear = data.frame(
  Feature = names(coefficients)[-1],  # Exclude intercept
  Importance = coefficients[-1]
)

# Plotting and Saving Feature Importance graph
feature_importance_plot(feature_importance_df = feature_importance_linear,
                        model_name_title = 'Linear Regression',
                        importance_type = 'Coefficient Value')

print(paste('--------------------------------', Sys.time(), 'RANDOM FOREST REGRESSION', '---'))

# Parameter grid to test and tune for random forest
rf_parameter_grid = expand.grid(
  mtry = c(2, 4, 6),        # Number of predictors to consider at each split
  splitrule = "variance",   # Default for regression
  min.node.size = c(1, 5, 10)  # Minimum size of terminal nodes
)

# Doing 5 Folds cross-validation control
rf_train_control = trainControl(
  method = "cv",            # Cross-validation
  number = 5,               # Number of folds
  verboseIter = TRUE,       # Print progress
  allowParallel = TRUE      # Allow parallel processing
)

# Training the random forest regression model
rf_model = rf_reg_tuning(
  X_train = X_train,
  y_train_scaled = y_train_scaled,
  parameter_grid = rf_parameter_grid,
  train_control = rf_train_control
)

# Extracting the best tuned hyperparameters
rf_model_tuned_parameters = rf_model$bestTune
print ("Best Parameters for Random Forest Regressor ::")
rf_model_tuned_parameters

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

# PLOTTING FEATURE IMPORTANCE
# Extract feature importance from the ranger model within the caret object
rf_importance = varImp(rf_model, scale = FALSE)

# Convert to a data frame and some minor adjustments for consistency
feature_importance_rf = as.data.frame(rf_importance$importance) %>%
  mutate(features = rownames(rf_importance$importance)) %>%
  rename('Importance' = 'Overall') %>%
  rename('Feature' = 'features')

# Plotting and Saving Feature Importance graph
feature_importance_plot(feature_importance_df = feature_importance_rf,
                        model_name_title = 'Random Forest',
                        importance_type = 'IncNodePurity')

print(paste('--------------------------------', Sys.time(), 'XGB REGRESSION', '-------------'))

# Parameter grid to test and tune for xgboost
xgb_parameter_grid = expand.grid(
  nrounds = c(100, 150, 200),       # Fixed rounds for simplicity
  max_depth = c(4, 5, 6),       # Optimal value based on prior knowledge or exploration
  eta = c(0.05, 0.1, 0.15),           # Learning rate
  min_child_weight = c(3, 5),   # Minimum child weight
)

# Doing 5 Folds cross-validation control
xgb_train_control = trainControl(
  method = "cv",          # Cross-validation
  number = 5,             # 5 folds
  verboseIter = TRUE,     # Show progress
  allowParallel = TRUE    # Enable parallel processing
)

# Training the XGB regression model and tuning it
# This will test all the hyperparameter given and extract the best model that is trained on this cross validation set
xgb_result = xgb_reg_tuning(
  X_train = X_train,
  y_train_scaled = y_train_scaled,
  parameter_grid = xgb_parameter_grid,
  train_control = xgb_train_control
)

# Extracting the best tuned hyperparameters
xgb_model_tuned_parameters = xgb_result$bestTune
print ("Best Parameters for XGBoost Regressor ::")
xgb_model_tuned_parameters

# Extract the best model
xgb_model = xgb_result$finalModel

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

# PLOT FEATURE IMPORTANCE
# Extract feature importance
xgb_importance = xgb.importance(model = xgb_model)

# Convert importance matrix to data frame
feature_importance_xgb = as.data.frame(xgb_importance) %>%
  rename('Importance' = 'Gain') %>%
  select(-c(Cover, Frequency))

# Plotting and Saving Feature Importance graph
feature_importance_plot(feature_importance_df = feature_importance_xgb,
                        model_name_title = 'XGBOOST',
                        importance_type = 'Gain')

print(paste('--------------------------------', Sys.time(), 'SAVING MODELS', '--------------'))

if (lyrical_switch == TRUE) {
  saveRDS(linear_model, "Trained_Models/With Lyrics/lm_model.rds")
  saveRDS(rf_model, "Trained_Models/With Lyrics/rf_model.rds")
  saveRDS(xgb_model, "Trained_Models/With Lyrics/xgb_model.rds")
} else {
  saveRDS(linear_model, "Trained_Models/Without Lyrics/lm_model.rds")
  saveRDS(rf_model, "Trained_Models/Without Lyrics/rf_model.rds")
  saveRDS(xgb_model, "Trained_Models/Without Lyrics/xgb_model.rds")
}