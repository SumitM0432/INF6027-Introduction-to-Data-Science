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

linear_imp = ggplot(feature_importance_linear, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "lightseagreen") + # Using stat as identity to skip the aggregation since we already have values
  coord_flip() +
  labs(
    title = "Feature Importance : Linear Regression ",
    x = "Features",
    y = "Coefficient Value"
  ) +
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'))

# Plotting
plot(linear_imp)

# Saving the plot
ggsave(paste0("Feature_Importance_linear_reg.jpeg"), linear_imp, path = paste0(getwd(), "/Plots")) 

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

# PLOTTING FEATURE IMPORTANCE
# Extract feature importance
rf_importance = randomForest::importance(rf_model)

# Convert to a data frame
feature_importance_rf = data.frame(
  Feature = rownames(rf_importance),
  Importance = rf_importance[, "IncNodePurity"]
)

# Plot feature importance
rf_imp = ggplot(feature_importance_rf, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "lightseagreen") +
  coord_flip() +
  labs(
    title = "Feature Importance : Random Forest",
    x = "Features",
    y = "Importance (IncNodePurity)"
  ) +
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'))

# Plotting
plot(rf_imp)

# Saving the plot
ggsave(paste0("Feature_Importance_rf_reg.jpeg"), rf_imp, path = paste0(getwd(), "/Plots")) 

print(paste('--------------------------------', Sys.time(), 'XGB REGRESSION', '-------------'))

# Parameter grid to test and tune
parameter_grid = expand.grid(
  nrounds = c(100, 150, 200),       # Fixed rounds for simplicity
  max_depth = c(4, 5, 6),       # Optimal value based on prior knowledge or exploration
  eta = c(0.05, 0.1, 0.15),           # Learning rate
  gamma = c(0, 1),         # Regularization term
  colsample_bytree = c(0.7, 0.8), # Subsampling for features
  min_child_weight = c(3, 5),   # Minimum child weight
  subsample = c(0.7, 0.8)      # Subsampling for rows
)

# Doing 5 Folds cross-validation control
train_control = trainControl(
  method = "cv",          # Cross-validation
  number = 5,             # 5 folds
  verboseIter = TRUE,     # Show progress
  allowParallel = TRUE    # Enable parallel processing
)

# Training the XGB regression model and tuning it
# This will test all the hyperparameter given and extract the best model that is trained on this cross validation set
xgb_result = xgb_tuning_simple(
  X_train = X_train,
  y_train_scaled = y_train_scaled,
  parameter_grid = parameter_grid,
  train_control = train_control
)
# Extract the best model
xgb_model = xgb_result$focused$bestTune

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
feature_importance_xgb = as.data.frame(xgb_importance)

# Plot with ggplot2
xgb_imp = ggplot(feature_importance_xgb, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "lightseagreen") +
  coord_flip() +
  labs(
    title = "Feature Importance : XGBOOST",
    x = "Features",
    y = "Importance (Gain)"
  ) +
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'))

# Plotting
plot(xgb_imp)

# Saving the plot
ggsave(paste0("Feature_Importance_xgb_reg.jpeg"), xgb_imp, path = paste0(getwd(), "/Plots")) 

print(paste('--------------------------------', Sys.time(), 'SAVING MODELS', '--------------'))

saveRDS(linear_model, "Trained_Models/lm_model_est_lyrics.rds")
saveRDS(rf_model, "Trained_Models/rf_model_est_lyrics.rds")
saveRDS(xgb_model, "Trained_Models/xgb_model_est_lyrics.rds")