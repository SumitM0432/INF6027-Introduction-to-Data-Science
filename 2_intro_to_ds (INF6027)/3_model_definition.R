# LINEAR REGRESSION MODEL ------------------------------------------------------

# Linear Regression Model Definition (Baseline)
linear_reg = function (formula, data) {
  linear_model = lm(formula, data = df_train)
  return (linear_model)
}

# RANDOM FOREST MODEL ----------------------------------------------------------

# Random Forest Regressor Model Definition
random_forest_reg = function(X_train, y_train, n_tree) {
  rf_model = randomForest(
                    x = X_train,
                    y = y_train,
                    ntree = n_tree,
                    mtry = sqrt(ncol(X_train)) 
                  )
  
  return (rf_model)
}

# XGBOOST MODEL ----------------------------------------------------------------

# XGB Regressor Model Definition with hyperparameter tuning
xgb_reg_tuning = function(X_train, y_train_scaled, parameter_grid, train_control) {
  
  # Perform parameter tuning
  xgb_tuned = train(
    x = as.matrix(X_train),
    y = y_train_scaled,
    method = "xgbTree",
    trControl = train_control,
    tuneGrid = parameter_grid
  )
  
  # Return the models trained
  return (xgb_tuned)
}