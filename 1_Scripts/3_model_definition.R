# LINEAR REGRESSION MODEL ------------------------------------------------------

# Linear Regression Model Definition (Baseline)
linear_reg = function (formula, data) {
  linear_model = lm(formula, data = data)
  return (linear_model)
}

# RANDOM FOREST MODEL ----------------------------------------------------------

# Function to train Random Forest with hyperparameter tuning
rf_reg_tuning = function(X_train, y_train_scaled, parameter_grid, train_control) {
  
  rf_model = train(
    x = as.matrix(X_train),        # Ensuring the input is a matrix
    y = y_train_scaled,            # Scaled target variable
    method = "ranger",             
    trControl = train_control,     # Cross-validation
    tuneGrid = parameter_grid,     # Hyperparameters already defined to test
    importance = 'impurity'        # Enabling feature importance
  )
  
  # Returning the models trained
  return(rf_model)
}


# XGBOOST MODEL ----------------------------------------------------------------

# XGB Regressor Model Definition with hyperparameter tuning
xgb_reg_tuning = function(X_train, y_train_scaled, parameter_grid, train_control) {
  
  # Perform parameter tuning
  xgb_tuned = train(
    x = as.matrix(X_train),        # Ensuring the input is a matrix
    y = y_train_scaled,            # Scaled target variable
    method = "xgbTree",
    trControl = train_control,     # Cross-validation
    tuneGrid = parameter_grid      # Hyperparameters already defined to test
  )
  
  # Returning the models trained
  return (xgb_tuned)
}
