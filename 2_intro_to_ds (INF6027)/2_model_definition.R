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

# XGB Regressor Model Definition
xgb_reg = function(params, X_train, y_train, nrounds, verbose = 0) {
  xgb_model = xgboost(
                data = as.matrix(X_train),
                label = as.matrix(y_train),
                params = params,
                nrounds = nrounds,                   
                verbose = verbose                     
              )
  
  return (xgb_model)
}