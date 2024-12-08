# Training a linear regression model (Baseline)
linear_model <- lm(popularity ~ ., data = df_train)

# View model summary
summary(linear_model)

# Predicting using the trained model
predictions <- predict(linear_model, newdata = df_test)

# Adding the column in the test dataset for evaluation
df_test = df_test %>%
  mutate(predicted_popularity = predictions)

# Making a tibble for evaluation and passing through the custom evaluation function
results_data = tibble(target_values = df_test$popularity,
                      predicted_values = df_test$predicted_popularity)

# Evaluation of the trained model
evaluation_metrics(results_data)

# Saving the model
saveRDS(linear_model, "Results/lm_model_est.rds")
