folder_creation_check = function() {
  # Description : This function check if the required folders for the process are present or not
  #               If the folder is not present, it create them so the scripts run uninterrupted
  
  # Creating the Results Folder to save the plots for results
  if (!dir.exists("../2_Outputs/Plots/Results")){
    dir.create("../2_Outputs/Plots/Results/With Lyrics", recursive = TRUE)
    dir.create("../2_Outputs/Plots/Results/Without Lyrics", recursive = TRUE)
    
    print("Results Directory Created!")
  }else{
    print("Results Directory Exist!")
  }
  
  # Creating the EDA Folder
  if (!dir.exists("../2_Outputs/Plots/EDA")){
    dir.create("../2_Outputs/Plots/EDA", recursive = TRUE)
    
    print("EDA Directory Created!")
  }else{
    print("EDA Directory Exist!")
  }
  
  # Creating the Trained Model Folder
  if (!dir.exists("../2_Outputs/Trained_Models")){
    dir.create("../2_Outputs/Trained_Models/With Lyrics", recursive = TRUE)
    dir.create("../2_Outputs/Trained_Models/Without Lyrics", recursive = TRUE)
    
    print("Trained Model Directory Created!")
  }else{
    print("Trained Model Directory Exist!")
  }
  
  # Creating the Trained Model Folder
  if (!dir.exists("../2_Outputs/RData")){
    dir.create("../2_Outputs/RData", recursive = TRUE)
    
    print("RData Directory Created!")
  }else{
    print("RData Directory Exist!")
  }
}

extract_artist = function(val) {
  # Description : This function clean and extract the list of artists present in the df_meta_songs as one song can have multiple artists
  
  # Matches a literal expression saying extract anything between ' - ': where .* means match any sequence and ? means make it non greedy
  id = str_extract_all(val, "'(.*?)':")[[1]]
  # Replace ' and :
  id = str_replace_all(id, "[':]", "")
  # Replace anything that is there before ,
  id = str_replace_all(id, "(.*?), ", "")
  # Making a vector
  id_vector = as.vector(id)
  
  return (id_vector)
}

get_artist_features = function (artist_ids_list, df_meta_artists = df_meta_artists) {
  # Description : This function extract and aggregate the features of artists from df_meta_artist
  #               Features aggregated are followers, popularity, main genres and number of artists
  
  # Getting the artist ids as a list
  artist_ids_list = unlist(artist_ids_list)
  
  artist_info = df_meta_artists %>%
    # Filtering for the artist ids in the questions
    filter(artist_id %in% artist_ids_list) %>%
    # Summarizing and extracting the required features
    summarise(
      total_followers = sum(followers, na.rm = TRUE),
      avg_popularity = mean(popularity, na.rm = TRUE),
      unique_main_genre = n_distinct(main_genre, na.rm = TRUE),
      unique_diff_artist = n_distinct(artist_type, na.rm = TRUE)
    )
  
  # Returning all the features engineered
  return (as.list(artist_info))
}

get_artist_year_end_score = function(artist_ids_list, df_pop_artists = df_pop_artists) {
  # Description : This function extract the average year end score of artists taking for the latest year
  
  # Getting the artist ids as a list
  artist_ids_list = unlist(artist_ids_list)
  
  # Getting the Average year end score of the artist
  artist_info = df_pop_artists %>%
    filter(artist_id %in% artist_ids_list) %>%
    summarise(
      avg_artist_year_end_score = mean(year_end_score, na.rm = TRUE)
    )
  
  # Returning the average year end score for artists
  return (artist_info %>% pull(avg_artist_year_end_score))
}

evaluation_metrics = function(results_data) {
  # Description : This function evaluate the model performance using the RMSE and R-Squared metrics
  
  # Root Mean Squared Error (RMSE)
  rmse_val = rmse(results_data, truth = target_values,estimate = predicted_values)
  # R-squared
  rsq_val = rsq(results_data, truth = target_values, estimate = predicted_values)
  
  # Printing out the values
  print (paste0("Root Mean Squared Error :: ", round(rmse_val$.estimate, 3)))
  print (paste0("R Squared Error :: ", round(rsq_val$.estimate, 3)))
  
}

pred_vs_actual_plot = function(results_data, model_name) {
  # description : This function plot the prediction vs Actual song popularity.
  
  # Predicted vs Actual Plot
  pred_v_act = ggplot(results_data, aes(x = target_values, y = predicted_values)) +
    geom_point(alpha = 0.5, color = "turquoise3") +  # Points
    geom_abline(slope = 1, intercept = 0, color = "red4", linetype = "dashed", linewidth = 1.2) +
    labs(
      title = "Predicted vs Actual Values",
      x = "Actual Values",
      y = "Predicted Values"
    ) +
    theme_minimal() +
    theme(text = element_text(family = 'mono'),
          plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
          axis.text.x = element_text(size = 10, face = 'bold'),
          axis.text.y = element_text(size = 10, face = 'bold'))
  
  # Plotting
  plot(pred_v_act)
  
  # Saving the plot in a folder
  ggsave(paste0("Predicted_v_Actual_" , model_name, ".jpeg"), pred_v_act, path = path_for_results) 
}

residual_plot = function(results_data, model_name) {
  # Description : This function plot the residuals for the models
  
  # Calculating Residuals
  results_data = results_data %>%
    mutate(residuals = target_values - predicted_values)
  
  # Residuals Density/Histogram Plot
  res_plot = ggplot(results_data, aes(x = residuals)) +
    geom_histogram(fill = "steelblue", alpha = 0.5, bins = 60) +
    geom_vline(xintercept = 0, color = "darkred", linetype = "dashed", linewidth = 1.2) +
    labs(
      title = "Residuals Distribution",
      x = "Residuals",
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(text = element_text(family = 'mono'),
          plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
          axis.text.x = element_text(size = 10, face = 'bold'),
          axis.text.y = element_text(size = 10, face = 'bold'))
  
  # Plotting
  plot(res_plot)
  
  # Saving the plot in a folder
  ggsave(paste0("Residual_Plot_" , model_name, ".jpeg"), res_plot, path = path_for_results) 
}

feature_importance_plot = function(feature_importance_df, model_name_title, importance_type) {
  # Description : This function plots the feature importance bar graph
  
  feature_imp_plot = ggplot(feature_importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = ifelse(lyrical_switch == TRUE, 'lightseagreen', 'orange1')) + # Changing colors based on the usage of lyrical features
    coord_flip() +
    labs(
      title = paste0("Feature Importance : ", model_name_title),
      x = "Features",
      y = paste0("Importance (", importance_type, ")")
    ) +
    theme_minimal() +
    theme(text = element_text(family = 'mono'),
          plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
          axis.text.x = element_text(size = 10, face = 'bold'),
          axis.text.y = element_text(size = 10, face = 'bold'))
  
  # Plotting
  plot(feature_imp_plot)
  
  # Saving the plot
  ggsave(paste0("Feature_Importance ", model_name_title, ".jpeg"), feature_imp_plot, path = path_for_results) 
}