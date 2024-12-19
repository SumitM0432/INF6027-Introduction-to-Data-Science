print_description = function(df) {
  print (paste0("No. of Rows :: ", nrow(df)))
  print (paste0("No. of Columns :: ", ncol(df)))
  
  # Column Names
  ls = colnames(df)
  
  return (ls)
}

extract_artist = function(val) {
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

check_coverage = function(df1, df2, on_col) {
  
  # Selecting the only the ids for data frame 1 (on which we are gonna do the check)
  df1 = df1 %>%
    select(all_of(on_col)) %>%
    distinct()
  
  # Temp flag for the data frame 2 with whom we are gonna check
  df2 = df2 %>%
    mutate(temp_flag = 1) %>%
    select(all_of(c(on_col, 'temp_flag')))
  
  # ids not present count
  not_present = left_join(df1, df2, by = c(on_col)) %>%
    filter(is.na(temp_flag)) %>%
    select(all_of(on_col))
  
  print (paste0("ids not present in the second tables :: count :: ", nrow(not_present)))
  return (not_present)
  
}

get_artist_features = function (artist_ids_list, df_meta_artists = df_meta_artists) {
  
  # Getting the artist ids as a list
  artist_ids_list = unlist(artist_ids_list)
  
  artist_info = df_meta_artists %>%
    # Filtering for the artist ids in the questions
    filter(artist_id %in% artist_ids_list) %>%
    # Changing the data type for the followers for the aggregation
    mutate(followers = as.numeric(followers)) %>%
    # interpolation NA's with 0 since artist has no followers
    mutate(ifelse(is.na(followers), 0, followers)) %>%
    # Summarizing
    summarise(
      total_followers = sum(followers, na.rm = TRUE),
      avg_popularity = mean(popularity, na.rm = TRUE),
      unique_m_genre = n_distinct(main_genre, na.rm = TRUE)
    )
  
  # return (artist_info$total_followers, artist_info$avg_popularity, artist_info$unique_m_genre)
  return (as.list(artist_info))
}

get_artist_year_end_score = function(artist_ids_list, df_pop_artists = df_pop_artists) {
  # Getting the artist ids as a list
  artist_ids_list = unlist(artist_ids_list)
  
  # Getting the Average year end score of the artist
  artist_info = df_pop_artists %>%
    filter(artist_id %in% artist_ids_list) %>%
    summarise(
      avg_artist_year_end_score = mean(year_end_score, na.rm = TRUE)
    )
  
  return (artist_info %>% pull(avg_artist_year_end_score))
}

evaluation_metrics = function(results_data) {
  
  # Root Mean Squared Error (RMSE)
  rmse_val = rmse(results_data, truth = target_values,estimate = predicted_values)
  # Mean Absolute Error (MAE)
  mae_val = mae(results_data, truth = target_values, estimate = predicted_values)
  # R-squared
  rsq_val = rsq(results_data, truth = target_values, estimate = predicted_values)
  
  print (paste0("Root Mean Squared Error :: ", round(rmse_val$.estimate, 3)))
  print (paste0("Mean Absolute Error :: ", round(mae_val$.estimate, 3)))
  print (paste0("R Squared Error :: ", round(rsq_val$.estimate, 3)))
  
}

pred_vs_actual_plot = function(results_data, model_name) {
  
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
  ggsave(paste0("Predicted_v_Actual_" , model_name, ".jpeg"), pred_v_act, path = paste0(getwd(), "/Plots")) 
}

# https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf -- used for colors

residual_plot = function(results_data, model_name) {
  
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
  ggsave(paste0("Residual_Plot_" , model_name, ".jpeg"), res_plot, path = paste0(getwd(), "/Plots")) 
}
