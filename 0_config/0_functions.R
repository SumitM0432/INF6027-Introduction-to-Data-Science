print_description = function(df) {
  print (paste0("No. of Rows :: ", nrow(df)))
  print (paste0("No. of Columns :: ", ncol(df)))
  
  # Column Names
  ls = colnames(df)
  
  return (ls)
}

extract_artist = function(val) {
  # Matches a literal expression saying extract anything between ' - ': where .* means match any sequence and ? means make it non greedy
  id <- str_extract_all(val, "'(.*?)':")[[1]]
  # Replace ' and :
  id <- str_replace_all(id, "[':]", "")
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

get_artist_info = function (artist_ids_list, df_meta_artists = df_meta_artists) {
  
  # Getting the artist ids as a list
  artist_ids_list = unlist(artist_ids_list)
  
  artist_info = df_meta_artists %>%
    # Filtering for the artist ids in the questions
    filter(artist_id %in% as.list(artist_ids_list)) %>%
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