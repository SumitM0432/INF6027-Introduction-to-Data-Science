# Setting the Current Path
if (interactive()) {
  # If in RStudio, use rstudioapi
  if ("rstudioapi" %in% rownames(installed.packages())) {
    setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  }
}

# Getting the libraries and the required functions
source("../0_config/0_libraries.R", local = TRUE)
source("../0_config/0_functions.R", local = TRUE)

# Loading the Data
source("music_data_loading.R", local = TRUE)

# Loading the RData
load("RData/Loaded_MusicData.RData")

# 20406 -> 20405 # No NULLS in df_chart_songs and df_pop_songs
# No NULLS in df_pop_artists, df_chart_artists and df_meta_artists
# No NULLS in df_acoustic_features
# 744 NULLS in lyrics

df_meta_songs = df_meta_songs %>%
  # Dropping the null values
  drop_na()

df_meta_songs = df_meta_songs %>%
  # Getting the Artists IDs
  mutate(artist_id_vectors := mapply(extract_artist, artists)) %>%
  # Counting the number of artist that has worked on that song
  mutate(num_artist = str_count(artist_id_vectors, ",") + 1)

temp = df_meta_songs %>%
  mutate(temp_ids = artist_id_vectors) %>%
  separate_rows(artist_id_vectors, sep = ",")

get_artist_info = function (artist_ids_list, df_meta_artists = df_meta_artists) {
  artist_ids_list = artist_ids_list[[1]]
  
  artist_info = df_meta_artists %>%
    filter(artist_id %in% artist_ids_list) %>%
    mutate(followers = as.numeric(followers)) %>%
    group_by() %>%
    summarise(
      total_followers = sum(followers),
      avg_popularity = mean(popularity)
    )
  
}



