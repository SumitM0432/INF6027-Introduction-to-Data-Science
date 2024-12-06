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

# Converting artist string to a list and counting the number of unique artists there are
df_meta_songs = df_meta_songs %>%
  # Getting the Artists IDs
  mutate(artist_id_vectors := mapply(extract_artist, artists)) %>%
  # Counting the number of artist that has worked on that song
  mutate(num_artist = str_count(artist_id_vectors, ",") + 1)

# Making new features like the total followers of the artists involved, their average popularity and there top unique music genres
df_meta_songs = df_meta_songs %>%
  rowwise() %>%
  # Getting the total followers, unique genres for them and average popularity
  mutate(artist_info = list(get_artist_info(artist_id_vectors, df_meta_artists))) %>%
  mutate(total_followers = artist_info$total_follower,
         avg_popularity = artist_info$avg_popularity,
         unique_m_genre = artist_info$unique_m_genre) %>%
  # Removing redundant column
  select(-c(artist_info)) %>%
  # Ungrouping
  ungroup()

# Getting the maximum year for the every song by grouping and summarizing
df_songs_my = df_pop_songs %>%
  group_by(song_id) %>%
  summarise(
    max_year = max(year)
  )

# Joining the maximum year and filtering on it to get the latest score year_end_score
df_pop_songs = df_pop_songs %>%
  left_join(df_songs_my, by = c('song_id')) %>%
  filter(year == max_year) %>%
  select(-c(max_year)) %>%
  distinct()

# Joining with the df_meta_song to get the overall dataset with latest year_end_score and the year
df_meta_songs = df_meta_songs %>%
  left_join(df_pop_songs, by = c('song_id'))
