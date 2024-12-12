# DATA PREPROCESSING ------------------------------------------------------

print(paste('--------------------------------', Sys.time(), 'ADDING SONGS FEATURES', '---'))
# Dropping the null values
df_meta_songs = df_meta_songs %>%
  drop_na()

# Getting the maximum year for the every song by grouping and summarizing
df_songs_max_year = df_pop_songs %>%
  group_by(song_id) %>%
  summarise(
    max_year = max(year)
  )

# Joining the maximum year and filtering on it to get the latest score year_end_score
df_pop_songs = df_pop_songs %>%
  left_join(df_songs_max_year, by = c('song_id')) %>%
  filter(year == max_year) %>%
  select(-c(max_year)) %>%
  distinct()

# Joining with the df_meta_song to get the overall dataset with latest year_end_score and the year
df_meta_songs = df_meta_songs %>%
  left_join(df_pop_songs, by = c('song_id'))

# Adding the acoustic features to the data
df_meta_songs = df_meta_songs %>%
  left_join(df_acoustic_features %>% distinct(), by = c('song_id'))

print(paste('--------------------------------', Sys.time(), 'ADDING ARTISTS FEATURES', '-'))
# Converting artist string to a list and counting the number of unique artists there are
df_meta_songs = df_meta_songs %>%
  # Getting the Artists IDs
  mutate(artist_id_vectors := mapply(extract_artist, artists)) %>%
  # Counting the number of artist that has worked on that song
  mutate(num_artist = str_count(as.character(artist_id_vectors), ",") + 1)

# Making new features like the total followers of the artists involved, their average popularity and there top unique music genres
df_meta_songs = df_meta_songs %>%
  rowwise() %>%
  # Getting the total followers, unique genres for them and average popularity
  mutate(artist_info = list(get_artist_features(artist_id_vectors, df_meta_artists))) %>%
  mutate(total_artist_followers = artist_info$total_follower,
         avg_artist_popularity = artist_info$avg_popularity,
         unique_artist_m_genre = artist_info$unique_m_genre) %>%
  # Removing redundant column
  select(-c(artist_info)) %>%
  # Ungrouping
  ungroup()

# Getting the maximum year for the every artist by grouping and summarizing
df_pop_artist_max_year = df_pop_artists %>%
  group_by(artist_id) %>%
  summarise(
    max_year = max(year)
  )

# Joining the maximum year and filtering on it to get the latest score year_end_score
df_pop_artists = df_pop_artists %>%
  left_join(df_pop_artist_max_year, by = c('artist_id')) %>%
  filter(year == max_year) %>%
  select(-c(max_year)) %>%
  distinct()

df_meta_songs = df_meta_songs %>%
  rowwise() %>%
  # Getting the average year end score
  mutate(avg_artist_year_end_score = list(get_artist_year_end_score(artist_id_vectors, df_pop_artists))) %>%
  # Ungrouping
  ungroup()

print(paste('--------------------------------', Sys.time(), 'ADDING ALBUM FEATURES', '---'))
# Joining with track records to get the album ids
df_meta_songs = df_meta_songs %>%
  left_join(df_meta_tracks %>% select(song_id, album_id, track_number), by = c('song_id'))

# 9105/20405 -> 44.6%
# temp = df_meta_songs %>%
#   left_join(df_meta_albums %>% select(album_id, total_tracks, album_type), by = c('album_id'))

print(paste('--------------------------------', Sys.time(), 'ADDING LYRICAL FEATURES', '-'))





print(paste('--------------------------------', Sys.time(), 'FURTHER PROCESSING', '------'))
# Removing the columns that won't be used for the training and testing
df_meta_songs = df_meta_songs %>%
  select(-c(song_id, song_name, billboard, artists, artist_id_vectors, album_id))

# One-Hot Encoding the categorical Variables
df_meta_songs_encoded <- dummy_cols(df_meta_songs,
                                    select_columns = c("explicit", "song_type"),
                                    remove_first_dummy = FALSE) %>%
  select(-c(explicit, song_type)) %>%
  mutate(across(everything(), as.numeric))
         
# omitting the NA's (8 Data Points)
df_meta_songs_encoded = na.omit(df_meta_songs_encoded)
print (paste0("Final Dataset Size :: ", nrow(df_meta_songs_encoded)))

print(paste('--------------------------------', Sys.time(), 'SPLITTING DATA INTO TRAIN AND TEST DATA', '-'))

# Splitting the data into train and test
train_index <- sample(seq_len(nrow(df_meta_songs_encoded)), size = 0.7 * nrow(df_meta_songs_encoded))
df_train <- df_meta_songs_encoded[train_index, ]
df_test <- df_meta_songs_encoded[-train_index, ]

print (paste0("Training Data Shape :: ", nrow(df_train), ", ", length(df_train)))
print (paste0("Testing Data Shape :: ", nrow(df_test), ", ", length(df_test)))

# FOR LINEAR REGRESSION
# Formula specific for the linear regression model
linear_regression_train_cols = popularity ~ .

# FOR RIDGE, LASSO, RANDOM FOREST AND XGB REGRESSION
# Training and Testing Data for the models
X_train = df_train %>% select(-c(popularity))
X_test = df_test %>% select(-c(popularity))

# Training and Testing Target Variable for the models
y_train = df_train$popularity
y_test = df_test$popularity

# Saving RData for decrease the data loading time
tables_to_save <- c('df_meta_songs_encoded', 'X_train', 'X_test', 'y_train', 'y_test', 'df_train', 'df_test', 'linear_regression_train_cols')
save(list = tables_to_save, file = paste0('RData/Processed_Music_Data.RData'))
