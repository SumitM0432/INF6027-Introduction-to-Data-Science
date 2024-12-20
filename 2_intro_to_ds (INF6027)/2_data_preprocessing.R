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
  rename("year_end_score_song" = "year_end_score") %>%
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
  mutate(avg_artist_year_end_score = get_artist_year_end_score(artist_id_vectors, df_pop_artists)) %>%
  # Ungrouping
  ungroup() %>%
  # Converting to numeric datatype
  mutate(avg_artist_year_end_score = as.numeric(avg_artist_year_end_score))

print(paste('--------------------------------', Sys.time(), 'ADDING ALBUM FEATURES', '---'))
# Joining with track records to get the album ids
df_meta_songs = df_meta_songs %>%
  left_join(df_meta_tracks %>% select(song_id, album_id, track_number), by = c('song_id'))

# 9105/20405 -> 44.6%
# temp = df_meta_songs %>%
#   left_join(df_meta_albums %>% select(album_id, total_tracks, album_type), by = c('album_id'))

print(paste('--------------------------------', Sys.time(), 'ADDING LYRICAL FEATURES', '-'))

# CLEANING THE LYRICS
# Removing unwanted character and also lower casing the words
df_lyrics = df_lyrics %>%
  # Removing the Unwanted Character except letters, and numbers from the lyrics
  mutate(cleaned_lyrics = gsub("[^a-zA-Z0-9\\s]", " ", lyrics)) %>%
  # Converting the lyrics to lower case
  mutate(cleaned_lyrics = tolower(cleaned_lyrics))

# Removing Stop words
df_lyrics = df_lyrics %>%
  mutate(cleaned_lyrics = cleaned_lyrics %>%
           # Split each row of 'cleaned_lyrics' into individual words using spaces as the delimiter
           strsplit("\\s+") %>%
           # Apply a function to each list of words
           lapply(function(words) 
             # For each list of words, removing those that match the stop words in English
             paste(words[!words %in% stopwords("en")], collapse = " ")
           ) %>%
           # Creating a character vector from list
           unlist())

# Removing extra white spaces
df_lyrics = df_lyrics %>%
  # Multiple spaces between words
  mutate(cleaned_lyrics = gsub("\\s+", " ", cleaned_lyrics)) %>%
  # Leading and Trailing spaces
  mutate(cleaned_lyrics = trimws(cleaned_lyrics))

# EXTRACTING THE FEATURES
# - Sentiment Polarity
# - Objectivity (1 - Subjectivity)
# - Word Count
# - Lexical Diversity
# - Average Word Length
# - Repetition Ratio

df_lyrics = df_lyrics %>%
  # Calculating Word Count
  mutate(word_count = str_count(cleaned_lyrics, "\\S+")) %>%
  # Calculating Sentiment Polarity
  mutate(sentiment_polarity = get_sentiment(cleaned_lyrics, method = "syuzhet") / word_count) %>%
  # Calculating Objectivity Score from the Subjectivity Score (Normalized)
  mutate(subjectivity = get_sentiment(cleaned_lyrics, method = "afinn") / word_count) %>%
  mutate(objectivity = 1 - subjectivity)

# Calculating the Lexical Diversity
lexical_df = df_lyrics %>%
  # Tokenizing the lyrics into individual words
  unnest_tokens(word, cleaned_lyrics) %>%
  # grouping by each song
  group_by(song_id) %>%
  summarise(
    lexical_diversity = n_distinct(word) / n(),  # Calculating Type-Token Ratio
    .groups = "drop" # Ungrouping the data table
  )

# Joining with main lyrical data table
df_lyrics = df_lyrics %>%
  left_join(lexical_df, by = c('song_id'))

# Calculating average word length
df_lyrics = df_lyrics %>%
  mutate(avg_word_length = sapply(strsplit(cleaned_lyrics, "\\s+"), 
                                  function(words) mean(nchar(words))))

# Calculating repetition ratio
df_lyrics = df_lyrics %>%
  mutate(
    total_words = sapply(strsplit(cleaned_lyrics, "\\s+"), length),
    unique_words = sapply(strsplit(cleaned_lyrics, "\\s+"), function(words) length(unique(words)))
  ) %>%
  mutate(repetition_ratio = 1 - (unique_words/total_words))%>%
  # Selecting the required columns
  select(song_id, sentiment_polarity, objectivity, word_count, lexical_diversity, avg_word_length, repetition_ratio)

# Final join to get the lyrical features combined with songs features we processed earlier
df_meta_songs = df_meta_songs %>%
  left_join(df_lyrics, by = c('song_id'))

# Just Saving this dataset for EDA of lyrical features
df_meta_songs_eda = copy(df_meta_songs)

print(paste('--------------------------------', Sys.time(), 'FURTHER PROCESSING', '------'))
# Removing the columns that won't be used for the training and testing
df_meta_songs = df_meta_songs %>%
  select(-c(song_id, song_name, billboard, artists, artist_id_vectors, album_id))

# Label Encoding the songs_type and explicit columns
df_meta_songs_encoded = df_meta_songs %>%
  mutate(
    explicit = ifelse(explicit == "True", 1, 0),  # TRUE as 1, FALSE as 0
    song_type = ifelse(song_type == "Solo", 1, ifelse(song_type == "Collaboration", 2, NA))  # Solo as 1, Collaboration as 2
  )

# omitting the NA's
df_meta_songs_encoded = na.omit(df_meta_songs_encoded)
print (paste0("Final Dataset Size :: ", nrow(df_meta_songs_encoded)))

print(paste('--------------------------------', Sys.time(), 'SPLITTING DATA INTO TRAIN AND TEST DATA', '-'))

# Splitting the data into train and test
train_index <- sample(seq_len(nrow(df_meta_songs_encoded)), size = 0.7 * nrow(df_meta_songs_encoded))
df_train <- df_meta_songs_encoded[train_index, ]
df_test <- df_meta_songs_encoded[-train_index, ]

print (paste0("Training Data Shape :: ", nrow(df_train), ", ", length(df_train)))
print (paste0("Testing Data Shape :: ", nrow(df_test), ", ", length(df_test)))

# FOR RANDOM FOREST AND XGB REGRESSION
# Training and Testing Data for the models
X_train = df_train %>% select(-c(popularity))
X_test = df_test %>% select(-c(popularity))

# Training and Testing Target Variable for the models
y_train = df_train$popularity
y_train_scaled = df_train$popularity / 100 # Scaled Values for regression models
y_test = df_test$popularity

# For LINEAR REGRESSION we need an extra column for scaled value in df_train
df_train = df_train %>%
  mutate(popularity_scaled = popularity / 100) %>%
  select(-c(popularity))

# Saving RData for decrease the data loading time
tables_to_save <- c('X_train', 'X_test', 'y_train', 'y_test', 'df_train', 'df_test', 'df_meta_songs_eda')
save(list = tables_to_save, file = paste0('RData/Processed_Music_Data.RData'))