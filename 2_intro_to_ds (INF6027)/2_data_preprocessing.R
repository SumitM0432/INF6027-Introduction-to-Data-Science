# DATA PREPROCESSING ------------------------------------------------------
print(paste('--------------------------------', Sys.time(), 'ADDING SONGS FEATURES', '---'))
# Dropping the null values (only one where the song_id is just and empty string)
df_meta_songs = df_meta_songs %>%
  filter(!song_id %in% c(""))

# Getting the maximum year for the every song by grouping and summarizing
df_songs_max_year = df_pop_songs %>%
  group_by(song_id) %>%
  summarise(
    max_year = max(year)
  )

# Joining the maximum year and filtering on it to get the latest year_end_score
df_pop_songs = df_pop_songs %>%
  left_join(df_songs_max_year, by = c('song_id')) %>%
  filter(year == max_year) %>%
  rename("year_end_score_song" = "year_end_score") %>%
  select(-c(max_year)) %>%
  distinct()

# Joining with the df_meta_song to get the overall dataset with latest year_end_score and the year
df_meta_songs = df_meta_songs %>%
  left_join(df_pop_songs, by = c('song_id'))

# Imputing the Null year_end_score_song and year
# For year_end_score we will calculate the average year end score for every popularity rating and impute the NA values based on that
# For year we will take the median of the year column and impute NA Values
df_pop_impute = df_pop_songs %>%
  left_join(df_meta_songs %>% select(song_id, popularity) %>% distinct(), by = c('song_id')) %>%
  mutate(popularity = as.character(popularity)) %>%
  group_by(popularity) %>%
  summarize(
    avg_yes = mean(year_end_score_song, na.rm = TRUE)
  )

df_meta_songs = df_meta_songs %>%
  # Joining the df_pop_impute to get the values to fill NA
  left_join(df_pop_impute %>% mutate(popularity = as.integer(popularity)), by = c('popularity')) %>%
  mutate(
    # Imputing the NA's with the appropriate values
    year_end_score_song = ifelse(is.na(year_end_score_song), as.integer(avg_yes), year_end_score_song),
    # Imputing the year with the median of the years
    year = ifelse(is.na(year), median(year, na.rm = TRUE), year)
  ) 

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

# Changing the inconsistent band names and also giving a new category to the artist type as not given" if there's '-'
# Doing the same with '-' in main_genre as 'not given'
df_meta_artists = df_meta_artists %>%
  # Changing 'band' to band
  mutate(artist_type = ifelse(artist_type == "'band'", 'band', artist_type)) %>%
  # Changing the NA given as '-'
  mutate(artist_type = ifelse(artist_type == '-', 'not given', artist_type)) %>%
  # Changing the NA given as '-'
  mutate(main_genre = ifelse(main_genre == '-', 'not given', main_genre))
  
# Adjusting follower column since it's character with some strings as 'None'
df_meta_artists = df_meta_artists %>%
  mutate(followers = ifelse(followers == 'None', '0', followers)) %>%
  # Changing from character to integer
  mutate(followers = as.integer(followers))

# Making new features like the total followers of the artists involved, their average popularity and there top unique music genres
df_meta_songs = df_meta_songs %>%
  rowwise() %>%
  # Getting the total followers, unique genres for them and average popularity
  mutate(artist_info = list(get_artist_features(artist_id_vectors, df_meta_artists))) %>%
  mutate(total_artist_followers = artist_info$total_follower,
         avg_artist_popularity = artist_info$avg_popularity,
         unique_artist_main_genre = artist_info$unique_main_genre,
         unique_artist_type = artist_info$unique_diff_artist) %>%
  # Removing redundant column
  select(-c(artist_info)) %>%
  # Ungrouping
  ungroup()

# Filling Nulls of avg_popularity with 0 since we don't have data on the artist
# Note: there are not a lot of null in this column so we can safely put 0
df_meta_songs = df_meta_songs %>%
  mutate(avg_artist_popularity = ifelse(is.na(avg_artist_popularity), 0, avg_artist_popularity))

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

# Getting the average artist year end score and we take the latest score for each artist and then take the average since
# There could be more than 1 artist per song
df_meta_songs = df_meta_songs %>%
  rowwise() %>%
  # Getting the average year end score
  mutate(avg_artist_year_end_score = get_artist_year_end_score(artist_id_vectors, df_pop_artists)) %>%
  # Ungrouping
  ungroup() %>%
  # Converting to integer datatype
  mutate(avg_artist_year_end_score = as.numeric(avg_artist_year_end_score)) %>%
  # Also filling the nulls with avg
  mutate(avg_artist_year_end_score = ifelse(is.na(avg_artist_year_end_score),
                                            mean(avg_artist_year_end_score, na.rm = TRUE),
                                            avg_artist_year_end_score)
         )

print(paste('--------------------------------', Sys.time(), 'ADDING LYRICAL FEATURES', '-'))

# CLEANING THE LYRICS
# Removing unwanted character and also lower casing the words
df_lyrics = df_lyrics %>%
  # Removing the Unwanted Character except letters, and numbers from the lyrics
  mutate(cleaned_lyrics = gsub("[^a-zA-Z0-9\\s]", " ", lyrics)) %>%
  # Converting the lyrics to lower case
  mutate(cleaned_lyrics = tolower(cleaned_lyrics)) %>%
  # Making a new lyrics column to calculate the features where stop words also plays an important role
  mutate(cleaned_lyrics_stop = cleaned_lyrics)

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
  mutate(cleaned_lyrics = gsub("\\s+", " ", cleaned_lyrics),
         cleaned_lyrics_stop = gsub("\\s+", " ", cleaned_lyrics_stop)) %>%
  # Leading and Trailing spaces
  mutate(cleaned_lyrics = trimws(cleaned_lyrics),
         cleaned_lyrics_stop = trimws(cleaned_lyrics_stop))

# EXTRACTING THE FEATURES
# - Sentiment Polarity
# - Objectivity (1 - Subjectivity)
# - Word Count
# - Lexical Diversity
# - Average Word Length
# - Repetition Ratio

df_lyrics = df_lyrics %>%
  # Calculating Word Count (including stop words to look at the features holistically)
  mutate(word_count = str_count(cleaned_lyrics_stop, "\\S+")) %>%
  # Calculating Sentiment Polarity (without stop words since stop words doesn't have any sentiment)
  mutate(sentiment_polarity = get_sentiment(cleaned_lyrics, method = "syuzhet") / word_count) %>%
  # Calculating Objectivity Score from the Subjectivity Score (Normalized)
  mutate(subjectivity = get_sentiment(cleaned_lyrics, method = "afinn") / word_count) %>%
  mutate(objectivity = 1 - subjectivity)

# Calculating the Lexical Diversity (including stop words)
lexical_df = df_lyrics %>%
  # Tokenizing the lyrics into individual words
  unnest_tokens(word, cleaned_lyrics_stop) %>%
  # grouping by each song
  group_by(song_id) %>%
  summarise(
    lexical_diversity = n_distinct(word) / n(),  # Calculating Type-Token Ratio
    .groups = "drop" # Ungrouping the data table
  )

# Joining with main lyrical data table
df_lyrics = df_lyrics %>%
  left_join(lexical_df, by = c('song_id'))

# Calculating average word length (including stop words)
df_lyrics = df_lyrics %>%
  mutate(avg_word_length = sapply(strsplit(cleaned_lyrics_stop, "\\s+"), 
                                  function(words) mean(nchar(words))))

# Calculating repetition ratio (including stop words)
df_lyrics = df_lyrics %>%
  mutate(
    total_words = sapply(strsplit(cleaned_lyrics_stop, "\\s+"), length),
    unique_words = sapply(strsplit(cleaned_lyrics_stop, "\\s+"), function(words) length(unique(words)))
  ) %>%
  mutate(repetition_ratio = 1 - (unique_words/total_words))

# Taking care of some unusual cases where the lyrics are just "ssss", "", and "instrumental"
# These lyrics justify nothing except for instrumental which depicts it has no lyrics so we handle these use cases
# We reset the values to neutral since they can't be defined without the lyrics
df_lyrics = df_lyrics %>%
  mutate(
    sentiment_polarity = ifelse(cleaned_lyrics %in% c("ssss", "", "instrumental"), 0, sentiment_polarity),
    objectivity = ifelse(cleaned_lyrics %in% c("ssss", "", "instrumental"), 0.5, objectivity),
    word_count = ifelse(cleaned_lyrics %in% c("ssss", "", "instrumental"), 0, word_count),
    lexical_diversity = ifelse(cleaned_lyrics %in% c("ssss", "", "instrumental"), 0, lexical_diversity),
    avg_word_length = ifelse(cleaned_lyrics %in% c("ssss", "", "instrumental"), 0, avg_word_length),
    repetition_ratio = ifelse(cleaned_lyrics %in% c("ssss", "", "instrumental"), 0, repetition_ratio)
  )

# Selecting the required columns
df_lyrics = df_lyrics %>%
  select(song_id, sentiment_polarity, objectivity, word_count, lexical_diversity, avg_word_length, repetition_ratio)

# Final join to get the lyrical features combined with songs features we processed earlier and dropping NA's
df_meta_songs = df_meta_songs %>%
  left_join(df_lyrics, by = c('song_id')) %>%
  # Dropping the NA's where we can't calculate the lyrical features because the lyrical data is NULL for those songs
  drop_na()

# Just Saving this dataset for EDA of lyrical features
df_meta_songs_eda = copy(df_meta_songs)

print (paste0("Dataset Size After Feature Engineering :: ", nrow(df_meta_songs)))

print(paste('--------------------------------', Sys.time(), 'FURTHER PROCESSING', '------'))
# Removing the columns that won't be used for the training and testing
df_meta_songs = df_meta_songs %>%
  select(-c(song_id, song_name, billboard, artists, artist_id_vectors))

# Label Encoding the songs_type and explicit columns
df_meta_songs_encoded = df_meta_songs %>%
  mutate(
    explicit = ifelse(explicit == "True", 1, 0),  # TRUE as 1, FALSE as 0
    song_type = ifelse(song_type == "Solo", 1, ifelse(song_type == "Collaboration", 2, NA))  # Solo as 1, Collaboration as 2
  )

# Finally omitting any NA's that are there left
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