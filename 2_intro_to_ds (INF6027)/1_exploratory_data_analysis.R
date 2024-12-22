# SETTING THE THEME AND FONT FOR ALL THE GRAPHS
theme_set(
  theme_minimal() +
    theme(text = element_text(family = 'mono'),
          plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'),
          axis.text.x = element_text(size = 10, face = 'bold'),
          axis.text.y = element_text(size = 10, face = 'bold'))
)

#### SONGS ---------------------------------------------------------------------
# Dropping NUll since it's redundant here
df_meta_songs = df_meta_songs %>%
  drop_na()

# Popularity Distribution
# Visualizing the distribution of song popularity scores (spread of popularity values across songs)
ggplot(df_meta_songs, aes(x = popularity)) +
  geom_histogram(fill = "lightseagreen", bins = 30) +
  labs(
    title = "Distribution of Song Popularity Scores",
    x = "Popularity",
    y = "Count"
  )

# Grouping songs by popularity and summarizing
count_pop = df_meta_songs %>%
  group_by(popularity) %>%
  summarize(coun = n(),
            av = mean(popularity)) %>%
  filter(is.na(popularity) == FALSE)

# Count of songs with a popularity score >= 50
sum(count_pop[count_pop$popularity >= 50, c('coun')])

# Explicit bar graph (count of explicit and non-explicit songs)
ggplot(df_meta_songs, aes(x = explicit)) +
  geom_bar(fill = "red4") +
  labs(
    title = "Count of Explicit vs. Non-Explicit Songs",
    x = "Explicit",
    y = "Count"
  )

# Grouping songs by explicit content
count_ex = df_meta_songs %>%
  group_by(explicit) %>%
  summarize(coun = n())

# Song type bar graph
# Analyzing the distribution of song types (Solo vs Collaboration)
ggplot(df_meta_songs, aes(x = song_type)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Song Types",
    x = "Song Type",
    y = "Count"
  )

# Grouping by song type
count_stype = df_meta_songs %>%
  group_by(song_type) %>%
  summarize(coun = n())

# Top 10 artists with the highest number of songs
# Breaking down songs by artist to identify those with the highest contributions
df_meta_songs = df_meta_songs %>%
  mutate(artist_id_vectors := mapply(extract_artist, artists))

df_exploded = df_meta_songs[, .(artist_id = unlist(strsplit(as.character(artist_id_vectors), ","))), by = song_id]

df_exploded = df_exploded %>%
  group_by(artist_id) %>%
  summarize(
    song_count = n_distinct(song_id)
  ) %>%
  arrange(desc(song_count)) %>%
  head(10) %>%
  left_join(df_meta_artists %>% select(artist_id, name), by = c('artist_id'))

# Plotting top 10 artists by song count
ggplot(df_exploded, aes(x = fct_infreq(name, song_count), y = song_count)) +
  geom_col(fill = "steelblue3") +
  labs(
    title = "Top 10 Artists by Number of Songs",
    x = "Artist Name",
    y = "Song Count"
  ) +
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold', angle = 60, vjust = 1.1, hjust = 1),
        axis.text.y = element_text(size = 10, face = 'bold'))

# Average year-end score over the years for songs
ggplot(df_pop_songs %>% group_by(year) %>% summarize(avg_year_end_score = mean(year_end_score)),
       aes(x = year, y = avg_year_end_score)) +
  geom_area(fill = 'lightseagreen') +
  labs(
    title = "Average Year-End Score Over Years",
    x = "Year",
    y = "Average Year-End Score"
  )

# Average year_end_score over the years for songs based on whether they are explicit or not
# Joining df_pop_songs with explicit column to include the explicit information
avg_explicit_yes = df_pop_songs %>% 
  left_join(df_meta_songs %>% select(song_id, explicit), by = c('song_id'))

# Plotting the average year_end_score grouped by year and explicit status
ggplot(avg_explicit_yes %>% 
         group_by(year, explicit) %>% 
         summarize(avg_year_end_score = mean(year_end_score, na.rm = TRUE)), # Handle NA values
       aes(x = year, y = avg_year_end_score, fill = explicit)) +
  geom_area(alpha = 0.8) +  # Create an area plot with transparency
  scale_fill_manual(values = c("True" = "red3", "False" = "seagreen3")) +  # Custom colors for explicit status
  labs(
    title = "Average Year-End Score Over the Years by Explicit Status",
    x = "Year",
    y = "Average Year-End Score",
    fill = "Explicit"
  )

# Average year_end_score over the years for songs based on song type (Solo or Collaboration)
# Joining df_pop_songs with song_type column to include the song type information
avg_sol_c_yes = df_pop_songs %>% 
  left_join(df_meta_songs %>% select(song_id, song_type), by = c('song_id'))

# Plotting the average year_end_score grouped by year and song type
ggplot(avg_sol_c_yes %>% 
         group_by(year, song_type) %>% 
         summarize(avg_year_end_score = mean(year_end_score, na.rm = TRUE)), # Handle NA values
       aes(x = year, y = avg_year_end_score, fill = song_type)) +
  geom_area(alpha = 0.8) +  # Create an area plot with transparency
  scale_fill_manual(values = c("Solo" = "steelblue", "Collaboration" = "orange")) +  # Custom colors for song types
  labs(
    title = "Average Year-End Score Over the Years by Song Type",
    x = "Year",
    y = "Average Year-End Score",
    fill = "Song Type"
  )

#### ARTIST --------------------------------------------------------------------
# Popularity Distribution (distribution of artist popularity scores)
ggplot(df_meta_artists, aes(x = popularity)) +
  geom_histogram(fill = "orange", bins = 30) +
  labs(
    title = "Distribution of Artist Popularity Scores",
    x = "Popularity",
    y = "Count"
  )

# Grouping artists by popularity and summarizing
# Summarizing for counts and mean popularity
count_pop = df_meta_artists %>%
  group_by(popularity) %>%
  summarize(
    coun = n(),
    av = mean(popularity)
  ) %>%
  filter(is.na(popularity) == FALSE)

# Count of artists with a popularity score >= 50
# Calculate total number of popular artists
sum(count_pop[count_pop$popularity >= 50, c('coun')])

# Followers Distribution (not a good idea) (the distribution of artist followers)
ggplot(df_meta_artists %>%
         mutate(
           followers = ifelse(followers == 'None', 0, followers),
           followers = as.numeric(followers)
         ),
       aes(x = followers)) +
  geom_histogram(fill = "lightseagreen", bins = 30) +
  labs(
    title = "Distribution of Artist Followers",
    x = "Followers",
    y = "Count"
  )

# Total followers of artists over the years
# Analyzing total followers over time
agg_followers = df_pop_artists %>%
  left_join(df_meta_artists %>% select(artist_id, followers), by = c('artist_id')) %>%
  mutate(
    followers = ifelse(followers == 'None', 0, followers),
    followers = as.numeric(followers)
  ) %>%
  group_by(year) %>%
  summarize(
    sum_fol = sum(followers, na.rm = TRUE)
  )

ggplot(agg_followers, aes(x = year, y = sum_fol)) +
  geom_area(fill = "darkgreen") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
  scale_x_continuous(breaks = seq(min(agg_followers$year), max(agg_followers$year), by = 6)) +
  labs(
    title = "Total Artist Followers Over the Years",
    x = "Year",
    y = "Total Followers (in Millions)"
  )

# Artist Type Bar Graph (Distribution of artist types
ggplot(df_meta_artists %>%
         mutate(
           artist_type = ifelse(artist_type == "'band'", 'band', artist_type),
           artist_type = ifelse(artist_type == '-', 'Not Given', artist_type)
         ), 
       aes(x = artist_type)) +
  geom_bar(fill = "lightseagreen") +
  labs(
    title = "Distribution of Artist Types",
    x = "Artist Type",
    y = "Count"
  )

# Most Common Top 10 Main Genres of the Artist
# Analyzing the most common genres excluding 'Not Given'
genre_count_df = df_meta_artists %>%
  mutate(main_genre = ifelse(main_genre == '-', 'Not Given', main_genre)) %>%
  group_by(main_genre) %>%
  summarize(genres_count = n()) %>%
  arrange(desc(genres_count)) %>%
  head(11) %>%
  filter(main_genre != 'Not Given')

ggplot(genre_count_df, aes(x = fct_infreq(main_genre, genres_count), y = genres_count)) +
  geom_col(fill = "lightseagreen") +
  labs(
    title = "Top 10 Main Genres of Artists",
    x = "Genre",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = 'mono'),
    plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'),
    axis.text.x = element_text(size = 10, face = 'bold', angle = 60, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 10, face = 'bold')
  )

# Average Year-End Score of Artists by Artist Type
# Analyzing year-end scores grouped by artist type and year
test_meta = df_pop_artists %>%
  left_join(df_meta_artists %>% select(artist_id, artist_type) %>% distinct(), by = c('artist_id')) %>%
  mutate(
    artist_type = ifelse(artist_type == "'band'", 'band', artist_type),
    artist_type = ifelse(artist_type == '-', 'Not Given', artist_type),
    artist_type = ifelse(is.na(artist_type), 'Not Given', artist_type)
  ) %>%
  group_by(artist_type, year) %>%
  summarize(
    score_sum = sum(year_end_score, na.rm = TRUE)
  )

ggplot(test_meta, aes(x = year, y = score_sum, fill = artist_type)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(values = c(
    "singer" = "steelblue",
    "rapper" = "darkorange",
    "DJ" = "mediumpurple",
    "band" = "mediumseagreen",
    "Not Given" = "gray",
    "duo" = "lightcoral"
  )) +
  facet_wrap(~artist_type, scales = "free_y") +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = " K")) +
  scale_x_continuous(breaks = seq(min(test_meta$year), max(test_meta$year), by = 10)) +
  labs(
    title = "Year-End Scores by Artist Type Over the Years",
    x = "Year",
    y = "Year-End Score (in Thousands)",
    fill = "Artist Type"
  )

#### LYRICAL FEATURES ----------------------------------------------------------
# Running the Preprocessing and Features Engineering Script
# Ensuring all Preprocessing steps are completed since we need the cleaned up lyrical features for visualization
source("2_data_preprocessing.R")

# Distribution of sentiment polarity based on explicit content
ggplot(df_meta_songs_eda, aes(x = sentiment_polarity, fill = explicit)) +
  geom_density(alpha = 0.5) +  # Density plot
  scale_x_continuous(breaks = seq(-1, 1, by = 0.1)) +  # Scale for x-axis since polarity is normalized
  labs(
    title = "Sentiment Polarity Distribution by Explicit Content",
    x = "Sentiment Polarity",
    y = "Density",
    fill = "Explicit"
  )

# Distribution of sentiment polarity based on song type (Solo/Collaboration)
ggplot(df_meta_songs_eda, aes(x = sentiment_polarity, fill = song_type)) +
  geom_density(alpha = 0.5) +  # Density plot
  scale_x_continuous(breaks = seq(-1, 1, by = 0.1)) +  # Scale for x-axis
  labs(
    title = "Sentiment Polarity Distribution by Song Type",
    x = "Sentiment Polarity",
    y = "Density",
    fill = "Song Type"
  )

# Correlation Heatmap for Lyrical Features and Popularity
# Compute correlation matrix and visualize as heatmap
cor_matrix <- cor(na.omit(df_meta_songs_eda[, c("popularity", "sentiment_polarity", "objectivity", 
                                                "word_count", "lexical_diversity", "avg_word_length", "repetition_ratio")]))

ggplot(data = melt(cor_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +  # Heatmap tiles
  geom_text(aes(label = sprintf("%.2f", value)), color = "gray0", size = 4) +  # Annotations
  scale_fill_gradient2(low = "steelblue4", high = "darkred", mid = "white", midpoint = 0) +  # Gradient scale
  labs(
    title = "Correlation Heatmap",
    x = "Features",
    y = "Features"
  ) +
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'),
        axis.text.x = element_text(angle = 50, size = 10, face = 'bold', vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10, face = 'bold'))

# Scatter plot with linear regression line for repetition ratio and popularity
ggplot(df_meta_songs_eda, aes(x = repetition_ratio, y = popularity)) +
  geom_point(alpha = 0.6, color = "blue") +  # Scatter plot
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +  # Regression line
  labs(
    title = "Popularity vs. Repetition Ratio",
    x = "Repetition Ratio",
    y = "Popularity"
  ) +
  theme_minimal()

# Change of average lexical diversity over the years
df_meta_songs_eda %>%
  group_by(year) %>%
  summarize(avg_lex = mean(lexical_diversity, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_lex)) +
  geom_line(color = "darkgreen", size = 1) +  # Line plot
  labs(
    title = "Average Lexical Diversity Over Time",
    x = "Year",
    y = "Lexical Diversity"
  )

# Change of average sentiment polarity over the years
df_meta_songs_eda %>%
  group_by(year) %>%
  summarize(avg_sentiment = mean(sentiment_polarity, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_sentiment)) +
  geom_line(color = "darkgreen", size = 1) +  # Line plot
  labs(
    title = "Average Sentiment Polarity Over Time",
    x = "Year",
    y = "Sentiment Polarity"
  )

# Change of average word count over the years
df_meta_songs_eda %>%
  group_by(year) %>%
  summarize(avg_word_c = mean(word_count, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_word_c)) +
  geom_line(color = "darkgreen", size = 1) +  # Line plot
  labs(
    title = "Average Word Count Over Time",
    x = "Year",
    y = "Word Count"
  )

# Change of average word length over the years
df_meta_songs_eda %>%
  group_by(year) %>%
  summarize(avg_word_l = mean(avg_word_length, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_word_l)) +
  geom_line(color = "darkgreen", size = 1) +  # Line plot
  labs(
    title = "Average Word Length Over Time",
    x = "Year",
    y = "Word Length"
  )



#### ACOUSTIC FEATURES ---------------------------------------------------------

# Distribution of Acoustic Features
# Plotting density for key acoustic features to observe their distribution
features <- c("acousticness", "danceability", "energy", "valence")

df_acoustic_features_long <- df_acoustic_features %>%
  select(all_of(features)) %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "value")

# Density plot for feature distributions
ggplot(df_acoustic_features_long, aes(x = value, fill = feature)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~feature, scales = "free") +  # Separate plots for each feature
  labs(
    title = "Distribution of Key Acoustic Features", 
    x = "Feature Value", 
    y = "Density"
  ) +
  theme_minimal()

# Tempo vs. Energy Scatter Plot
# Observing the relationship between tempo and energy
# Adding a trend line to identify patterns
ggplot(df_acoustic_features, aes(x = tempo, y = energy)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(
    title = "Relationship Between Tempo and Energy",
    x = "Tempo (BPM)",
    y = "Energy Level"
  ) +
  theme_minimal()

# Danceability by Time Signature
# Using boxplot to compare danceability scores across time signatures
ggplot(df_acoustic_features, aes(x = factor(time_signature), y = danceability, fill = factor(time_signature))) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Danceability by Time Signature",
    x = "Time Signature",
    y = "Danceability"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Heatmap for Acousticness and Energy by Key and Mode
# Creating a heatmap to show average acousticness and energy grouped by key and mode
heatmap_data <- df_acoustic_features %>%
  group_by(key, mode) %>%
  summarise(
    avg_acousticness = mean(acousticness, na.rm = TRUE),
    avg_energy = mean(energy, na.rm = TRUE)
  )

# Heatmap for acousticness
ggplot(heatmap_data, aes(x = factor(key), y = factor(mode), fill = avg_acousticness)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white", midpoint = 0.5, name = "Acousticness"
  ) +
  labs(
    title = "Average Acousticness by Key and Mode",
    x = "Key",
    y = "Mode"
  ) +
  theme_minimal()

# Heatmap for energy
ggplot(heatmap_data, aes(x = factor(key), y = factor(mode), fill = avg_energy)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white", midpoint = 0.5, name = "Energy"
  ) +
  labs(
    title = "Average Energy by Key and Mode",
    x = "Key",
    y = "Mode"
  ) +
  theme_minimal()

# Loudness vs. Liveness for Popular Songs
# Highlighting popular songs based on energy and danceability
df_acoustic_features <- df_acoustic_features %>%
  mutate(is_popular = ifelse(energy > 0.7 & danceability > 0.7, "Popular", "Not Popular"))

ggplot(df_acoustic_features, aes(x = loudness, y = liveness, color = is_popular)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Liveness vs. Loudness for Popular Songs",
    x = "Loudness (dB)",
    y = "Liveness",
    color = "Popularity"
  ) +
  theme_minimal()

# FINAL CORRELATION HEATMAP
correlation_matrix = round(cor(df_meta_songs_encoded), 2)
melted_correlation_matrix = melt(correlation_matrix) # Long Format

ggplot(data = melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), color = "gray0", size = 2) +
  scale_fill_gradient2(
    low = "steelblue", high = "darkred", mid = "white", midpoint = 0,
    limit = c(-1, 1), space = "Lab", name = "Correlation"
  ) +
  labs(
    title = "Correlation Heatmap",
    x = "Features",
    y = "Features"
  ) +
  theme_minimal() +  # Minimal theme for a clean look
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
        axis.text.x = element_text(angle = 50, size = 10, face = 'bold', vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10, face = 'bold'))