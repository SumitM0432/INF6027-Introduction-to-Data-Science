# SETTING THE THEME AND FONT FOR ALL THE GRAPHS
theme_set(
  theme_minimal() +
    theme(text = element_text(family = 'mono'),
          plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
          axis.text.x = element_text(size = 10, face = 'bold'),
          axis.text.y = element_text(size = 10, face = 'bold'))
)

#### SONGS ---------------------------------------------------------------------
# Popularity Distribution
ggplot(df_meta_songs, aes(x = popularity)) +
  geom_histogram(fill = "lightseagreen", bins = 30)

count_pop = df_meta_songs %>%
  group_by(popularity) %>%
  summarize(coun = n(),
            av = mean(popularity)) %>%
  filter(is.na(popularity) == FALSE)

# Songs having a rating of more than and equal to 50
sum(count_pop[count_pop$popularity >= 50, c('coun')])

# Explicit bargraph
ggplot(df_meta_songs, aes(x = explicit)) +
  geom_bar(fill = "lightseagreen")

count_ex = df_meta_songs %>%
  group_by(explicit) %>%
  summarize(coun = n())

# Songtype bargraph
ggplot(df_meta_songs, aes(x = song_type)) +
  geom_bar(fill = "lightseagreen")

count_stype = df_meta_songs %>%
  group_by(song_type) %>%
  summarize(coun = n())

# Looking at the top 10 artist with the highest number of songs sung solo or collaboration
df_meta_songs = df_meta_songs %>%
  # Getting the Artists IDs
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

# top 10 artist with most songs
ggplot(df_exploded, aes(x = fct_infreq(name, song_count), y = song_count)) +
  geom_col(fill = "lightseagreen") +
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold', angle = 60, vjust = 1.1, hjust = 1),
        axis.text.y = element_text(size = 10, face = 'bold'))

# Avg year_end_score over the years for the songs
ggplot(df_pop_songs %>% group_by(year) %>% summarize(avg_year_end_score = mean(year_end_score)),
       aes(x = year, y = avg_year_end_score, fill = explicit)) +
  geom_area(fill = 'lightseagreen')

# Avg year_end_score over the years for the songs based on explicit true or false
avg_explicit_yes = df_pop_songs %>% left_join(df_meta_songs %>% select(song_id, explicit), by = c('song_id'))

ggplot(avg_explicit_yes %>% group_by(year, explicit) %>% summarize(avg_year_end_score = mean(year_end_score)),
       aes(x = year, y = avg_year_end_score, fill = explicit)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(values = c("True" = "red3", "False" = "seagreen3"))

# Avg year_end_score over the years for the songs based on song_type Solo or Collaboration
avg_sol_c_yes = df_pop_songs %>% left_join(df_meta_songs %>% select(song_id, song_type), by = c('song_id'))

ggplot(avg_sol_c_yes %>% group_by(year, song_type) %>% summarize(avg_year_end_score = mean(year_end_score)),
       aes(x = year, y = avg_year_end_score, fill = song_type)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(values = c("Solo" = "steelblue", "Collaboration" = "orange"))

#### ARTIST --------------------------------------------------------------------

# Popularity Distribution
ggplot(df_meta_artists, aes(x = popularity)) +
  geom_histogram(fill = "lightseagreen", bins = 30)

count_pop = df_meta_artists %>%
  group_by(popularity) %>%
  summarize(coun = n(),
            av = mean(popularity)) %>%
  filter(is.na(popularity) == FALSE)

# Songs having a rating of more than and equal to 50
sum(count_pop[count_pop$popularity >= 50, c('coun')])

# followers distribution (not a good idea)
ggplot(df_meta_artists %>%
         mutate(followers = ifelse(followers == 'None', 0, followers),
                followers = as.numeric(followers)),
       aes(x = followers)) +
  geom_histogram(fill = "lightseagreen", bins = 30)

# Total followers of artists over the years
agg_followers = df_pop_artists %>%
  left_join(df_meta_artists %>% select(artist_id, followers), by = c('artist_id')) %>%
  mutate(followers = ifelse(followers == 'None', 0, followers),
         followers = as.numeric(followers)) %>%
  group_by(year) %>%
  summarize(
    sum_fol = sum(followers, na.rm = TRUE)
  )

ggplot(agg_followers, aes(x = year, y = sum_fol)) +
  geom_area(fill = "darkgreen") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
  scale_x_continuous(breaks = seq(min(agg_followers$year), max(agg_followers$year), by = 6))

# Artist type bargraph
ggplot(df_meta_artists %>%
         mutate(artist_type = ifelse(artist_type == "'band'", 'band', artist_type),
                artist_type = ifelse(artist_type == '-', 'Not Given', artist_type)), 
       aes(x = artist_type)) +
  geom_bar(fill = "lightseagreen")

# Most common top 10 main genres of the artist (not including the cases when it's not given)
genre_count_df = df_meta_artists %>%
  mutate(main_genre = ifelse(main_genre == '-', 'Not Given', main_genre)) %>% group_by(main_genre) %>% summarize(genres_count = n()) %>%
  arrange(desc(genres_count)) %>%
  head(11) %>%
  filter(main_genre != 'Not Given')

ggplot(genre_count_df, aes(x = fct_infreq(main_genre, genres_count), y = genres_count)) +
  geom_col(fill = "lightseagreen") +
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold', ),
        axis.text.x = element_text(size = 10, face = 'bold', angle = 60, vjust = 1.2, hjust = 1),
        axis.text.y = element_text(size = 10, face = 'bold'))

# avg year end score of artist based on their artist type
test_meta = df_pop_artists %>%
  left_join(df_meta_artists %>% select(artist_id, artist_type) %>% distinct(), by = c('artist_id')) %>%
  mutate(artist_type = ifelse(artist_type == "'band'", 'band', artist_type),
         artist_type = ifelse(artist_type == '-', 'Not Given', artist_type),
         artist_type = ifelse(is.na(artist_type), 'Not Given', artist_type)) %>%
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
  labs(fill = "Artist Type")

#### LYRICAL FEATURES ----------------------------------------------------------
# Running the Preprocessing and Features Engineering Script
source("2_data_preprocessing.R")

#### IMP
df_pop_nas = df_pop_songs %>%
  left_join(df_meta_songs %>% select(song_id, popularity) %>% distinct(), by = c('song_id')) %>%
  mutate(popularity = as.character(popularity)) %>%
  group_by(popularity) %>%
  summarize(
    avg_yes = mean(year_end_score, na.rm = TRUE)
  )

median(df_meta_songs_eda$year, na.rm = TRUE)

df_meta_songs_eda = df_meta_songs_eda %>%
  left_join(df_pop_nas %>% mutate(popularity = as.integer(popularity)), by = c('popularity')) %>%
  mutate(
    year_end_score_song = ifelse(is.na(year_end_score_song), as.integer(avg_yes), year_end_score_song),
    year = ifelse(is.na(year), median(year, na.rm = TRUE), year)
  ) 


# Distribution of sentiment polarity based on explicit content
ggplot(df_meta_songs_eda, aes(x = sentiment_polarity, fill = explicit)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.1))

# Distribution of sentiment polarity based on song_type content
ggplot(df_meta_songs_eda, aes(x = sentiment_polarity, fill = song_type)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.1))

# Correlation between lyrical features and the popularity
cor_matrix <- cor(na.omit(df_meta_songs_eda[, c("popularity", "sentiment_polarity", "objectivity", 
                         "word_count", "lexical_diversity", "avg_word_length", "repetition_ratio")]))

ggplot(data = melt(cor_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), color = "gray0", size = 4) +
  scale_fill_gradient2(low = "steelblue4", high = "darkred", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "Features", y = "Features") +
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
        axis.text.x = element_text(angle = 50, size = 10, face = 'bold', vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10, face = 'bold')) 

# Fitting a lm to see how does the repetition_ratio works to predict popularity
ggplot(df_meta_songs_eda, aes(x = repetition_ratio, y = popularity)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(title = "Popularity vs. Repetition Ratio", x = "Repetition Ratio", y = "Popularity") +
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
        axis.text.x = element_text(angle = 50, size = 10, face = 'bold', vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10, face = 'bold')) 

# Change of avg lexical diversity over the years for all the songs
df_meta_songs_eda %>%
  group_by(year) %>%
  summarize(avg_lex = mean(lexical_diversity, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_lex)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Average Lexical Diversity Over Time", x = "Year", y = "Lexical Diversity")

# Change of avg Sentiment polarity over the years for all the songs
df_meta_songs_eda %>%
  group_by(year) %>%
  summarize(avg_sentiment = mean(sentiment_polarity, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_sentiment)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Average Sentiment Polarity Over Time", x = "Year", y = "Sentiment Polarity")

# Change of avg word count over the years for all the songs (interesting)
df_meta_songs_eda %>%
  group_by(year) %>%
  summarize(avg_word_c = mean(word_count, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_word_c)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Average Word Count Over Time", x = "Year", y = "Word Count")

# Change of avg word length over the years for all the songs
df_meta_songs_eda %>%
  group_by(year) %>%
  summarize(avg_word_l = mean(avg_word_length, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_word_l)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Average Word Length Over Time", x = "Year", y = "Word Length")

#### SONG FEATURES -------------------------------------------------------------

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