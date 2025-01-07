# SETTING THE THEME AND FONT FOR ALL THE GRAPHS
theme_set(
  theme_minimal() +
    theme(text = element_text(family = 'mono'),
          plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'),
          axis.text.x = element_text(size = 10, face = 'bold'),
          axis.text.y = element_text(size = 10, face = 'bold'),
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 10))
)

#### LYRICAL FEATURES ----------------------------------------------------------
# Distribution of sentiment polarity based on explicit content
sentiment_explicit = ggplot(df_meta_songs_eda, aes(x = sentiment_polarity, fill = explicit)) +
  geom_density(alpha = 0.5) +  # Density plot
  scale_x_continuous(breaks = seq(-1, 1, by = 0.1)) +  # Scale for x-axis
  labs(
    title = "Sentiment Polarity Distribution by Explicit Content",
    x = "Sentiment Polarity",
    y = "Density",
    fill = "Explicit"
  )

plot(sentiment_explicit)
ggsave(paste0("sentiment_explicit_distribution.jpeg"), sentiment_explicit, path = "../2_Outputs/Plots/EDA")

# Distribution of sentiment polarity based on song type (Solo/Collaboration)
sentiment_song_type = ggplot(df_meta_songs_eda, aes(x = sentiment_polarity, fill = song_type)) +
  geom_density(alpha = 0.5) +  # Density plot
  scale_x_continuous(breaks = seq(-1, 1, by = 0.1)) +  # Scale for x-axis
  labs(
    title = "Sentiment Polarity Distribution by Song Type",
    x = "Sentiment Polarity",
    y = "Density",
    fill = "Song Type"
  )

plot(sentiment_song_type)
ggsave(paste0("sentiment_song_type_distribution.jpeg"), sentiment_song_type, path = "../2_Outputs/Plots/EDA")

# FINAL CORRELATION HEATMAP
# Calculating the correlation matrix for encoded features and round to two decimals for all the features
correlation_matrix = round(cor(df_meta_songs_encoded), 2)

# Melting the correlation matrix into a long format
melted_correlation_matrix = melt(correlation_matrix)

# Plotting the heatmap
correlation_heatmap = ggplot(data = melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
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
  theme_minimal() +
  theme(
    text = element_text(family = 'mono'),
    plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'),
    axis.text.x = element_text(angle = 50, size = 10, face = 'bold', vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 10, face = 'bold')
  )

plot(correlation_heatmap)
ggsave(paste0("final_correlation_heatmap_with_lyrics.jpeg"), correlation_heatmap, path = "../2_Outputs/Plots/EDA")

# Scatter plot with linear regression line for repetition ratio and popularity
repetition_popularity = ggplot(df_meta_songs_eda, aes(x = repetition_ratio, y = popularity)) +
  geom_point(alpha = 0.6, color = "blue") +  # Scatter plot
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +  # Regression line
  labs(
    title = "Popularity vs. Repetition Ratio",
    x = "Repetition Ratio",
    y = "Popularity"
  )

plot(repetition_popularity)
ggsave(paste0("repetition_vs_popularity.jpeg"), repetition_popularity, path = "../2_Outputs/Plots/EDA")

# Change of average lexical diversity over the years (it's decresing)
lexical_diversity_time = df_meta_songs_eda %>%
  group_by(year) %>%
  summarize(avg_lex = mean(lexical_diversity, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_lex)) +
  geom_line(color = "darkgreen", size = 1) +  # Line plot
  labs(
    title = "Average Lexical Diversity Over Time",
    x = "Year",
    y = "Lexical Diversity"
  )

plot(lexical_diversity_time)
ggsave(paste0("lexical_diversity_over_time.jpeg"), lexical_diversity_time, path = "../2_Outputs/Plots/EDA")

# Change of average sentiment polarity over the years
sentiment_over_time = df_meta_songs_eda %>%
  group_by(year) %>%
  summarize(avg_sentiment = mean(sentiment_polarity, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_sentiment)) +
  geom_line(color = "darkgreen", size = 1) +  # Line plot
  labs(
    title = "Average Sentiment Polarity Over Time",
    x = "Year",
    y = "Sentiment Polarity"
  )

plot(sentiment_over_time)
ggsave(paste0("sentiment_over_time.jpeg"), sentiment_over_time, path = "../2_Outputs/Plots/EDA")

# Change of average word count over the years
word_count_over_time = df_meta_songs_eda %>%
  group_by(year) %>%
  summarize(avg_word_c = mean(word_count, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_word_c)) +
  geom_line(color = "darkgreen", size = 1) +  # Line plot
  labs(
    title = "Average Word Count Over Time",
    x = "Year",
    y = "Word Count"
  )

plot(word_count_over_time)
ggsave(paste0("word_count_over_time.jpeg"), word_count_over_time, path = "../2_Outputs/Plots/EDA")

# Change of average word length over the years
word_length_over_time = df_meta_songs_eda %>%
  group_by(year) %>%
  summarize(avg_word_l = mean(avg_word_length, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_word_l)) +
  geom_line(color = "darkgreen", size = 1) +  # Line plot
  labs(
    title = "Average Word Length Over Time",
    x = "Year",
    y = "Word Length"
  )

plot(word_length_over_time)
ggsave(paste0("word_length_over_time.jpeg"), word_length_over_time, path = "../2_Outputs/Plots/EDA")