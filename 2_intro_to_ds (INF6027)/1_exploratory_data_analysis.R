#### SONGS ---------------------------------------------------------------------
# Popularity Distribution
ggplot(df_meta_songs, aes(x = popularity)) +
  geom_histogram(fill = "lightseagreen", bins = 30) +
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'))

count_pop = df_meta_songs %>%
  group_by(popularity) %>%
  summarize(coun = n(),
            av = mean(popularity)) %>%
  filter(is.na(popularity) == FALSE)

# Songs having a rating of more than and equal to 50
sum(count_pop[count_pop$popularity >= 50, c('coun')])

# Explicit bargraph
ggplot(df_meta_songs, aes(x = explicit)) +
  geom_bar(fill = "lightseagreen") +
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'))

count_ex = df_meta_songs %>%
  group_by(explicit) %>%
  summarize(coun = n())

# Songtype bargraph
ggplot(df_meta_songs, aes(x = song_type)) +
  geom_bar(fill = "lightseagreen") +
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'))

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
        axis.text.x = element_text(size = 10, face = 'bold', angle = 60, vjust = 1.2, hjust = 1),
        axis.text.y = element_text(size = 10, face = 'bold'))

# Avg year_end_score over the years for the songs
ggplot(df_pop_songs %>% group_by(year) %>% summarize(avg_year_end_score = mean(year_end_score)),
       aes(x = year, y = avg_year_end_score, fill = explicit)) +
  geom_area(fill = 'lightseagreen') +
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'))

# Avg year_end_score over the years for the songs based on explicit true or false
avg_explicit_yes = df_pop_songs %>% left_join(df_meta_songs %>% select(song_id, explicit), by = c('song_id'))

ggplot(avg_explicit_yes %>% group_by(year, explicit) %>% summarize(avg_year_end_score = mean(year_end_score)),
       aes(x = year, y = avg_year_end_score, fill = explicit)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(values = c("True" = "red3", "False" = "seagreen3")) +
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'))

# Avg year_end_score over the years for the songs based on song_type Solo or Collaboration
avg_sol_c_yes = df_pop_songs %>% left_join(df_meta_songs %>% select(song_id, song_type), by = c('song_id'))

ggplot(avg_sol_c_yes %>% group_by(year, song_type) %>% summarize(avg_year_end_score = mean(year_end_score)),
       aes(x = year, y = avg_year_end_score, fill = song_type)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(values = c("Solo" = "steelblue", "Collaboration" = "orange")) +
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'))

#### ARTIST --------------------------------------------------------------------

# Popularity Distribution
ggplot(df_meta_artists, aes(x = popularity)) +
  geom_histogram(fill = "lightseagreen", bins = 30) +
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'))

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
  geom_histogram(fill = "lightseagreen", bins = 30) +
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'))

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
  theme_minimal() +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
  scale_x_continuous(breaks = seq(min(agg_followers$year), max(agg_followers$year), by = 6))
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'))

df_meta_artists %>%
  left_join(df_pop_artists %>% select(year, artist_id))

# Artist type bargraph
ggplot(df_meta_artists %>%
         mutate(artist_type = ifelse(artist_type == "'band'", 'band', artist_type),
                artist_type = ifelse(artist_type == '-', 'Not Given', artist_type)), 
       aes(x = artist_type)) +
  geom_bar(fill = "lightseagreen") +
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'))

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
  theme_minimal() +
  theme(text = element_text(family = 'mono'),
        plot.title = element_text(hjust = 0.5, , size = 15, face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold')) +
  labs(fill = "Artist Type")

#### LYRICAL FEATURES ----------------------------------------------------------