#### SONGS ---------------------------------------------------------------------
# Popularity
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

sum(count_pop[count_pop$popularity >= 50, c('coun')])

# Explicit
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

# Songtype
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

#### ARTIST ---------------------------------------------------------------------



