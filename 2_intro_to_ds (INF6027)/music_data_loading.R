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

# Setting up the connection with the MySQL Server
con = dbConnect(RMySQL::MySQL(),
                host='localhost',
                port=3306,
                user='root',
                password='qwe123@A@A')

dbSendQuery(con, "SET GLOBAL local_infile = true;")

# Creating and Using the Database
dbSendQuery(con, "CREATE DATABASE IF NOT EXISTS musicoset;")
dbSendQuery(con, "USE musicoset;")

# Music Metadata
album_results = dbSendQuery(con, "SELECT * FROM musicoset.albums;")
df_meta_albums = data.table(fetch(album_results, n = -1))

artists_results = dbSendQuery(con, "SELECT * FROM musicoset.artists;")
df_meta_artists = data.table(fetch(artists_results, n = -1))

releases_results = dbSendQuery(con, "SELECT * FROM musicoset.releases;")
df_meta_releases = data.table(fetch(releases_results, n = -1))

songs_results = dbSendQuery(con, "SELECT * FROM musicoset.songs;")
df_meta_songs = data.table(fetch(songs_results, n = -1))

tracks_results = dbSendQuery(con, "SELECT * FROM musicoset.tracks;")
df_meta_tracks = data.table(fetch(tracks_results, n = -1))

# Music Popularity Pop
album_pop_results = dbSendQuery(con, "SELECT * FROM musicoset.album_pop;")
df_pop_albums = data.table(fetch(album_pop_results, n = -1))

artists_pop_results = dbSendQuery(con, "SELECT * FROM musicoset.artist_pop;")
df_pop_artists = data.table(fetch(artists_pop_results, n = -1))

song_pop_results = dbSendQuery(con, "SELECT * FROM musicoset.song_pop;")
df_pop_songs = data.table(fetch(song_pop_results, n = -1))

# Music Chart
album_chart_results = dbSendQuery(con, "SELECT * FROM musicoset.album_chart;")
df_chart_albums = data.table(fetch(album_chart_results, n = -1))

artists_chart_results = dbSendQuery(con, "SELECT * FROM musicoset.artist_chart;")
df_chart_artists = data.table(fetch(artists_chart_results, n = -1))

song_chart_results = dbSendQuery(con, "SELECT * FROM musicoset.song_chart;")
df_chart_songs = data.table(fetch(song_chart_results, n = -1))

# Music Song Features
af_results = dbSendQuery(con, "SELECT * FROM musicoset.acoustic_features;")
df_acoustic_features = data.table(fetch(af_results, n = -1))

lyrics_results = dbSendQuery(con, "SELECT * FROM musicoset.lyrics;")
df_lyrics = data.table(fetch(lyrics_results, n = -1))

# Saving RData for decrease the data loading time
tables_to_save <- grep("df", ls(), value = TRUE)
save(list = tables_to_save, file = paste0('RData/Loaded_MusicData.RData'))
