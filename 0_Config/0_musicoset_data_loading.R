# Setting up the connection with the MySQL Server (user and password will change according to the user)
con = dbConnect(RMySQL::MySQL(),
                host = 'localhost',
                port = 3306,
                user = 'root',
                password = 'qwe123@A@A')

# Using the database where the sql script is run and tables are saved
dbSendQuery(con, "USE musicoset;")

# Running the queries to ingest the required tables and converting them to data table for further preprocessing
# Music Metadata
artists_results = dbSendQuery(con, "SELECT DISTINCT * FROM musicoset.artists;")
df_meta_artists = data.table(fetch(artists_results, n = -1)) # -1 flag used to ingest all the rows

songs_results = dbSendQuery(con, "SELECT DISTINCT * FROM musicoset.songs;")
df_meta_songs = data.table(fetch(songs_results, n = -1)) # -1 flag used to ingest all the rows

# Music Popularity Pop
artists_pop_results = dbSendQuery(con, "SELECT DISTINCT * FROM musicoset.artist_pop;")
df_pop_artists = data.table(fetch(artists_pop_results, n = -1)) # -1 flag used to ingest all the rows

song_pop_results = dbSendQuery(con, "SELECT DISTINCT * FROM musicoset.song_pop;")
df_pop_songs = data.table(fetch(song_pop_results, n = -1)) # -1 flag used to ingest all the rows

# Music Chart
artists_chart_results = dbSendQuery(con, "SELECT DISTINCT * FROM musicoset.artist_chart;")
df_chart_artists = data.table(fetch(artists_chart_results, n = -1)) # -1 flag used to ingest all the rows

song_chart_results = dbSendQuery(con, "SELECT DISTINCT * FROM musicoset.song_chart;")
df_chart_songs = data.table(fetch(song_chart_results, n = -1)) # -1 flag used to ingest all the rows

# Music Song Features
df_ac_results = dbSendQuery(con, "SELECT DISTINCT * FROM musicoset.acoustic_features;")
df_acoustic_features = data.table(fetch(df_ac_results, n = -1)) # -1 flag used to ingest all the rows

lyrics_results = dbSendQuery(con, "SELECT DISTINCT * FROM musicoset.lyrics;")
df_lyrics = data.table(fetch(lyrics_results, n = -1)) # -1 flag used to ingest all the rows

# Deleting the unused variables to save memory
rm(con, artists_results, songs_results, artists_pop_results, song_pop_results,
   artists_chart_results, song_chart_results, df_ac_results, lyrics_results)

# Garbage collection
gc()

# Saving RData for decrease the data loading time
tables_to_save <- grep("df", ls(), value = TRUE)
save(list = tables_to_save, file = paste0('../3_Outputs/RData/Loaded_Music_Data.RData'))