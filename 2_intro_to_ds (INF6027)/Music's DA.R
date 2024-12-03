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

# test code
# con = dbConnect(SQLite(), '../1_datasets/MusicOSet/musicoset.db')

# Read and execute the SQL script
# sql_script <- readLines("../1_datasets/MusicOSet/musicoset.sql")
# dbExecute(con, paste(sql_script, collapse = "\n"))

con = dbConnect(RMySQL::MySQL(),
                host='localhost',
                port=3306,
                user='root',
                password='qwe123@A@A')

dbSendQuery(con, "SET GLOBAL local_infile = true;")
dbSendQuery(con, "CREATE DATABASE IF NOT EXISTS musicoset;")

result = dbSendQuery(con, "SELECT * FROM musicoset.acoustic_features")
df_acoustic_features = data.table(fetch(result, n = -1)) #n = -1 for all the rows

result = dbSendQuery(con, "SELECT * FROM musicoset.song_chart")
df_song_chart = data.table(fetch(result, n = -1)) #n = -1 for all the rows


list.files(path = "../1_datasets/MusicOSet/musicoset_metadata/", pattern = "*.csv")

df <- 
  list.files(path = "../1_datasets/MusicOSet/musicoset_metadata/",
             pattern = "*.csv",
             full.names = TRUE) %>% 
  map_df(~fread(.))
df
