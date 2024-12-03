# Setting the Current Path
if (interactive()) {
  # If in RStudio, use rstudioapi
  if ("rstudioapi" %in% rownames(installed.packages())) {
    setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  }
}

source("../0_config/0_libraries.R", local = TRUE)
source("../0_config/0_functions.R", local = TRUE)

con = dbConnect(RMySQL::MySQL(),
                            dbname='musicoset',
                            host='localhost',
                            port=3306,
                            user='root',
                            password='qwe123@A@A')

dbSendQuery(con, "SET GLOBAL local_infile = true;")

result = dbSendQuery(con, "select * from musicoset.acoustic_features")
acoustic_features = data.table(fetch(result, n = -1)) #n = -1 for all the rows

list.files(path = "../1_datasets/MusicOSet/musicoset_metadata/", pattern = "*.csv")

df <- 
  list.files(path = "../1_datasets/MusicOSet/musicoset_metadata/",
             pattern = "*.csv",
             full.names = TRUE) %>% 
  map_df(~fread(.))
df
