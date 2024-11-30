# Setting the Current Path
if (interactive()) {
  # If in RStudio, use rstudioapi
  if ("rstudioapi" %in% rownames(installed.packages())) {
    setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  }
}

source("0_libraries.R", local = TRUE)
source("0_functions.R", local = TRUE)

df_ratings = fread("Data/IMDB Dataset/title.ratings.tsv")
df_names = fread("Data/IMDB Dataset/name.basics.tsv", quote = "")
df_principal = fread("Data/IMDB Dataset/title.principals.tsv", quote = "")
