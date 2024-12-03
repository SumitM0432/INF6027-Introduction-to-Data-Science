# Setting the Current Path
if (interactive()) {
  # If in RStudio, use rstudioapi
  if ("rstudioapi" %in% rownames(installed.packages())) {
    setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  }
}

source("../0_config/0_libraries.R", local = TRUE)
source("../0_config/0_functions.R", local = TRUE)

df_names = fread("../1_datasets/IMDB Dataset/name.basics.tsv", quote = "")
df_akas = fread("../1_datasets/IMDB Dataset/title.akas.tsv", quote = "")
df_basics = fread("../1_datasets/IMDB Dataset/title.basics.tsv", quote = "")
df_crew = fread("../1_datasets/IMDB Dataset/title.crew.tsv", quote = "")
df_episodes = fread("../1_datasets/IMDB Dataset/title.episode.tsv", quote = "")
# df_principal = fread("../1_datasets/IMDB Dataset/title.principals.tsv", quote = "")
df_ratings = fread("../1_datasets/IMDB Dataset/title.ratings.tsv", quote = "")











