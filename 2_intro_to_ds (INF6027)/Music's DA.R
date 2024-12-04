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

# Loading the Data
source("music_data_loading.R", local = TRUE)

# Loading the RData
load("RData/Loaded_MusicData.RData")

# 20406 -> 20405 # No NULLS in df_chart_songs and df_pop_songs
df_meta_songs = df_meta_songs %>%
  # Dropping the null values
  drop_na()

# No NULLS in df_pop_artists, df_chart_artists and df_meta_artists
# No NULLS in df_acoustic_features
# 744 NULLS in lyrics

# Getting the Artists IDs
df_meta_songs = df_meta_songs %>%
  mutate(
    artist_id_vectors := mapply(extract_artist, artists)
  )

check_coverage = function(df1, df2, on_col) {
  
  # Selecting the only the ids for data frame 1 (on which we are gonna do the check)
  df1 = df1 %>%
    select(all_of(on_col)) %>%
    distinct()
  
  # Temp flag for the data frame 2 with whom we are gonna check
  df2 = df2 %>%
    mutate(temp_flag = 1) %>%
    select(all_of(c(on_col, 'temp_flag')))
  
  # ids not present count
  not_present = left_join(df1, df2, by = c(on_col)) %>%
    filter(is.na(temp_flag)) %>%
    select(all_of(on_col))
  
  print (paste0("ids not present in the second tables :: count :: ", nrow(not_present)))
  return (not_present)
  
}





