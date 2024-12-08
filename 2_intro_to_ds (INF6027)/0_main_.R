# Setting the Current Path
if (interactive()) {
  # If in RStudio, use rstudioapi
  if ("rstudioapi" %in% rownames(installed.packages())) {
    setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  }
}

# Setting seed for reproducibility
set.seed(123)

# Getting the libraries and the required functions
source("../0_config/0_libraries.R", local = TRUE)
source("../0_config/0_functions.R", local = TRUE)

# Loading the Data from the Script
source("../0_config/0_musicoset_data_loading.R", local = TRUE)

# Running the Preprocessing and Features Engineering Script
source("1_data_preprocessing.R")

# Training the machine learning model 
source("2_modelling.R")