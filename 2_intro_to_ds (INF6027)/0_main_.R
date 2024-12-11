# Setting the Current Path
if (interactive()) {
  # If in RStudio, use rstudioapi
  if ("rstudioapi" %in% rownames(installed.packages())) {
    setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  }
}

# Setting seed for reproducibility
set.seed(123)

print(paste(Sys.time(), ' :: INGESTING LIBRARIES AND FUNCTIONS'))
# Getting the libraries and the required functions
source("../0_config/0_libraries.R", local = TRUE)
source("../0_config/0_functions.R", local = TRUE)

print(paste(Sys.time(), ' :: DOWNLOADING THE DATA FROM THE DATABASE'))
# Loading the Data from the Script
source("../0_config/0_musicoset_data_loading.R", local = TRUE)

print(paste(Sys.time(), ' :: DATA PREPROCESSING'))
# Running the Preprocessing and Features Engineering Script
source("1_data_preprocessing.R")

print(paste(Sys.time(), ' :: MODEL DEFINITIONS'))
# Defining the models
source("2_model_definition.R")

print(paste(Sys.time(), ' :: MODEL TRAINING AND EVALUATION'))
# Training the machine learning model 
source("3_training_evaluation.R")