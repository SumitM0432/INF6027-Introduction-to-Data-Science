# Setting the Current Path
if (interactive()) {
  # If in RStudio, use rstudioapi
  if ("rstudioapi" %in% rownames(installed.packages())) {
    setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  }
}

# VARIABLES TO SET BASED ON WHAT ANALYSIS IS BEING DONE
# TRUE Means EDA will run (Note: it doesn't effect the prediction and model training so should be run independently to reduce run time)
exploratory_switch = FALSE

# TRUE mean lyrical features will be generated and would be used for training except vice-versa
lyrical_switch = TRUE

if (lyrical_switch == TRUE) {
  path_for_results = '/Plots/Results/With Lyrics'
} else {
  path_for_results = '/Plots/Results/Without Lyrics'
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

if (exploratory_switch == TRUE) {
  print(paste(Sys.time(), ' :: EXPLORATORY DATA ANALYSIS'))
  # Script that includes all the EDA and Understanding (Note: This doesn't effect any other script)
  source("1_exploratory_data_analysis.R", local = TRUE)
} else {
  print(paste(Sys.time(), ' :: SKIPPING EXPLORATORY DATA ANALYSIS'))
}

print(paste(Sys.time(), ' :: DATA PREPROCESSING'))
# Running the Preprocessing and Features Engineering Script
source("2_data_preprocessing.R")

print(paste(Sys.time(), ' :: MODEL DEFINITIONS'))
# Defining the models
source("3_model_definition.R")

print(paste(Sys.time(), ' :: MODEL TRAINING AND EVALUATION'))
# Training the machine learning model 
source("4_training_evaluation.R")