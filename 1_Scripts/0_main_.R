# Setting the path to the current directory
if (interactive()) {
  # If in RStudio, use rstudioapi
  if ("rstudioapi" %in% rownames(installed.packages())) {
    setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  }
}

# Username and Password for SQL Server
username_sql = 'root'
password_sql = 'qwe123@A@A'

# Setting seed for reproducibility
set.seed(123)

# TRUE - RUN with Lyrical Features and FALSE - RUN without Lyrical Features
# Generate two sets of results based on the research questions in the report.
lyrical_switch = TRUE

print(paste(Sys.time(), ' :: INGESTING LIBRARIES AND FUNCTIONS'))
# Getting the libraries and the required functions
source("../0_Config/0_libraries.R", local = TRUE)
source("../0_Config/0_functions.R", local = TRUE)

# Checking for folder presence
folder_creation_check()

# Changing the path results based on lyrical features usage
if (lyrical_switch == TRUE) {
  path_for_results = '../2_Outputs/Plots/Results/With Lyrics'
} else {
  path_for_results = '../2_Outputs/Plots/Results/Without Lyrics'
}

print(paste(Sys.time(), ' :: DOWNLOADING THE DATA FROM THE DATABASE'))
# Loading the Data from the Script
source("../0_config/0_musicoset_data_loading.R", local = TRUE)

print(paste(Sys.time(), ' :: EXPLORATORY DATA ANALYSIS'))
source("1_exploratory_data_analysis.R", local = TRUE)

print(paste(Sys.time(), ' :: DATA PREPROCESSING'))
# Running the Preprocessing and Features Engineering Script
source("2_1_data_preprocessing.R")

if (lyrical_switch == TRUE) {
  print(paste(Sys.time(), ' :: EXPLORATORY DATA ANALYSIS FOR LYRICAL FEATURES'))
  # Running the EDA script after the lyrical features are engineered
  source("2_2_exploratory_data_analysis_lyrical.R")
} else {
  print(paste(Sys.time(), ' :: SKIPPING EXPLORATORY DATA ANALYSIS FOR LYRICAL FEATURES'))
}

print(paste(Sys.time(), ' :: MODEL DEFINITIONS'))
# Defining the models
source("3_model_definition.R")

print(paste(Sys.time(), ' :: MODEL TRAINING AND EVALUATION'))
# Training the machine learning model and evaluating them
source("4_training_evaluation.R")