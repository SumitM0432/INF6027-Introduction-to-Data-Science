# INF6027 INTRODUCTION TO DATA SCIENCE
### Description
This repository contains the codebase for the INF6027 Introduction to Data Science Module report. It includes data preprocessing, feature engineering, model training, and evaluation scripts. The repository also generates the results, plots, and visualizations used in the report and provides the workflow for the analysis.

### File Structure
```
├── 0_Config
│    ├── 0_functions.R
│    ├── 0_libraries.R
│    └── 0_musicoset_data_loading.R
├── 1_Scripts
│    ├── 0_main_.R
│    ├── 1_exploratory_data_analysis.R
│    ├── 2_1_data_preprocessing.R
│    ├── 2_2_exploratory_data_analysis_lyrical.R
│    ├── 3_model_definition.R
│    └── 4_training_evaluation.R
├── 2_Outputs
│    ├── Plots
│    │    ├── EDA
│    │    └── Results
│    │        ├── With Lyrics
│    │        └── Without Lyrics
│    ├── RData
│    └── Trained_Models
│        ├── With Lyrics
└──      └── Without Lyrics
```
##### Configuration Scripts (0_Config/):
```
0_functions.R - Script consisting of all the functions used in the main scripts

0_libraries.R - Script to install the Required libraries

0_musicoset_data_loading.R - Loading the data from the SQL server
```

##### Main Scripts (1_Scripts/):
```
0_main_.R - Main Script to run all the required scripts in order

1_exploratory_data_analysis.R - Script for Exploratory Data Analysis

2_1_data_preprocessing.R - Script to do the data preprocessing and feature engineering

2_2_exploratory_data_analysis_lyrical.R - Script for Exploratory Data Analysis for lyrical features

3_model_definition.R - Script to define the Machine Learning Model

4_training_evaluation.R - Script For training the model and evaluation
```

##### Output Folders (2_Outputs/):
```
Plots Folder: Plots saved from EDA and training and evaluation of the model

RData Folder: Saves the RData generated from running the scripts

Trained_Models Folder: Saves the Trained Models for later usage
```
## Instructions for Running the Code
### 1. Ingesting the dataset
1. On the terminal install MySQL.
2. Download the SQL Script from the [MusicOSet Dataset](https://marianaossilva.github.io/DSW2019/index.html#relational>) Website.
3. Make the Database 'musicoset' and use the same database.
4. Run the SQL Script.
   
![Screenshot 2025-01-04 at 8 44 08 PM](https://github.com/user-attachments/assets/72af55fc-2b9b-4315-a4f9-c8feada97bc1)

### 2. Setting up the Configuration
##### Note: The configuration variable needs to be set in 1_Scripts/0_main_.R file
1. Change the username and password for the MySQL server to ingest the data.
2. The variable lyrical_switch can be changed based on the part of the report, TRUE uses lyrical features to train the models and FALSE neglects the lyrical features. *(DEFAULT = TRUE)*

### 3. Running the Code (Scripts)
1. Run the ```0_main_.R``` Script.
*NOTE: This script runs all the required scripts and produces the results*
