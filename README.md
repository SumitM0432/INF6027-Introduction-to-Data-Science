# INF6027 INTRODUCTION TO DATA SCIENCE
### Description
This repository contains the codebase for the INF6027 Introduction to Data Science Module report. It includes data preprocessing, feature engineering, model training, and evaluation scripts. The repository also generates the results, plots, and visualizations used in the report and provides the workflow for the analysis.

### File Structure


### Ingesting the dataset
1. On the terminal install MySQL.
2. Download the SQL Script from the [MusicOSet Dataset](https://marianaossilva.github.io/DSW2019/index.html#relational>) Website.
3. Make the Database 'musicoset' and use the same database.
4. Run the SQL Script.
   
![Screenshot 2025-01-04 at 8 44 08 PM](https://github.com/user-attachments/assets/72af55fc-2b9b-4315-a4f9-c8feada97bc1)

### Setting up the Configuration
##### Note: The configuration variable needs to be set in 1_Scripts/0_main_.R file
1. Change the username and password for the MySQL server to ingest the data.
2. The variable lyrical_switch can be changed based on the part of the report, TRUE uses lyrical features to train the models and FALSE neglects the lyrical features. </br>
*(DEFAULT = TRUE)*

### Running the Code.
1. Run the 0_main_.R Script. </br>
*NOTE: This script runs all the required scripts and produces the results*
