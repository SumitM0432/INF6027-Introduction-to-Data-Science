# Data Ingestion
if (!requireNamespace("RMySQL")) install.packages("RMySQL")
library("RMySQL")

if (!requireNamespace("DBI")) install.packages("DBI")
library("DBI")

# Preprocessing and Plotting
if (!requireNamespace("data.table")) install.packages("data.table")
library("data.table")

if (!requireNamespace("tidyverse")) install.packages("tidyverse")
library("tidyverse")

if (!requireNamespace("tidytext")) install.packages("tidytext")
library("tidytext")

if (!requireNamespace("stopwords")) install.packages("stopwords")
library("stopwords")

if (!requireNamespace("syuzhet")) install.packages("syuzhet")
library("syuzhet")

# For Model Training, Prediction and Evaluation
if (!requireNamespace("tidymodels")) install.packages("tidymodels")
library("tidymodels")

if (!requireNamespace("caret")) install.packages("caret")
library("caret")

if (!requireNamespace("xgboost")) install.packages("xgboost")
library("xgboost")