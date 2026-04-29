
############################################################
# LOSS COST MODEL WORKSHOP SCRIPT
# "Let's Build a Loss Cost Model Together!"
############################################################


############################################################
#R Basics
############################################################
# https://cran.r-project.org/doc/contrib/Short-refcard.pdf
# https://dplyr.tidyverse.org/

# Install Packages
install.packages("tidyverse")
install.packages("DescTools")
install.packages("corrplot")
install.packages("caret")
install.packages("tweedie")
install.packages("statmod")
install.packages("xgboost")
install.packages("Matrix")

# Load Data


# Inspect Data


# dplyr Exercises
library(tidyverse)

## Selecting columns
### Select named_insured_age, vehicle_age, and annual_miles


### Select all columns except named_insured_age, vehicle_age, and annual_miles


### Select all numeric columns


### Select all character columns



## Filter Data
### Filter vehicle_type to Sedan


### Filter vehicle_type to Sedans with less than 15,000 annual_miles


### Filter to records with vehicle_type as Sedan or annual_miles less than 15,000


### Filter to missing values of credit_score or annual_miles



## Sort Data
### Sort based on annual_miles


### Sort based on descending annual_miles


### Sort based on named_insured_age and descending vehicle_age



## Summarize Data
### What is the mean and median collision_loss_cost?


### What are the min and max annual_miles?


### What is the Mode of vehicle age and named insured age?
library(DescTools)



### What are the average annual_miles by territory?


## Creating/deleting columns
### Calculate severity


############################################################
# Exploratory Data Analysis (EDA)
############################################################
#https://rstudio.github.io/cheatsheets/html/data-visualization.html

# Histogram

# Barchart

# Scatterplot

# Boxplot


# Correlation Plot
library(corrplot)



############################################################
# Data Modification
############################################################
# Check for duplicate records


# Check Factor levels
## Convert strings to factors


## Review exposure by level


## Adjust base level of the categorical variable


# Handle NA values
## Remove all records with NA


## Impute values


############################################################
# Train/Test Split
############################################################
library(caret)
set.seed(123)

# Remove columns


# Create train/test index


############################################################
# GLM
############################################################
# Severity
## Gamma


## Residual Plots


## Variable Transformations - Train
### Territory Group


## Variable Transformations - Test
### Territory Group


## Predictions


# Frequency
## Poisson


## Residual Plots


## Variable Transformations - Train
### Vehicle Group

### UW Group

## Variable Transformations - Test
### Vehicle Group

### UW Group


## Predictions


# Tweedie
library(tweedie)
library(statmod)

## Power Parameter


## Tweedie Model


## Residual Plots


## Predictions


############################################################
# GBM
############################################################
library(xgboost)
library(Matrix)



# Variable Importance


############################################################
# Model Comparison
############################################################
# RMSE
## Calculate Frequency-Severity predictions


## Calculate RMSE


# Double Lift Chart
