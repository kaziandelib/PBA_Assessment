# Customer Response Prediction using Machine Learning

## Overview

This project focuses on predicting customer responses to a marketing campaign using machine learning techniques. The Superstore dataset is preprocessed, analysed, and used to train classification models that determine whether a customer will accept or decline an offer.

The workflow includes data cleaning, feature engineering, handling class imbalance, feature selection, and model evaluation using multiple algorithms.

---

## Dataset

* Source: Superstore dataset (superstore_data.csv)
* Contains customer demographics, purchasing behaviour, and campaign response
* Target variable:

  * Response → Binary classification (Accepted / Declined)

---

## Project Pipeline

### Data Preprocessing

* Removed irrelevant columns (e.g., Id)
* Handled missing values (e.g., dropped rows with missing Income)
* Removed invalid entries:

  * Unrealistic birth years (e.g., 1893, 1899, 1900)
  * Invalid marital statuses (e.g., YOLO, Absurd)
* Converted dataset into tibble format

### Feature Engineering

* One-hot encoding for:

  * Education
  * Marital_Status
* Converted Dt_Customer into date format
* Removed outliers using the IQR method

### Exploratory Data Analysis (EDA)

* Histograms and boxplots for numerical variables
* Bar plots for categorical variables
* Analysis of relationships with the target variable (Response)

---

## Handling Class Imbalance

* Applied upsampling using caret
* Balanced the dataset to improve model performance

---

## Feature Selection

* Used Random Forest feature importance
* Selected features based on importance thresholds:

  * Threshold ~40 (broader feature set)
  * Threshold ~90 (reduced feature set)

---

## Models Implemented

### Random Forest

* Hyperparameter tuning using tuneRF
* Compared:

  * With vs without scaling
  * Different feature thresholds

### K-Nearest Neighbors (KNN)

* Tuned k using cross-validation (10-fold CV with 3 repeats)
* Compared:

  * Scaled vs unscaled data
  * Different feature subsets

---

## Model Evaluation

* Confusion matrices
* Visual evaluation using ConfusionTableR
* Metrics observed:

  * Accuracy
  * Classification performance (Accepted vs Declined)

---

## Experiments Conducted

* Feature selection thresholds (40 vs 90)
* Scaling vs no scaling
* Outlier removal impact
* Model comparison (Random Forest vs KNN)

---

## Libraries Used

* Data manipulation: dplyr, tidyr, reshape2
* Visualisation: ggplot2, corrplot
* Machine Learning: caret, randomForest, caTools
* Preprocessing: mice, lubridate
* Evaluation: ConfusionTableR

---

## How to Run

1. Clone the repository

2. Open the R script in RStudio

3. Install required packages
   install.packages(c("tidyverse", "caret", "randomForest", "ggplot2", "mice", "lubridate", "ConfusionTableR"))

4. Run the script step-by-step or all at once

---

## Key Insights

* In-Store Customers are more likely to opt in for the membership
* Customers with children at home tend to buy more alcohol

---


---
