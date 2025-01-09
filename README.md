# Song Popularity Analysis and Prediction

## Overview
This project is a part of the INF6027: Introduction to Data Science module assignment at the Information School, University of Sheffield. The primary objective of this project is to analyse and predict the popularity of songs using the MusicOset dataset. The project also explores how different audio features influence a song's popularity, offering valuable insights for the music industry.

## Project Main Processes
1. Exploratory Data Analysis (EDA):
- Visualise song popularity distribution and audio feature correlations.
- Examine song features distribution and identify key patterns.
2. Predictive Modeling:
- Build classification models to predict whether a song is popular or not.
- Develop regression models to predict song popularity scores.
3. Machine Learning Methods:
- Logistic Regression, Random Forest, XGBoost for classification.
- Linear Regression, Random Forest Regression, XGBoost Regression for predicting scores.
4. Evaluation Metrics:
- Precision, Recall, F1-Score for classification models.
- RMSE and R-squared for regression models.

## Dataset
The MusicOset dataset (2008-2018) was used for this project. It includes:
- Audio Features: Danceability, energy, valence, tempo, loudness, etc.
- Metadata: Artist names, album details, and genre information.
- Target Variable: songs.popularity indicating the popularity of a song.

## Files
- song_popularity_analysis.R: Main script for data preprocessing, EDA, machine learning modelling, and visualisation.
- README.md: Documentation for the project (this file).

## Key Features of the Script
1. Data Preprocessing:
- Merge file using MS Excel.
- Normalisation of features.
- Splitting data into training and testing sets.
2. Exploratory Data Analysis:
- Correlation heatmap for audio features.
- Box plots for comparing feature distributions.
3. Modeling:
- Classification models: Logistic Regression, Random Forest, XGBoost.
- Regression models: Linear Regression, Random Forest Regression, XGBoost Regression.
4. Model validation:
- Actual vs. Predicted plots for regression.
- Feature importance for Random Forest and XGBoost models.

## Getting Started
1. Clone the repository:
bash
> git clone https://github.com/your-username/song-popularity-analysis.git
2. Open the R script (song_popularity_analysis.R) in RStudio.
3. Install required libraries:
R
> install.packages(c("tidyverse", "caret", "Metrics", "ggcorrplot", "ggridges", "viridis", "randomForest", "xgboost"))
4. Run the script step-by-step to reproduce the analysis and results.

## Results
- Classification models achieved high precision and recall for predicting popular songs.
- Regression models showed low performance in predicting popularity scores with RMSE and R-squared values.
- Model validation provided actionable insights into the characteristics of popular songs.

## Acknowledgments
This project was completed as part of the INF6027 Introduction to Data Science module at the Information School, University of Sheffield.

## Contact
For questions or feedback, feel free to contact:
- Email: kkittivibul1@sheffield.ac.uk
- GitHub: https://github.com/Katy-Kittivibul
- Linkedin: www.linkedin.com/in/kulisara-kittivibul
