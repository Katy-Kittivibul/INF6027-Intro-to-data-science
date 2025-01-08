# INF6027: Introduction to Data Science Semester 1/2024

# Update your R packages
R.version.string
update.packages(ask = FALSE, checkBuilt = TRUE)

install.packages("tidyverse")
install.packages("GGally")
install.packages("ggplot2")
install.packages("hrbrthemes")

library(corrplot)
library(dplyr)
library(ggcorrplot)
library(GGally)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(RColorBrewer)
library(scales)
library(stringr)
library(tidyr)
library(viridis)

# All song
library(readxl)
song_description <- read_excel("song_description.xlsx")
View(song_description)

################################################################################
# Filter song year 2008-2018
#song_filtered <- song_description %>%
#  filter(release_date %in% 2008:2018)

start_date <- as.Date("2008-01-01")
end_date <- as.Date("2018-12-31")

song_filtered <- song_description[song_description$release_date >= start_date & 
                                    song_description$release_date <= end_date,]

# Check Duplication song
song_edited <- song_filtered[!duplicated(song_filtered$tracks.song_id),]

# Check data structure
str(song_edited)

# Check missing values
colSums(is.na(song_edited))

# Remove rows with missing values
song_cleaned <- na.omit(song_edited)

################################################################################

# Exploratory Data Analysis

# Data summary
summary_data <- summary(song_cleaned)
summary_table <- as.data.frame(summary_data)
print(summary_table)

# Select features
feature_pop <- names(song_cleaned)[c(6, 7, 16, 34:36)]
feature_song <- names(song_cleaned)[c(19:31)]

# Histogram
hist_song <- song_cleaned %>% 
  select(all_of(feature_song)) %>%
  pivot_longer(cols = feature_song) 

hist_song %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~name, ncol = 5, scales = 'free') +
  labs(title = "Acoustic Patterns Histogram",
       x = '', 
       y = '',
       caption = 'MusicOset dataset') +
  theme(axis.text.y = element_blank())

hist_pop <- song_cleaned %>% 
  select(all_of(feature_pop)) %>%
  pivot_longer(cols = feature_pop) 

hist_pop %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~name, ncol = 3, scales = 'free') +
  labs(title = "Popularity Pattern Frequency Plots",
       x = '', 
       y = '') +
  theme(axis.text.y = element_blank())

# Outlier
outlier_song <-  song_cleaned %>% 
  select(all_of(feature_song)) %>%
  pivot_longer(cols = feature_song)

outlier_song %>% 
  ggplot(aes(x = value)) +
  geom_boxplot() +
  facet_wrap(~name, nrow = 3, scales = "free") +
  labs(title = "Outlier analysis",
       subtitle = "For different song attributes",
       caption = "MusicOset dataset") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1),
        axis.text.y = element_blank()) 

# Correlation
ggpairs(song_cleaned[feature_song])

ggcorr(song_cleaned[feature_song], label = TRUE)

ggcorr(song_cleaned[, c("acoustic_features.duration_ms", 
                                      "acoustic_features.key", "acoustic_features.mode", 
                                      "acoustic_features.time_signature", "acoustic_features.acousticness", 
                                      "acoustic_features.danceability", "acoustic_features.energy", 
                                      "acoustic_features.instrumentalness", "acoustic_features.liveness", 
                                      "acoustic_features.loudness", "acoustic_features.speechiness", 
                                      "acoustic_features.valence", "acoustic_features.tempo")],
       label = TRUE,
       label_size = 3,
       hjust = 0.9,
       layout.exp = 5
       )


################################################################################
install.packages("caret")
install.packages("randomForest")
install.packages("xgboost")
install.packages("pROC")
install.packages("Metrics")

library(caret)
library(randomForest)
library(xgboost)
library(pROC)
library(Metrics)

# Data pre-processing
set.seed(42)

# Predict popular and unpopular song

# Select features and target variable
sel_data <- song_cleaned %>%
  select(
    song_pop.is_pop,  
    acoustic_features.duration_ms, 
    acoustic_features.key, 
    acoustic_features.mode,
    acoustic_features.time_signature, 
    acoustic_features.acousticness, 
    acoustic_features.danceability,
    acoustic_features.energy, 
    acoustic_features.instrumentalness, 
    acoustic_features.liveness,
    acoustic_features.loudness,
    acoustic_features.speechiness,
    acoustic_features.valence,
    acoustic_features.tempo
    )

# Convert target variable to binary factor
sel_data$song_pop.is_pop <- as.factor(ifelse(sel_data$song_pop.is_pop == TRUE, 1, 0))

# Split data into training and testing sets
train_index <- createDataPartition(sel_data$song_pop.is_pop, p = 0.8, list = FALSE)
train_data <- sel_data[train_index, ]
test_data <- sel_data[-train_index, ]

################################################################################

# Normalize numerical features
preprocess <- preProcess(train_data[, -1], method = c("center", "scale"))

train_data_normalized <- train_data
train_data_normalized[, -1] <- predict(preprocess, train_data[, -1])

test_data_normalized <- test_data
test_data_normalized[, -1] <- predict(preprocess, test_data[, -1])

################################################################################

# Classification
# Classify songs into popularity categories (e.g., Popular and Unpopular).

# 1. Logistic Regression
logistic_model <- glm(song_pop.is_pop ~ ., 
                      data = train_data_normalized, 
                      family = binomial)

# Predictions and Evaluation for Logistic Regression
logistic_preds <- predict(logistic_model, test_data_normalized, type = "response")
logistic_class <- ifelse(logistic_preds > 0.5, 1, 0)
logistic_accuracy <- mean(logistic_class == test_data_normalized$song_pop.is_pop)
cat("Logistic Regression Accuracy:", logistic_accuracy, "\n")

# Logistic regression accuracy = 0.578

logistic_conf_matrix <- confusionMatrix(as.factor(logistic_class), 
                                        as.factor(test_data_normalized$song_pop.is_pop))
cat("Logistic Regression Metrics:\n")
print(logistic_conf_matrix)


# 2. Random Forest
rf_model <- randomForest(song_pop.is_pop ~ ., 
                         data = train_data_normalized, 
                         ntree = 500, mtry = 3)

# Predictions and Evaluation for Random Forest
rf_preds <- predict(rf_model, test_data_normalized, type = "response")
rf_accuracy <- mean(rf_preds == test_data_normalized$song_pop.is_pop)
cat("Random Forest Accuracy:", rf_accuracy, "\n")

# Random forest accuracy = 0.561

rf_conf_matrix <- confusionMatrix(as.factor(rf_preds), 
                                  as.factor(test_data_normalized$song_pop.is_pop))
cat("Random Forest Metrics:\n")
print(rf_conf_matrix)


# 3. Gradient Boosting

# Convert data to matrix for XGBoost
train_matrix <- xgb.DMatrix(data = as.matrix(train_data_normalized[, -1]), 
                            label = as.numeric(train_data_normalized$song_pop.is_pop) - 1)
test_matrix <- xgb.DMatrix(data = as.matrix(test_data_normalized[, -1]), 
                           label = as.numeric(test_data_normalized$song_pop.is_pop) - 1)

# Train XGBoost Model
xgb_model <- xgboost(data = train_matrix, 
                     max_depth = 3, 
                     eta = 0.1, 
                     nrounds = 100, 
                     objective = "binary:logistic", 
                     verbose = 0)

# Predictions and Evaluation for XGBoost
xgb_preds <- predict(xgb_model, test_matrix)
xgb_class <- ifelse(xgb_preds > 0.5, 1, 0)
xgb_accuracy <- mean(xgb_class == as.numeric(test_data_normalized$song_pop.is_pop) - 1)
cat("XGBoost Accuracy:", xgb_accuracy, "\n")

# XGBoost Accuracy: 0.552

xgb_conf_matrix <- confusionMatrix(as.factor(xgb_class), 
                                   as.factor(test_data_normalized$song_pop.is_pop))
cat("XGBoost Metrics:\n")
print(xgb_conf_matrix)


# Extract Precision, Recall, F1-Score Manually (optional)
get_metrics <- function(conf_matrix) {
  precision <- conf_matrix$byClass["Precision"]
  recall <- conf_matrix$byClass["Recall"]
  f1_score <- 2 * (precision * recall) / (precision + recall)
  return(c(Precision = precision, Recall = recall, F1_Score = f1_score))
}

cat("Logistic Regression Metrics (Precision, Recall, F1-Score):\n", get_metrics(logistic_conf_matrix), "\n")
cat("Random Forest Metrics (Precision, Recall, F1-Score):\n", get_metrics(rf_conf_matrix), "\n")
cat("XGBoost Metrics (Precision, Recall, F1-Score):\n", get_metrics(xgb_conf_matrix), "\n")


# ROC Curves (Optional)
roc_logistic <- roc(as.numeric(test_data_normalized$song_pop.is_pop) - 1, logistic_preds)
roc_rf <- roc(as.numeric(test_data_normalized$song_pop.is_pop) - 1, as.numeric(rf_preds) - 1)
roc_xgb <- roc(as.numeric(test_data_normalized$song_pop.is_pop) - 1, xgb_preds)

plot(roc_logistic, col = "blue", main = "ROC Curves")
plot(roc_rf, add = TRUE, col = "green")
plot(roc_xgb, add = TRUE, col = "red")
legend("bottomright", 
       legend = c("Logistic", "Random Forest", "XGBoost"), 
       col = c("blue", "green", "red"), lwd = 2)

################################################################################

# Prediction 
# predict a continuous popularity score.

# Select features and target variable
sel_data_2 <- song_cleaned %>%
  select(
    songs.popularity,  
    acoustic_features.duration_ms, 
    acoustic_features.key, 
    acoustic_features.mode,
    acoustic_features.time_signature, 
    acoustic_features.acousticness, 
    acoustic_features.danceability,
    acoustic_features.energy, 
    acoustic_features.instrumentalness, 
    acoustic_features.liveness,
    acoustic_features.loudness,
    acoustic_features.speechiness,
    acoustic_features.valence,
    acoustic_features.tempo
  )

# Split data into training and testing sets
train_index_2 <- createDataPartition(sel_data_2$songs.popularity, p = 0.8, list = FALSE)
train_data_2 <- sel_data_2[train_index_2, ]
test_data_2 <- sel_data_2[-train_index_2, ]

# Normalize numerical features
preprocess_2 <- preProcess(train_data_2[, -1], method = c("center", "scale"))

train_data_normalized_2 <- train_data_2
train_data_normalized_2[, -1] <- predict(preprocess_2, train_data_2[, -1])

test_data_normalized_2 <- test_data_2
test_data_normalized_2[, -1] <- predict(preprocess_2, test_data_2[, -1])


# 1. Linear Regression
linear_model <- lm(songs.popularity ~ ., 
                   data = train_data_normalized_2)
summary(linear_model)

# Predictions and Evaluation for Linear Regression
linear_preds <- predict(linear_model, test_data_normalized_2)
linear_rmse <- rmse(test_data_normalized_2$songs.popularity, linear_preds)
cat("Linear Regression RMSE:", linear_rmse, "\n")

# Linear Regression RMSE: 14.92074


# 2. Random Forest Regression
rf_reg_model <- randomForest(songs.popularity ~ ., 
                             data = train_data_normalized_2, 
                             ntree = 500, mtry = 3)
summary(rf_reg_model)

# Predictions and Evaluation for Random Forest
rf_reg_preds <- predict(rf_reg_model, test_data_normalized_2)
rf_reg_rmse <- rmse(test_data_normalized_2$songs.popularity, rf_reg_preds)
cat("Random Forest RMSE:", rf_reg_rmse, "\n")

# Random Forest RMSE: 14.65087


# 3. Gradient Boosting Regression
# Convert data to matrix for XGBoost
train_reg_matrix <- xgb.DMatrix(data = as.matrix(train_data_normalized_2[, -1]), 
                            label = train_data_normalized_2$songs.popularity)
test_reg_matrix <- xgb.DMatrix(data = as.matrix(test_data_normalized_2[, -1]), 
                           label = test_data_normalized_2$songs.popularity)

# Train XGBoost Model
xgb_reg_model <- xgboost(data = train_reg_matrix, 
                         max_depth = 3, 
                         eta = 0.1, 
                         nrounds = 100, 
                         objective = "reg:squarederror", 
                         verbose = 0)

# Predictions and Evaluation for XGBoost
xgb_reg_preds <- predict(xgb_reg_model, test_reg_matrix)
xgb_reg_rmse <- rmse(test_data_normalized_2$songs.popularity, xgb_reg_preds)
cat("XGBoost RMSE:", xgb_reg_rmse, "\n")

# XGBoost RMSE: 14.47437


#Visualize Predictions
results <- data.frame(
  Actual = test_data_normalized_2$songs.popularity,
  Linear_Regression = linear_preds,
  Random_Forest = rf_reg_preds,
  XGBoost = xgb_reg_preds
)

# Scatter plot of actual vs predicted values
results_long <- results %>%
  pivot_longer(-Actual, names_to = "Model", values_to = "Predicted")

ggplot(results_long, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Predicted vs Actual Song Popularity", 
       x = "Actual Popularity", 
       y = "Predicted Popularity") +
  theme_bw()

# Function to calculate R-squared
calculate_r_squared <- function(actual, predicted) {
  ss_residual <- sum((actual - predicted)^2)
  ss_total <- sum((actual - mean(actual))^2)
  r_squared <- 1 - (ss_residual / ss_total)
  return(r_squared)
}

# Calculate R-squared for Linear Regression
linear_r_squared <- calculate_r_squared(test_data_normalized_2$songs.popularity, linear_preds)
cat("Linear Regression R-squared:", linear_r_squared, "\n")

# Linear Regression R-squared: 0.2340067 

# Calculate R-squared for Random Forest
rf_r_squared <- calculate_r_squared(test_data_normalized_2$songs.popularity, rf_reg_preds)
cat("Random Forest R-squared:", rf_r_squared, "\n")

# Random Forest R-squared: 0.261465

# Calculate R-squared for XGBoost
xgb_r_squared <- calculate_r_squared(test_data_normalized_2$songs.popularity, xgb_reg_preds)
cat("XGBoost R-squared:", xgb_r_squared, "\n")

# XGBoost R-squared: 0.2791526