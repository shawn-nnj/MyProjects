cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100)

#How do demographic factors (age, gender, ethnicity) and neighborhood characteristics (housing type, proximity to commercial areas) affect the likelihood of a crime occurring? 
# Load required libraries
library(dplyr)   
library(tidyr)   
library(MASS)   
library(car)  
library(tidyr)
library(readr)
library(ggplot2)
library(tidyverse,pacman)
library(knitr)
library(kableExtra)
library(magrittr)
library(caret)
library(forcats)
library(glmnet)
library(logistf)

crime_data <- read_csv("Crime_Data_from_2020_to_Present.csv")

# Step 1: Clean and preprocess the data
cleaned_data <- crime_data %>%
  rename(Vict_Age = `Vict Age`,
         Vict_Sex = `Vict Sex`,
         Vict_Descent = `Vict Descent`,
         Premis_Desc = `Premis Desc`,
         Crm_Cd_Desc = `Crm Cd Desc`) %>%
  filter(Vict_Age >= 0, !is.na(Vict_Sex))  # Adjust filtering criteria as needed

# Check if there are any observations left after filtering
if (nrow(cleaned_data) == 0) {
  stop("No valid observations remain after data cleaning.")
}

# Step 2: Encode categorical variables and ensure target variable format
cleaned_data <- cleaned_data %>%
  mutate(Vict_Sex = factor(Vict_Sex, levels = c("M", "F", "X")),
         Vict_Descent = factor(Vict_Descent, levels = c("A", "B", "C", "D", "F", "G", "H", "I", "J", "K", "L", "O", "P", "S", "U", "V", "W", "X", "Z")),
         Premis_Desc = factor(Premis_Desc))

# Step 3: Sample 10,000 rows if possible
num_samples <- min(10000, nrow(cleaned_data))
sampled_data <- cleaned_data %>%
  sample_n(num_samples, replace = (num_samples > nrow(cleaned_data)))

# Check if enough samples were obtained
if (nrow(sampled_data) < 10000) {
  warning("Less than 10,000 rows were sampled due to insufficient data.")
}

# Step 4: Build Logistic Regression Model with regularization
library(glmnet)

# Convert factor variables to model matrix
model_matrix <- model.matrix(Crm_Cd_Desc ~ . - 1, data = sampled_data)

# Define target variable
target_variable <- as.numeric(sampled_data$Crm_Cd_Desc) - 1  # Convert 0/1 to 0/1

# Split data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(model_matrix), 0.8 * nrow(model_matrix))
train_x <- model_matrix[train_indices, ]
train_y <- target_variable[train_indices]
test_x <- model_matrix[-train_indices, ]
test_y <- target_variable[-train_indices]

# Fit logistic regression model with L1 regularization
logistic_model <- cv.glmnet(train_x, train_y, alpha = 1, family = "binomial")

# Step 5: Evaluate Model Performance
# Predict on test set
pred_probs <- predict(logistic_model, newx = test_x, s = "lambda.min", type = "response")
pred_class <- ifelse(pred_probs > 0.5, 1, 0)

# Calculate accuracy
accuracy <- mean(pred_class == test_y)

# Display model coefficients
coef(logistic_model, s = "lambda.min")

# Display model performance
cat("Model Accuracy (Test Set):", accuracy, "\n")