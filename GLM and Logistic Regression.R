#Loading libraries
library(dplyr)
library(ggplot2)
library(readr)
library(corrplot)
library(ISLR)
library(caret)
library(pROC)
library(vtable)
summary(College)

#EDA
head(College)      # View the first few rows
summary(College)   # Summary statistics
str(College)       # Structure of the dataset
dim(College)       # Dimensions of the dataset (rows, columns)

# Check for missing values
sum(is.na(College))

data(College)
sumtable(College)
vartable <- vtable(College,out='viewer')

sumtable(College,
         summ=c('notNA(x)',
                'mean(x)',
                'median(x)',
                'propNA(x)'))

st(College, col.breaks = 4,
   summ = list(
     c('notNA(x)','mean(x)','sd(x^2)','min(x)','max(x)'),
     c('notNA(x)','mean(x)')
   ),
   summ.names = list(
     c('N','Mean','SD of X^2','Min','Max'),
     c('Count','Percent')
   ))

st(College, digits = 5)

# Display the structure of the dataset
str(College)

# Plotting histograms of numeric variables
par(mfrow = c(2, 2))
hist(College$Apps, main = "Distribution of Applications")
hist(College$Enroll, main = "Distribution of Enrollments")
hist(College$Outstate, main = "Distribution of Outstate Tuition")
hist(College$Grad.Rate, main = "Distribution of Graduation Rate")

# Plotting boxplot of Outstate Tuition by Private/Public status
boxplot(Outstate ~ Private, data = College, col = c("skyblue", "lightgreen"),
        main = "Outstate Tuition by Private/Public University")

# Set seed for reproducibility
set.seed(123)

# Create train/test split (70% train, 30% test)
train_index <- sample(1:nrow(College), 0.7*nrow(College))
train_data <- College[train_index, ]
test_data <- College[-train_index, ]

# Fit logistic regression model using glm()
logistic_model <- glm(Private ~ Apps + Outstate, data = train_data, family = "binomial")

options(scipen = 10)
# Summary of the model
summary(logistic_model)

# Predict on training set
train_predictions <- predict(logistic_model, newdata = train_data, type = "response")

# Convert predicted probabilities to binary predictions (0 or 1)
train_pred_class <- ifelse(train_predictions > 0.5, 1, 0)

# Create confusion matrix
conf_matrix_train <- table(train_pred_class, train_data$Private)
conf_matrix_train

# Compute accuracy, precision, recall, and specificity
accuracy_train <- mean(train_pred_class == train_data$Private)
precision_train <- conf_matrix_train[2,2] / sum(train_pred_class == 1)
recall_train <- conf_matrix_train[2,2] / sum(train_data$Private == 1)
specificity_train <- conf_matrix_train[1,1] / sum(train_data$Private == 0)

# Print metrics
cat("Accuracy (Train):", accuracy_train, "\n")
cat("Precision (Train):", precision_train, "\n")
cat("Recall (Train):", recall_train, "\n")
cat("Specificity (Train):", specificity_train, "\n")

# Predict on test set
test_predictions <- predict(logistic_model, newdata = test_data, type = "response")

# Convert predicted probabilities to binary predictions (0 or 1)
test_pred_class <- ifelse(test_predictions > 0.5, 1, 0)

# Create confusion matrix
conf_matrix_test <- table(test_pred_class, test_data$Private)
conf_matrix_test

# Compute accuracy, precision, recall, and specificity for test set
accuracy_test <- mean(test_pred_class == test_data$Private)
precision_test <- conf_matrix_test[2,2] / sum(test_pred_class == 1)
recall_test <- conf_matrix_test[2,2] / sum(test_data$Private == 1)
specificity_test <- conf_matrix_test[1,1] / sum(test_data$Private == 0)

# Print metrics
cat("Accuracy (Test):", accuracy_test, "\n")
cat("Precision (Test):", precision_test, "\n")
cat("Recall (Test):", recall_test, "\n")
cat("Specificity (Test):", specificity_test, "\n")

# Compute ROC curve
roc_curve <- roc(test_data$Private, test_predictions)

# Plot ROC curve
plot(roc_curve, main = "ROC Curve for Logistic Regression Model")
lines(x = c(0, 1), y = c(0, 1), col = "gray", lty = 2)
legend("bottomright", legend = paste("AUC =", round(auc(roc_curve), 2)), bty = "n")


