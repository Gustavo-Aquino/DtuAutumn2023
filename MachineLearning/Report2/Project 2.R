# Load necessary libraries
library(dplyr)
library(nnet)  # For ANN
library(caret)
library(caret)
library(neuralnet)

# Load the glass dataset (replace with the actual dataset file path)
data <- read.csv("glass.data")

# Add class names to the dataset
data$typeName[data$Type == 1] <- "Building, float"
data$typeName[data$Type == 2] <- "Building, non-float"
data$typeName[data$Type == 3] <- "Vehicle, float"
data$typeName[data$Type == 4] <- "Vehicle, non-float"
data$typeName[data$Type == 5] <- "Container"
data$typeName[data$Type == 6] <- "Tableware"
data$typeName[data$Type == 7] <- "Headlamp"

# Remove numeric class attribute as there is one missing type
data <- data %>%
  select(-Type)

# Extract attributes (features)
X <- data[, 2:10]
classLabels <- data[, 11]

# Check and record dimensions
(N <- nrow(X))
(M <- ncol(X))

# Extract numeric class assignments
y <- as.numeric(as.factor(classLabels)) - 1

# Implement two-level cross-validation
K1 <- 10  # Number of outer folds
K2 <- 10  # Number of inner folds

# Initialize variables to store results
results <- data.frame(H = numeric(0), Lambda = numeric(0), MeanAccuracy = numeric(0))

# Define a range of values for ANN's hidden units (h) and regularization parameter (lambda)
h_values <- c(1, 5, 10)  # Adjust as needed
lambda_values <- c(0.01, 0.1, 1)  # Adjust as needed

# Implement two-level cross-validation
set.seed(123)  # Set a seed for reproducibility
outer_folds <- createFolds(y, k = K1, list = TRUE)

for (h in h_values) {
  for (lambda in lambda_values) {
    inner_results <- c()  # Store inner loop results
    
    for (i in 1:K1) {
      outer_train_idx <- unlist(outer_folds[-i])
      outer_test_idx <- unlist(outer_folds[i])
      
      inner_folds <- createFolds(y[outer_train_idx], k = K2, list = TRUE)
      
      for (j in 1:K2) {
        inner_train_idx <- unlist(inner_folds[-j])
        inner_validation_idx <- unlist(inner_folds[j])
        
        X_outer_train <- X[outer_train_idx, ]
        X_outer_test <- X[outer_test_idx, ]
        X_inner_train <- X[outer_train_idx[inner_train_idx], ]
        X_inner_validation <- X[outer_train_idx[inner_validation_idx], ]
        
        y_outer_train <- y[outer_train_idx]
        y_outer_test <- y[outer_test_idx]
        y_inner_train <- y[outer_train_idx[inner_train_idx]]
        y_inner_validation <- y[outer_train_idx[inner_validation_idx]]
        
        # Train an ANN with h hidden units and lambda regularization
        ann_model <- nnet(y_inner_train ~ ., data = X_inner_train, size = h, linout = TRUE)
        
        # Evaluate the ANN model on the inner validation set
        ann_predictions <- as.integer(predict(ann_model, newdata = X_inner_validation) > 0.5)
        ann_accuracy <- sum(ann_predictions == y_inner_validation) / length(y_inner_validation)
        
        inner_results <- c(inner_results, ann_accuracy)
      }
      
      # Calculate the mean performance over inner validation folds for this model
      inner_mean_accuracy <- mean(inner_results)
    }
    
    # Calculate the mean performance over outer test folds for this model
    outer_mean_accuracy <- mean(inner_mean_accuracy)
    
    # Store this result in the results data frame
    results <- rbind(results, data.frame(H = h, Lambda = lambda, MeanAccuracy = outer_mean_accuracy))
  }
}

# Find the best ANN model based on the highest mean accuracy
best_ann_model <- results[which.max(results$MeanAccuracy), ]

# Display the results
cat("ANN Model Selection Results:\n")
print(results)

cat("Best ANN Model:\n")
print(best_ann_model)

# Baseline model: Linear regression with no features
baseline_predictions <- rep(mean(y), length(y))

# Evaluate the baseline model
baseline_accuracy <- sum(as.integer(baseline_predictions > 0.5) == y) / length(y)

cat("Baseline Model Accuracy (Linear Regression with No Features):\n")
print(baseline_accuracy)










# Initialize a data frame to store the results
table_results <- data.frame(OuterFold = numeric(0), ANN_hidden_units = numeric(0), ANN_lambda = numeric(0), ANN_E_test = numeric(0), LinearRegression_E_test = numeric(0), Baseline_E_test = numeric(0))

# Implement two-level cross-validation
set.seed(123)  # Set a seed for reproducibility
outer_folds <- createFolds(y, k = K1, list = TRUE)

for (i in 1:K1) {
  outer_train_idx <- unlist(outer_folds[-i])
  outer_test_idx <- unlist(outer_folds[i])
  
  # Inner loop for selecting optimal h and lambda
  best_h <- 0
  best_lambda <- 0
  best_ann_model <- NULL
  best_inner_mean_accuracy <- 0
  
  for (h in h_values) {
    for (lambda in lambda_values) {
      inner_results <- c()  # Store inner loop results
      
      # Inner folds for inner validation
      inner_folds <- createFolds(y[outer_train_idx], k = K2, list = TRUE)
      
      for (j in 1:K2) {
        inner_train_idx <- unlist(inner_folds[-j])
        inner_validation_idx <- unlist(inner_folds[j])
        
        X_inner_train <- X[outer_train_idx, ]
        X_inner_validation <- X[outer_train_idx[inner_validation_idx], ]
        
        y_inner_train <- y[outer_train_idx]
        y_inner_validation <- y[outer_train_idx[inner_validation_idx]]
        
        # Train an ANN with h hidden units and lambda regularization
        ann_model <- nnet(y_inner_train ~ ., data = X_inner_train, size = h, linout = TRUE)
        
        # Evaluate the ANN model on the inner validation set
        ann_predictions <- as.integer(predict(ann_model, newdata = X_inner_validation) > 0.5)
        ann_accuracy <- sum(ann_predictions == y_inner_validation) / length(y_inner_validation)
        
        inner_results <- c(inner_results, ann_accuracy)
      }
      
      # Calculate the mean performance over inner validation folds for this model
      inner_mean_accuracy <- mean(inner_results)
      
      # Check if this model has a higher mean accuracy than the best one so far
      if (inner_mean_accuracy > best_inner_mean_accuracy) {
        best_inner_mean_accuracy <- inner_mean_accuracy
        best_h <- h
        best_lambda <- lambda
        best_ann_model <- ann_model
      }
    }
  }
  
  # Calculate the mean performance over outer test folds for the best ANN model
  best_ann_predictions <- as.integer(predict(best_ann_model, newdata = X[outer_test_idx, ]) > 0.5)
  best_ann_E_test <- 1 - sum(best_ann_predictions == y[outer_test_idx]) / length(y[outer_test_idx])
  
  # Calculate the linear regression E_test
  linear_regression_model <- lm(y[outer_train_idx] ~ ., data = X[outer_train_idx, ])
  linear_regression_predictions <- as.integer(predict(linear_regression_model, newdata = X[outer_test_idx, ]) > 0.5)
  linear_regression_E_test <- 1 - sum(linear_regression_predictions == y[outer_test_idx]) / length(y[outer_test_idx])
  
  # Calculate the baseline E_test
  baseline_predictions <- rep(mean(y[outer_train_idx]), length(outer_test_idx))
  baseline_E_test <- 1 - sum(as.integer(baseline_predictions > 0.5) == y[outer_test_idx]) / length(y[outer_test_idx])
  
  # Add the results to the data frame
  table_results <- rbind(table_results, data.frame(OuterFold = i, ANN_hidden_units = best_h, ANN_lambda = best_lambda, ANN_E_test = best_ann_E_test, LinearRegression_E_test = linear_regression_E_test, Baseline_E_test = baseline_E_test))
}

# Display the table
print(table_results)































          