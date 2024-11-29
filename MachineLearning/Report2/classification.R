# rm(list = ls()) # Clear work space
setwd("C:/Faculdade/Intercâmbio/Machine Learning/Report2")

# Load necessary libraries
library(dplyr)
library(nnet)
library(glmnet)
library(caret)
library(class)
library(stats)
library(FNN)
library(rpart.plot)


mcnemar <- function(y_true, yhatA, yhatB, alpha=0.05){
  nn <- matrix(, nrow = 2, ncol = 2)  
  c1 = yhatA - y_true == 0
  c2 = yhatB - y_true == 0  
  
  nn[1,1] = sum(c1 & c2)
  nn[1,2] = sum(c1 & !c2)
  nn[2,1] = sum(!c1 & c2)
  nn[2,2] = sum(!c1 & !c2)
  
  n12 = nn[1,2]
  n21 = nn[2,1]
  n <- sum(nn)
  Etheta = (n12-n21)/n
  Q = n**2 * (n+1) * (Etheta+1) * (1-Etheta) / ( (n*(n12+n21) - (n12-n21)**2) )
  p = (Etheta + 1)/2 * (Q-1)
  q = (1-Etheta)/2 * (Q-1)
  
  thetaL =  
    CI <- c( 2*qbeta( alpha/2, p, q)-1, 2*qbeta(1-alpha/2, p, q)-1)
  # thetahat <-  2*p/(p+q) - 1
  
  p <- 2*pbinom(min( n12, n21), size=n12+n21, prob=0.5  )
  
  
  # print(paste("Result of McNemars test using alpha=", alpha))
  
  # print("Comparison matrix n")
  # print(nn)
  # if(n12+n21 <= 10){
  #   print(paste("Warning, n12+n21 is low: n12+n21=",(n12+n21)))
  # }
  # print("Approximate 1-alpha confidence interval of theta: [thetaL,thetaU] = ")
  # print(CI)
  # print(paste( "p-value for two-sided test A and B have same accuracy (exact binomial test): p=", p) )
  
  rt = list("thetahat"=Etheta, "CI"=CI, "p"=p, "cm"=nn)
  return(rt)  
}


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

possible_k_neighbors <- c(1:5, 10:15, 20:25)
possible_minsplit <- c(10,20,40,60,80)

classifier_results <- tribble(~ct_multinon, ~knn_multinon, ~ct_baseline, ~knn_baseline, ~multinom_baseline)

table_results <- data.frame(OuterFold = numeric(0), CT_E_test = numeric(0), k_neighbors = numeric(0), KNN_E_test = numeric(0), MultinomRegression_E_test = numeric(0), Baseline_E_test = numeric(0))

# Implement two-level cross-validation
set.seed(123)  # Set a seed for reproducibility
outer_folds <- createFolds(y, k = K1, list = TRUE)
preProcValues <- preProcess(X, method = c("center", "scale"))

for (i in 1:K1) {
  outer_train_idx <- unlist(outer_folds[-i])
  outer_test_idx <- unlist(outer_folds[i])
  
  # Inner loop for selecting optimal h and lambda
  best_k <- 0
  best_inner_mean_accuracy <- 0
  
  for (k in possible_k_neighbors) {
    inner_results <- c()  # Store inner loop results
    
    # Inner folds for inner validation
    inner_folds <- createFolds(y[outer_train_idx], k = K2, list = TRUE)
    
    for (j in 1:K2) {
      inner_train_idx <- unlist(inner_folds[-j])
      inner_validation_idx <- unlist(inner_folds[j])
      
      # browser()
      
      X_inner_train <- predict(preProcValues, X[inner_train_idx, ])
      X_inner_validation <- predict(preProcValues, X[inner_validation_idx, ])
      
      
      y_inner_train <- y[inner_train_idx]
      y_inner_validation <- y[inner_validation_idx]
      
      # Train an KNN with k neighbors
      knn_model <- knn(train = X_inner_train, test = X_inner_validation, cl = y_inner_train, k = k, algorithm = "kd_tree")
      knn_accuracy <- sum(knn_model == y_inner_validation) / length(y_inner_validation)
      
      inner_results <- c(inner_results, knn_accuracy)
    }
    
    # Calculate the mean performance over inner validation folds for this model
    inner_mean_accuracy <- mean(inner_results)
    
    # Check if this model has a higher mean accuracy than the best one so far
    if (inner_mean_accuracy > best_inner_mean_accuracy) {
      best_inner_mean_accuracy <- inner_mean_accuracy
      best_k <- k
    }
    
  }
  
  best_min <- 0
  best_ct_model <- NULL
  best_inner_mean_accuracy <- 0
  
  for (minsplit in possible_minsplit) {
    inner_results <- c()  # Store inner loop results
    
    # Inner folds for inner validation
    inner_folds <- createFolds(y[outer_train_idx], k = K2, list = TRUE)
    
    for (j in 1:K2) {
      inner_train_idx <- unlist(inner_folds[-j])
      inner_validation_idx <- unlist(inner_folds[j])
      
      X_inner_train <- X[inner_train_idx, ]
      X_inner_validation <- X[inner_validation_idx, ]
      
      y_inner_train <- y[inner_train_idx]
      y_inner_validation <- y[inner_validation_idx]
      
      ct_model <- rpart(y_inner_train ~ ., data = X_inner_train,
                        control = rpart.control(minsplit = minsplit, minbucket = 5, cp = 0),
                        parms = list(split = "gini"), method = "class")
      ct_predictions <- predict(ct_model, X_inner_validation, type = "class")
      ct_accuracy <- sum(ct_predictions == y_inner_validation) / length(y_inner_validation)
      
      inner_results <- c(inner_results, ct_accuracy)
    }
    
    # Calculate the mean performance over inner validation folds for this model
    inner_mean_accuracy <- mean(inner_results)
    
    # Check if this model has a higher mean accuracy than the best one so far
    if (inner_mean_accuracy > best_inner_mean_accuracy) {
      best_inner_mean_accuracy <- inner_mean_accuracy
      best_minsplit <- minsplit
      best_ct_model <- ct_model
    }
    
  }
  # Calculate for the best KNN model
  best_knn_predictions <- knn(
    train = predict(preProcValues, X[outer_train_idx, ]),
    test = predict(preProcValues, X[outer_test_idx, ]),
    cl = y[outer_train_idx], k = best_k
    )
  best_knn_E_test <- sum(best_knn_predictions != y[outer_test_idx]) / length(y[outer_test_idx])
  
  best_ct_predictions <- predict(best_ct_model, X[outer_test_idx, ], type = "class")
  best_ct_E_test <- sum(best_ct_predictions != y[outer_test_idx]) / length(y[outer_test_idx])
  prp(best_ct_model)
  
  # Calculate the linear regression E_test
  multinom_regression_model <- multinom(y[outer_train_idx] ~ ., data = predict(preProcValues, X[outer_train_idx, ]))
  multinom_regression_predictions <- predict(multinom_regression_model, newdata = predict(preProcValues, X[outer_test_idx, ]))
  multinom_regression_E_test <- sum(multinom_regression_predictions != y[outer_test_idx]) / length(y[outer_test_idx])
  
  for(regularization_strength in c(1, 5, 10)){
    multinom_regression_model_1 <- glmnet(predict(preProcValues, X[outer_train_idx, ]), y[outer_train_idx], family = "multinomial", alpha = 0,
                  lambda = regularization_strength)
    multinom_regression_predictions_1 <- predict(multinom_regression_model_1,
                                                 as.matrix(predict(preProcValues, X[outer_test_idx, ])),
                                                 type = 'class', s = regularization_strength)
    multinom_regression_E_test_1 <- sum(multinom_regression_predictions_1 != y[outer_test_idx]) / length(y[outer_test_idx])

    print(paste('Normal:',multinom_regression_E_test,' /Reg(',regularization_strength,'):',multinom_regression_E_test_1))
  }
  # Calculate the baseline E_test
  baseline_predictions <- rep(names(sort(table(y[outer_train_idx]), decreasing = TRUE))[1], length(outer_test_idx))
  baseline_E_test <- sum(baseline_predictions != y[outer_test_idx]) / length(y[outer_test_idx])
  
  # Add the results to the data frame
  table_results <- rbind(table_results, 
                         data.frame(
                           OuterFold = i,
                           minsplit = best_minsplit, CT_E_test = best_ct_E_test,
                           k_neighbors = best_k, KNN_E_test = best_knn_E_test,
                           MultinomRegression_E_test = multinom_regression_E_test,
                           Baseline_E_test = baseline_E_test
                           )
                         )
  
  # browser()
  mcnemar_ct_multinom <- mcnemar(
    y[outer_test_idx],
    as.numeric(best_ct_predictions),
    as.numeric(multinom_regression_predictions),
    alpha = 0.5
  )
  
  mcnemar_knn_multinom <- mcnemar(
    y[outer_test_idx],
    as.numeric(best_knn_predictions),
    as.numeric(multinom_regression_predictions),
    alpha = 0.5
  )
  
  mcnemar_ct_baseline <- mcnemar(
    y[outer_test_idx], 
    as.numeric(best_ct_predictions), 
    as.numeric(baseline_predictions),
    alpha = 0.5
  )
  
  mcnemar_knn_baseline <- mcnemar(
    y[outer_test_idx], 
    as.numeric(best_knn_predictions), 
    as.numeric(baseline_predictions),
    alpha = 0.5
  )
  
  mcnemar_multinom_baseline <- mcnemar(
    y[outer_test_idx], 
    as.numeric(multinom_regression_predictions), 
    as.numeric(baseline_predictions),
    alpha = 0.5
  )
  
  # Add the predictions to the data frame
  classifier_results <- rbind(classifier_results,
                              tribble(
                                ~ct_multinom, ~knn_multinon, ~ct_baseline, ~knn_baseline, ~multinom_baseline,
                                mcnemar_ct_multinom, mcnemar_knn_multinom,mcnemar_ct_baseline,mcnemar_knn_baseline,mcnemar_multinom_baseline
                                )
                              )
  
}

table_results
