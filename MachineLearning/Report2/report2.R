# rm(list = ls()) # Clear work space

# Load necessary libraries
library(dplyr)
library(nnet)
library(glmnet)
library(caret)
library(class)
library(stats)
library(FNN)
library(rpart.plot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(hrbrthemes)
library(viridis)

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

default_classification_tests <- function(){
  # Load the glass dataset (replace with the actual dataset file path)
  data <- read.csv(here::here("glass.data"))
  
  data_t <- data
  
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
  
  possible_k_neighbors <- c(1:40)
  possible_minsplit <- seq(1:80)
  # some machines may have problem running with a lambda as low as 0.01,
  # this is a problem with glmnet function that we couldn't resolve even with TA's help
  # but we can see the convergence to better results as lambda approaches zero
  possible_lambda <- seq(0.04, 1, 0.01)
  
  classifier_results <- tribble(~ct, ~multinom, ~baseline, ~actual,
                                NA,NA,NA,NA)
  
  table_results <- data.frame(OuterFold = numeric(0), CT_E_test = numeric(0), k_neighbors = numeric(0), KNN_E_test = numeric(0), lambda = numeric(0), MultinomRegression_E_test = numeric(0), Baseline_E_test = numeric(0))
  
  # Implement two-level cross-validation
  set.seed(123)  # Set a seed for reproducibility
  outer_folds <- createFolds(y, k = K1, list = TRUE)
  preProcValues <- preProcess(X, method = c("center", "scale"))
  
  for (i in 1:K1) {
    outer_train_idx <- unlist(outer_folds[-i])
    outer_test_idx <- unlist(outer_folds[i])
    
    # Inner folds for inner validation
    inner_folds <- createFolds(y[outer_train_idx], k = K2, list = TRUE)
    
    # inner loop for tuning k-nearest neighbors k
    best_k <- 0
    best_inner_mean_accuracy <- 0
    
    for (k in possible_k_neighbors) {
      inner_results <- c()  # Store inner loop results
      
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
    
    # Calculate for the best KNN model
    best_knn_predictions <- knn(
      train = predict(preProcValues, X[outer_train_idx, ]),
      test = predict(preProcValues, X[outer_test_idx, ]),
      cl = y[outer_train_idx], k = best_k
    )
    best_knn_E_test <- sum(best_knn_predictions != y[outer_test_idx]) / length(y[outer_test_idx])
    
    # inner loop for tuning Classification tree
    best_min <- 0
    best_ct_model <- NULL
    best_inner_mean_accuracy <- 0
    
    for (minsplit in possible_minsplit) {
      inner_results <- c()  # Store inner loop results
      
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
    
    best_ct_predictions <- predict(best_ct_model, X[outer_test_idx, ], type = "class")
    best_ct_E_test <- sum(best_ct_predictions != y[outer_test_idx]) / length(y[outer_test_idx])
    prp(best_ct_model)
    
    # inner loop for tuning Multinomial regression
    best_lambda <- 0
    best_multinom_model <- NULL
    best_inner_mean_accuracy <- 0
    
    for(regularization_strength in possible_lambda){
      inner_results <- c()
      
      for (j in 1:K2) {
        inner_train_idx <- unlist(inner_folds[-j])
        inner_validation_idx <- unlist(inner_folds[j])
        
        X_inner_train <- X[inner_train_idx, ]
        X_inner_validation <- X[inner_validation_idx, ]
        
        y_inner_train <- y[inner_train_idx]
        y_inner_validation <- y[inner_validation_idx]
        
        multinom_model <- glmnet(predict(preProcValues, X[inner_train_idx, ]), y[inner_train_idx],
                                 family = "multinomial", alpha = 0, lambda = regularization_strength)
        multinom_predictions <- predict(multinom_model,
                                        as.matrix(predict(preProcValues, X[inner_validation_idx, ])),
                                        type = 'class', s = regularization_strength)
        multinom_accuracy <- sum(multinom_predictions == y_inner_validation) / length(y_inner_validation)
        
        inner_results <- c(inner_results, multinom_accuracy)
      }
      
      # Calculate the mean performance over inner validation folds for this model
      inner_mean_accuracy <- mean(inner_results)
      
      # Check if this model has a higher mean accuracy than the best one so far
      if (inner_mean_accuracy > best_inner_mean_accuracy) {
        best_inner_mean_accuracy <- inner_mean_accuracy
        best_lambda <- regularization_strength
        best_multinom_model <- multinom_model
      }
    }  
    # browser()
    # Calculate the multinomial regression E_test
    best_multinom_predictions <- predict(best_multinom_model,
                                         newx = as.matrix(predict(preProcValues, X[outer_test_idx, ])),
                                         type = 'class', s = best_multinom_model$lambda)
    multinom_regression_E_test <- sum(best_multinom_predictions != y[outer_test_idx]) / length(y[outer_test_idx])
    
    # Calculate the baseline E_test
    baseline_predictions <- rep(names(sort(table(y[outer_train_idx]), decreasing = TRUE))[1], length(outer_test_idx))
    baseline_E_test <- sum(baseline_predictions != y[outer_test_idx]) / length(y[outer_test_idx])
    
    # Add the results to the data frame
    table_results <- rbind(table_results, 
                           data.frame(
                             OuterFold = i,
                             minsplit = best_minsplit, CT_E_test = best_ct_E_test,
                             k_neighbors = best_k, KNN_E_test = best_knn_E_test,
                             lambda = best_lambda, MultinomRegression_E_test = multinom_regression_E_test,
                             Baseline_E_test = baseline_E_test
                           )
    )
    
    # Add the predictions to the data frame
    classifier_results <- rbind(
      classifier_results,
      c(list(best_ct_predictions), list(best_multinom_predictions), list(baseline_predictions), list(y[outer_test_idx]))
    )
    
  }
  
  return(list(classifier_results,table_results))
}

test_split <- function(){
  # Load the glass dataset (replace with the actual dataset file path)
  data <- read.csv("glass.data")
  
  data_t <- data
  
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
  
  possible_minsplit <- c(1:80)
  erros <- tribble(
    ~split,~train_e,~test_e,
    0,0,0
  )
  
  # Implement two-level cross-validation
  set.seed(123)  # Set a seed for reproducibility
  outer_folds <- createFolds(y, k = K1, list = TRUE)
  
  for (i in 1:K1) {
    outer_train_idx <- unlist(outer_folds[-i])
    outer_test_idx <- unlist(outer_folds[i])
    
    # Inner folds for inner validation
    inner_folds <- createFolds(y[outer_train_idx], k = K2, list = TRUE)
    
    # inner loop for tuning Classification tree
    best_minsplit <- 0
    best_ct_model <- NULL
    best_inner_mean_accuracy <- 0
    
    for (minsplit in possible_minsplit) {
      inner_results <- c()  # Store inner loop results
      
      for (j in 1:K2) {
        inner_train_idx <- unlist(inner_folds[-j])
        inner_validation_idx <- unlist(inner_folds[j])
        
        X_inner_train <- X[inner_train_idx, ]
        X_inner_validation <- X[inner_validation_idx, ]
        
        y_inner_train <- y[inner_train_idx]
        y_inner_validation <- y[inner_validation_idx]
        
        ct_model <- rpart(y_inner_train ~ ., data = X_inner_train,
                          control = rpart.control(minsplit = minsplit, cp = 0),
                          parms = list(split = "gini"), method = "class")
        ct_predictions <- predict(ct_model, X_inner_validation, type = "class")
        ct_accuracy <- sum(ct_predictions == y_inner_validation) / length(y_inner_validation)
        
        ct_train_predictions <- predict(ct_model, X_inner_train, type = "class")
        ct_train_e <- sum(ct_train_predictions != y_inner_train) / length(y_inner_train)
        ct_test_e <- sum(ct_predictions != y_inner_validation) / length(y_inner_validation)
        
        erros <- rbind(erros, c(minsplit, ct_train_e, ct_test_e))
        
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
    
    best_ct_validation <- predict(best_ct_model, X[outer_train_idx, ], type = "class")
    best_ct_predictions <- predict(best_ct_model, X[outer_test_idx, ], type = "class")
    best_ct_E_train <- sum(best_ct_validation != y[outer_train_idx]) / length(y[outer_train_idx])
    best_ct_E_test <- sum(best_ct_predictions != y[outer_test_idx]) / length(y[outer_test_idx])
    # prp(best_ct_model)
    print(best_minsplit)
    
  }
  
  return(erros)
}

test_deph <- function(){
  # Load the glass dataset (replace with the actual dataset file path)
  data <- read.csv("glass.data")
  
  data_t <- data
  
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
  
  possible_deph <- seq(1:15)
  
  erros <- tribble(
    ~deph,~train_e,~test_e,
    0,0,0
  )
  
  # Implement two-level cross-validation
  set.seed(123)  # Set a seed for reproducibility
  outer_folds <- createFolds(y, k = K1, list = TRUE)
  
  for (i in 1:K1) {
    outer_train_idx <- unlist(outer_folds[-i])
    outer_test_idx <- unlist(outer_folds[i])
    
    # Inner folds for inner validation
    inner_folds <- createFolds(y[outer_train_idx], k = K2, list = TRUE)
    
    # inner loop for tuning Classification tree
    best_deph <- 0
    best_ct_model <- NULL
    best_inner_mean_accuracy <- 0
    
    for (deph in possible_deph) {
      inner_results <- c()  # Store inner loop results
      
      for (j in 1:K2) {
        inner_train_idx <- unlist(inner_folds[-j])
        inner_validation_idx <- unlist(inner_folds[j])
        
        X_inner_train <- X[inner_train_idx, ]
        X_inner_validation <- X[inner_validation_idx, ]
        
        y_inner_train <- y[inner_train_idx]
        y_inner_validation <- y[inner_validation_idx]
        
        ct_model <- rpart(y_inner_train ~ ., data = X_inner_train,
                          control = rpart.control(cp = 0,maxdepth = deph),
                          parms = list(split = "gini"), method = "class")
        ct_predictions <- predict(ct_model, X_inner_validation, type = "class")
        ct_accuracy <- sum(ct_predictions == y_inner_validation) / length(y_inner_validation)
        
        ct_train_predictions <- predict(ct_model, X_inner_train, type = "class")
        ct_train_e <- sum(ct_train_predictions != y_inner_train) / length(y_inner_train)
        ct_test_e <- sum(ct_predictions != y_inner_validation) / length(y_inner_validation)
        
        erros <- rbind(erros, c(deph, ct_train_e, ct_test_e))
        
        inner_results <- c(inner_results, ct_accuracy)
      }
      
      # Calculate the mean performance over inner validation folds for this model
      inner_mean_accuracy <- mean(inner_results)
      
      # Check if this model has a higher mean accuracy than the best one so far
      if (inner_mean_accuracy > best_inner_mean_accuracy) {
        best_inner_mean_accuracy <- inner_mean_accuracy
        best_deph <- deph
        best_ct_model <- ct_model
      }
      
    }
    
    best_ct_validation <- predict(best_ct_model, X[outer_train_idx, ], type = "class")
    best_ct_predictions <- predict(best_ct_model, X[outer_test_idx, ], type = "class")
    best_ct_E_train <- sum(best_ct_validation != y[outer_train_idx]) / length(y[outer_train_idx])
    best_ct_E_test <- sum(best_ct_predictions != y[outer_test_idx]) / length(y[outer_test_idx])
    # prp(best_ct_model)
    print(best_deph)
    
  }
  
  return(erros)
}

coefficients_info <- function(best_multinom_model){
  # Get the coefficients
  coefficients <- coef(best_multinom_model)
  
  # Convert each matrix to a dense matrix
  dense_matrices <- lapply(coefficients, as.matrix)
  
  # Convert to a data frame
  coefficients_df <- t(as.data.frame(dense_matrices))
  
  # Print the resulting data frame
  print(coefficients_df)
  
  column_means <- apply(coefficients_df, 2, function(x) mean(abs(x)))
  
  print(column_means)
  
}

graph_depth_e_evolution <- function(depth_tests){
  deph <- t_deph[-1,] %>% 
    mutate(deph = as.factor(deph)) %>% 
    group_by(deph) %>% 
    mutate(
      'Test error' = mean(test_e),
      'Train error' = mean(train_e)
    ) %>% 
    select(-c(test_e,train_e)) %>% 
    pivot_longer(!deph,names_to = 'type', values_to = 'error') %>% 
    distinct()
  
  # Plot
  deph %>%
    ggplot( aes(x=deph, y=error, group=type, color=type)) +
    geom_line() +
    scale_color_viridis(discrete = TRUE) +
    ggtitle("Train/Test error by maxdeph") +
    theme_ipsum() +
    ylab("Error (missclassification rate, CV = 10)")
}

graph_split_e_evolution <- function(split_tests){
  
  split <- t_split[-1,] %>% 
    mutate(split = as.factor(split)) %>% 
    group_by(split) %>% 
    mutate(
      'Test error' = mean(test_e),
      'Train error' = mean(train_e)
    ) %>% 
    select(-c(test_e,train_e)) %>% 
    pivot_longer(!split,names_to = 'type', values_to = 'error') %>% 
    distinct()
  
  # Plot
  split %>%
    ggplot( aes(x=split, y=error, group=type, color=type)) +
    geom_line() +
    scale_color_viridis(discrete = TRUE) +
    theme_ipsum() +
    xlab("Minimum size of split")+
    ylab("Error (missclassification rate, CV = 10)")+
    scale_x_discrete(breaks = seq(1, 80, by = 10))+
    labs(color = NULL)
}

# Python code to lambda part
# tune regularization for multinomial logistic regression
# import pandas as pd
# from numpy import mean
# from numpy import std
# from sklearn.datasets import make_classification
# from sklearn.model_selection import cross_val_score
# from sklearn.model_selection import RepeatedStratifiedKFold
# from sklearn.linear_model import LogisticRegression
# from matplotlib import pyplot
# 
# # get the dataset
# def get_dataset():
#   dataset = pd.read_csv('/content/glass.data')
# X = dataset.drop(columns=['Id', 'Type'])
# y = dataset['Type']
# return X, y
# 
# # get a list of models to evaluate
# def get_models():
#   models = dict()
# for p in [0.0, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1.0, 10]:
#   # create name for model
#   key = '%.4f' % p
# # turn off penalty in some cases
# if p == 0.0:
#   # no penalty in this case
#   models[key] = LogisticRegression(multi_class='multinomial', solver='lbfgs', penalty='none')
# else:
#   models[key] = LogisticRegression(multi_class='multinomial', solver='lbfgs', penalty='l2', C=p)
# return models
# 
# # evaluate a give model using cross-validation
# def evaluate_model(model, X, y):
#   # define the evaluation procedure
#   cv = RepeatedStratifiedKFold(n_splits=10, n_repeats=3, random_state=1)
# # evaluate the model
# scores = cross_val_score(model, X, y, scoring='accuracy', cv=cv, n_jobs=-1)
# return scores
# 
# # define dataset
# X, y = get_dataset()
# # get the models to evaluate
# models = get_models()
# # evaluate the models and store results
# results, names = list(), list()
# for name, model in models.items():
#   # evaluate the model and collect the scores
#   scores = evaluate_model(model, X, y)
# # store the results
# results.append(scores)
# names.append(name)
# # summarize progress along the way
# print('>%s %.3f (%.3f)' % (name, mean(scores), std(scores)))
# # plot model performance for comparison
# pyplot.boxplot(results, labels=names, showmeans=True)
# pyplot.xlabel('Lambda values')
# pyplot.ylabel('Missclassification Error, CV = 10')
# pyplot.show()



