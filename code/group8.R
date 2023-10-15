library(pls)
library(randomForest)
library(rpart)
library(glmnet)
library(Metrics)
library(rpart.plot)

# Data Cleaning Function
clean_data <- function(data_path) {
  # Read the data
  data <- read.csv(data_path)
  
  # Remove rows with 0 or NA values
  data <- data[rowSums(data == 0) == 0, ]
  data <- na.omit(data)
  data <- data[-which.max(data$BODYFAT), ] # Remove row with max BODYFAT
  return(data)
}

# Load the dataset and clean the data
data <- clean_data("BodyFat.csv")

# Variable Selection

# Calculate the correlation matrix
cor_matrix <- cor(data)
print(cor_matrix)
## The r-square value for chest, abdomen, and adiposity are the highest against body fat with 0.687, 0.803 and 0.710 repectively. 

# Variable Selection Function
select_data <- function(data, vars) {
  return(data[, c(vars, "BODYFAT")])
}

# Model Training Function
train_models <- function(train_data) {
  # PCR
  pcr_model <- pcr(BODYFAT ~ ., data=train_data, validation="CV")
  # Random Forest
  rf_model <- randomForest(BODYFAT ~ ., data=train_data)
  # Decision Tree
  tree_model <- rpart(BODYFAT ~ ., data=train_data)
  # Linear Regression
  lm_model <- lm(BODYFAT ~ ., data=train_data)
  # Lasso Regression
  x_train <- as.matrix(train_data[, -ncol(train_data)])
  y_train <- train_data$BODYFAT
  lasso_model <- glmnet(x_train, y_train, alpha=1)
  
  return(list(pcr=pcr_model, rf=rf_model, tree=tree_model, lm=lm_model, lasso=lasso_model))
}

# Splitting Data Function
split_data <- function(data) {
  set.seed(42)
  train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
  train <- data[train_indices, ]
  test <- data[-train_indices, ]
  return(list(train=train, test=test))
}

# Performance Metrics Calculation Function
calculate_metrics <- function(models, test_data) {
  preds <- list(
    pcr=predict(models$pcr, test_data[, -ncol(test_data)]),
    rf=predict(models$rf, test_data),
    tree=predict(models$tree, test_data),
    lm=predict(models$lm, test_data),
    lasso=predict(models$lasso, s=0.01, newx=as.matrix(test_data[, -ncol(test_data)]))
  )
  
  R2 <- function(y_true, y_pred) {
    sst <- sum((y_true - mean(y_true))^2)
    ssr <- sum((y_true - y_pred)^2)
    return(1 - ssr/sst)
  }
  
  results <- lapply(preds, function(pred) {
    list(
      MAE = mae(test_data$BODYFAT, pred),
      MSE = mse(test_data$BODYFAT, pred),
      R2 = R2(test_data$BODYFAT, pred)
    )
  })
  
  results_df <- do.call(rbind, results)
  final_results <- data.frame(
    Model = names(results),
    MAE = sapply(results, function(x) x$MAE),
    MSE = sapply(results, function(x) x$MSE),
    R2 = sapply(results, function(x) x$R2)
  )
  
  return(final_results)
}

# Original Data with selected variables
selected_vars <- c('ABDOMEN', 'ADIPOSITY', 'CHEST')
data_selected <- select_data(data, selected_vars)
splits_original <- split_data(data_selected)
models_original <- train_models(splits_original$train)
results_original <- calculate_metrics(models_original, splits_original$test)

# Reduced Data with only CHEST and ABDOMEN
reduced_vars <- c('CHEST', 'ABDOMEN')
data_reduced <- select_data(data, reduced_vars)
splits_reduced <- split_data(data_reduced)
models_reduced <- train_models(splits_reduced$train)
results_reduced <- calculate_metrics(models_reduced, splits_reduced$test)

# Print the results
print("Original Data Results:")
print(results_original)
print("Reduced Data Results:")
print(results_reduced)

# Visualize the Decision Tree for Original Data
library(rpart.plot)
rpart.plot(models_original$tree, type=4, main="Decision Tree for Estimating Body Fat Percentage (Original Data)")

