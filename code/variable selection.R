
# 1. Data Cleaning

# Read the data
# Load the dataset
data <- read.csv("BodyFat.csv")

# Remove rows with 0 or NA values
data <- data[rowSums(data == 0) == 0, ]
data <- na.omit(data)
data <- data[-which.max(data$BODYFAT), ]

# 2. Variable Selection

# Calculate the correlation matrix
cor_matrix <- cor(data)

# Extract the target variables based on our previous selection
selected_vars <- c('ABDOMEN', 'ADIPOSITY', 'CHEST', 'BODYFAT')

# Subset the data
data_selected <- data[, selected_vars]

# 3. Model Training

# Splitting the data into 80/20
set.seed(42)
train_indices <- sample(1:nrow(data_selected), 0.8 * nrow(data_selected))
train_data <- data_selected[train_indices, ]
test_data <- data_selected[-train_indices, ]

# PCR (Principal Component Regression)
library(pls)
pcr_model <- pcr(BODYFAT ~ ., data=train_data, validation="CV")
pcr_pred <- predict(pcr_model, test_data[, -4])

# Random Forest
library(randomForest)
rf_model <- randomForest(BODYFAT ~ ., data=train_data)
rf_pred <- predict(rf_model, test_data)

# Decision Tree
library(rpart)
tree_model <- rpart(BODYFAT ~ ., data=train_data)
tree_pred <- predict(tree_model, test_data)

# Linear Regression
lm_model <- lm(BODYFAT ~ ., data=train_data)
lm_pred <- predict(lm_model, test_data)

# Lasso Regression
library(glmnet)
x_train <- as.matrix(train_data[, -4])
y_train <- train_data$BODYFAT
x_test <- as.matrix(test_data[, -4])
lasso_model <- glmnet(x_train, y_train, alpha=1)
lasso_pred <- predict(lasso_model, s=0.01, newx=x_test)

# Performance Metrics
library(Metrics)
models <- list(pcr=pcr_pred, rf=rf_pred, tree=tree_pred, lm=lm_pred, lasso=lasso_pred)

#Interpretation:

#The Decision Tree model has the best performance metrics across the board. It has the lowest MAE and MSE, meaning its predictions are, on average, the closest to the actual values and has the least squared error. Its R 
#2 value is the highest, indicating it explains about 69.25% of the variance in the body fat percentage using the selected features.
#The Random Forest model comes in as the second best, with slightly higher MAE and MSE values than the Decision Tree and an 
#R2of 66.27%.PCR is performing poorly given its negative R
#2 value.Given these results, the Decision Tree is the best model among those evaluated for this dataset using the selected features. It provides a balance of accuracy and interpretability, making it especially suitable for a "rule-of-thumb" approach.

# Calculate R2 manually
R2 <- function(y_true, y_pred) {
  sst <- sum((y_true - mean(y_true))^2)
  ssr <- sum((y_true - y_pred)^2)
  return(1 - ssr/sst)
}

# Update results calculation
results <- lapply(models, function(pred) {
  list(
    MAE = mae(test_data$BODYFAT, pred),
    MSE = mse(test_data$BODYFAT, pred),
    R2 = R2(test_data$BODYFAT, pred)
  )
})

# Convert results list to a data frame
results_df <- do.call(rbind, results)

# Convert the list of lists into a data frame
final_results <- data.frame(
  Model = names(results),
  MAE = sapply(results, function(x) x$MAE),
  MSE = sapply(results, function(x) x$MSE),
  R2 = sapply(results, function(x) x$R2)
)

# Print the final results table
print(final_results)

library(rpart.plot)

# Visualize the Decision Tree without the extra argument
rpart.plot(tree_model, type=4, main="Decision Tree for Estimating Body Fat Percentage")




