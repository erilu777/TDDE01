#### INSTALL NECESSARY PACKAGES ####
library(glmnet)

#### DIVIDE THE DATA ####
# Load the data into a variable
data <- read.csv("tecator.csv", header = TRUE) 

# Get the number of rows in the dataset
n <- nrow(data) 

# Set a random seed for reproducibility
set.seed(12345)

# Partition 50% of the data for the training set
id <- sample(1:n, floor(n * 0.5))
train <- data[id, ]
test <- data[-id, ]

#### TASK 1.1 ####
# Train/fit the linear regression model, Fat is the target variable, . add all columns - takes away columns. 
fit <- lm(Fat ~ . - Protein - Moisture - Sample, data = train)
summary(fit)

# Calculate MSE for training data
pred_train <- predict(fit, train)  # get predictions for training data
mse_train <- mean((train$Fat - pred_train)^2)  # calculate training MSE
mse_train # 0.005709117

# Calculate MSE for test data
pred_test <- predict(fit, test)  # get predictions for test data
mse_test <- mean((test$Fat - pred_test)^2)  # calculate test MSE
mse_test # 722.4294

#### Task 1.2 ####

# Cost function (5,25) in text book & slide 20 in lecture 2d

#### Task 1.3 ####
# Prepare data: remove unnecessary columns
x_train <- as.matrix(train[, grep("^Channel", names(train))])  # Select all Channels
y_train <- train$Fat

# Fit LASSO regression model with alpha=1
fit_lasso <- glmnet(x_train, y_train, alpha=1)
summary(fit_lasso)

plot(fit_lasso, xvar = "lambda", label = TRUE, main = "LASSO Coefficients vs Log(Lambda)")

#### Task 1.4 ####
# Fit LASSO regression model with alpha=1
fit_ridge <- glmnet(x_train, y_train, alpha=0)
summary(fit_ridge)

plot(fit_ridge, xvar = "lambda", label = TRUE, main = "RIDGE Coefficients vs Log(Lambda)")


#### Task 1.5 ####
# Perform cross-validation to find the optimal lambda
cv_lasso <- cv.glmnet(x_train, y_train, alpha=1) # 10 folds by default
# Plot CV error vs log(lambda)
plot(cv_lasso, main = "CV Error vs Log(Lambda)") # CV score = MSE
# The two vertical dashed lines correspond to:
# LambdaMIN: The value of λλ that minimizes the CV error.
# Lamda2: The largest λλ within one standard error of the minimum.

# Optimal lambda and corresponding number of variables
optimal_lambda <- cv_lasso$lambda.min # .min = optimal value (minimum CV error), min() = minimum value
optimal_lambda # 0.05744535
optimal_coef <- coef(cv_lasso, s = "lambda.min")
optimal_coef
non_zero_vars <- sum(optimal_coef != 0) - 1  # Exclude intercept
non_zero_vars  # Number of variables = 8 + intercept

#test if it is true
x_test <- as.matrix(test[, grep("^Channel", names(test))])
y_test <- test$Fat

# Predict Fat values on the test set using the optimal lambda
pred_test <- predict(cv_lasso, s = "lambda.min", newx = x_test)

# Calculate MSE for the test set
mse_test <- mean((y_test - pred_test)^2)
cat("Test MSE:", mse_test, "\n")

# Now pred_test contains the predicted values at log(lambda) = -4
# Convert log(lambda) = -4 to lambda
lambda_at_neg4 <- exp(-4)

# Predict using the specified lambda
pred_test <- predict(cv_lasso, s = lambda_at_neg4, newx = x_test)

# Calculate MSE for the test set
mse_test <- mean((y_test - pred_test)^2)
cat("Test MSE, lamda =-4:", mse_test, "\n")

# Create scatter plot for actual vs predicted values at optimal lambda
plot(y_test, pred_test,
     xlab = "Actual Test Values",
     ylab = "Predicted Test Values",
     main = "Scatter Plot of Test vs Predicted Values (Optimal Lambda)",
     pch = 19, col = "blue")

# Add a line of perfect prediction
abline(a = 0, b = 1, col = "red", lty = 2)




