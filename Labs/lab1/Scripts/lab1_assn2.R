# ASSIGNMENT 2

# Package imports
library(caret)


### TASK 1 ###

# read the data
data <- read.csv("parkinsons.csv")

voice_features <- c("Jitter...", "Jitter.Abs.", "Jitter.RAP", "Jitter.PPQ5", "Jitter.DDP",
                    "Shimmer", "Shimmer.dB.", "Shimmer.APQ3", "Shimmer.APQ5", "Shimmer.APQ11", "Shimmer.DDA",
                    "NHR", "HNR",
                    "RPDE",
                    "DFA",
                    "PPE")

model_data <- data[, c(voice_features, "motor_UPDRS")]

# Get the number of rows in the data set
n <- dim(model_data)[1]

# set random seed for reproducibility
set.seed(12345)

# Partition 60% of data for the training set
id <- sample(1:n, floor(n * 0.6))
train <- model_data[id, ]

# Partition 40% of the data for the testing set
id1 <- setdiff(1:n, id)
test <- model_data[id1, ]

# Scale training data with caret
scaler <- preProcess(train)  # Learns scaling from training data
trainScaled <- predict(scaler, train)  # Applies scaling to training data

# Scale test data using same parameters from training data
testScaled <- predict(scaler, test)

### TASK 2, Linear Regression Model ###

# Fit linear regression model using scaled training data
# motor_UPDRS is the target variable, use all other columns as predictors
# motor_UPDRS means we're trying to predict motor_UPDRS 
fit <- lm(formula = motor_UPDRS ~ . - 1, data=trainScaled)  # -1 removes intercept

# Get summary to see which variables are significant
summary(fit) # DFA has a t-value of -13.921, the predictor with the highest absolute t-value

# Calculate MSE for training data
pred_train <- predict(fit, trainScaled)  # get predictions for training data
mse_train <- mean((trainScaled$motor_UPDRS - pred_train)^2)  # calculate training MSE

# Calculate MSE for test data
pred_test <- predict(fit, testScaled)  # get predictions for test data
mse_test <- mean((testScaled$motor_UPDRS - pred_test)^2)  # calculate test MSE

# Print MSE results
print(paste("Training MSE:", round(mse_train, 4)))
print(paste("Test MSE:", round(mse_test, 4)))

### TASK 3 ###

# 3a #
loglikelihood <- function(theta, sigma, traindata){
  # Get X matrix (predictors) from training data
  X <- as.matrix(traindata[, -which(names(traindata) == "motor_UPDRS")])
  
  # Get y (actual values) from training data
  y <- traindata$motor_UPDRS
  
  # Calculate predicted values (X * theta)
  y_pred <- X %*% theta

  # Calculate log-likelihood using normal distribution formula
  n <- length(y) # Number of observations
  loglik <- -n/2 * log(2*pi) - n/2 * log(sigma^2) - sum((y - y_pred)^2)/(2*sigma^2) # Minus for minimization

  return(loglik)
}

# 3b #
ridge <- function(theta, sigma, lambda, traindata){
  # Get negative log-likelihood (minus because we want to minimize)
  neg_loglik <- -loglikelihood(theta, sigma, traindata)
  
  # Calculate ridge penalty using: lambda * sum(theta^2)
  # We exclude first theta (intercept) from penalty
  ridge_penalty <- lambda * sum(theta^2)
  
  # Return total negative log-likelihood + ridge penalty
  return(neg_loglik + ridge_penalty)
}

# 3c #
# Function that takes lambda and finds optimal theta and sigma
ridgeOpt <- function(lambda, traindata) {
  # Number of parameters we need (number of columns - 1 for motor_UPDRS)
  n_params <- ncol(traindata) - 1  
  
  # Function that optim will minimize
  # Takes parameters and returns ridge value
  ridge_value <- function(params) {
    theta <- params[1:n_params]  # first n_params values are theta
    sigma <- params[n_params + 1]  # last value is sigma
    return(ridge(theta, sigma, lambda, traindata))
}
  
  # Starting values for optim: zeros for theta, 1 for sigma
  start_values <- c(rep(0, n_params), 1)
  
  # Run optimization
  result <- optim(par = start_values,       # starting values
                  fn = ridge_value,           # function to minimize
                  method = "BFGS")          # optimization method
  
  # Return optimized parameters
  return(result$par)  # returns optimal theta and sigma
}

# 3d #
DF <- function(lambda, traindata){
  # Remove motor_UPDRS
  X <- as.matrix(traindata[, -which(names(traindata) == "motor_UPDRS")])
  
  # Matrix Magic
  p <- ncol(X)
  XtX <- t(X) %*% X
  ridge_matrix <- XtX + lambda * diag(p)
  df <- sum(diag(X %*% solve(ridge_matrix) %*% t(X)))
  return(df)
}

### 4 ###

# ridgeOpt returns the optimal parameters using the ridge function

result1 <- ridgeOpt(lambda=1, trainScaled)        # lambda=1
result100 <- ridgeOpt(lambda=100, trainScaled)    # lambda=100
result1000 <- ridgeOpt(lambda=1000, trainScaled)  # lambda=1000

# Function to calculate MSE
calculate_mse <- function(theta, sigma, data) {
  # Make X matrix without intercept
  X <- as.matrix(data[, -which(names(data) == "motor_UPDRS")])
  pred <- X %*% theta # Calculate predictions
  mse <- mean((data$motor_UPDRS - pred)^2)
  return(mse)
}

#### Calculate MSE for each lambda ####

# Calculate how many predictors we have
n_params <- ncol(model_data) - 1  # -1 for motor_UPDRS

# Calculate MSE for each lambda
# Lambda = 1
mse_train1 <- calculate_mse(result1[1:n_params], result1[n_params + 1], trainScaled)
mse_test1 <- calculate_mse(result1[1:n_params], result1[n_params + 1], testScaled)

# Lambda = 100
mse_train100 <- calculate_mse(result100[1:n_params], result100[n_params + 1], trainScaled)
mse_test100 <- calculate_mse(result100[1:n_params], result100[n_params + 1], testScaled)

# Lambda = 1000
mse_train1000 <- calculate_mse(result1000[1:n_params], result1000[n_params + 1], trainScaled)
mse_test1000 <- calculate_mse(result1000[1:n_params], result1000[n_params + 1], testScaled)

# Calculate degrees of freedom (DF) for each lambda
df1 <- DF(1, trainScaled)
df100 <- DF(100, trainScaled)
df1000 <- DF(1000, trainScaled)

# Print the MSE results and degrees of freedom
print(paste("Lambda = 0 (without ridge function):    Train MSE:", round(mse_train, 4), 
            "Test MSE:", round(mse_test, 4), 
            "DF:", n_params))  # DF is number of predictors (no intercept)
print(paste("Lambda = 1:    Train MSE:", round(mse_train1, 4), 
            "Test MSE:", round(mse_test1, 4),
            "DF:", round(df1, 4)))
print(paste("Lambda = 100:  Train MSE:", round(mse_train100, 4), 
            "Test MSE:", round(mse_test100, 4),
            "DF:", round(df100, 4)))
print(paste("Lambda = 1000: Train MSE:", round(mse_train1000, 4), 
            "Test MSE:", round(mse_test1000, 4),
            "DF:", round(df1000, 4)))


# Higher lambda -> Higher MSE -> Predictions get worse with increasing lambda
# This suggests that original model wasn't overfitting, no need for regularization

# Data frame of coefficients for comparison
coef_comparison <- data.frame(
  Variable = colnames(trainScaled)[-which(names(trainScaled) == "motor_UPDRS")],
  Lambda0 = fit$coefficients,  # All coefficients (no intercept to exclude)
  Lambda1 = result1[1:n_params],
  Lambda100 = result100[1:n_params],
  Lambda1000 = result1000[1:n_params]
)

# Print to look at the coefficients
print("Coefficient comparison:")
print(coef_comparison)


