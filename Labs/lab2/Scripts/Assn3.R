#### INSTALL NECESSARY PACKAGES ####
install.packages("dplyr")
install.packages("caret")
install.packages("ggplot2")
library(caret)
library(dplyr)
library(ggplot2)

#### Load the Data ####
# Load the data into a variable
data <- read.csv("communities.csv", header = T) 


####################
##### Task 3.1 #####
####################

features<- data %>% select(-ViolentCrimesPerPop)

# Use caret to scale and center the data
scaler <- preProcess(features, method = c("center", "scale"))
data_scaled <- predict(scaler, data) # Scaling all varaibles except ViolentCrimesPerPop

# Compute covariance matrix
cov_matrix <- cov(data_scaled)

# Perform PCA using eigen()
pca_result <- eigen(cov_matrix)
pca_result

# Extract eigenvalues and eigenvectors
eigenvalues <- pca_result$values
eigenvectors <- pca_result$vectors

# Proportion of variance explained by each component
eigen_percent <- eigenvalues / sum(eigenvalues)
cumulative_eigen_percent <- cumsum(eigen_percent)
eigen_percent
cumulative_eigen_percent

# Determine the number of components needed to explain at least 95% of the variance
n_commponets <- which(cumulative_eigen_percent>= 0.95)[1]
n_commponets # 35

#calculate the two most significant components, 
first_component_proportion <- eigen_percent[1]
first_component_proportion # 0.2502494
second_component_proportion <- eigen_percent[2]
second_component_proportion # 0.1693082

####################
##### Task 3.2 #####
####################

pca_princomp <- princomp(data, cor = TRUE)

summary(pca_princomp)

# Extract loadings (coefficients of each feature in the principal components)
loadings <- pca_princomp$loadings[, 1]  # Loadings for PC1


loadings_df <- data.frame(
  Feature = names(loadings),
  Contribution = loadings
)

ggplot(loadings_df, aes(x = reorder(Feature, -abs(Contribution)), y = Contribution)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Plot for First Principal Component (PC1)",
       x = "Features",
       y = "Contribution") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 4))  # Adjust font size for y-axis labels


# Identify the top 5 features with the largest absolute loadings in PC1
top5_features <- sort(abs(loadings), decreasing = TRUE)[1:5]
top5_feature_names <- names(top5_features)

# Print the top 5 features in PC1
print(top5_feature_names)
print(top5_features)


# Create a data frame with PC1, PC2, and crime levels
pca_scores <- data.frame(
  FirstPC = pca_princomp$scores[, 1],  # Scores for PC1
  SecondPC = pca_princomp$scores[, 2], # Scores for PC2
  CrimeRate = data$ViolentCrimesPerPop  # Crime levels
)

# Plot PC1 vs PC2 with crime levels as color
ggplot(pca_scores, aes(x = FirstPC, y = SecondPC, color = CrimeRate)) +
  geom_point() +  
  scale_color_gradient(low = "blue", high = "red") +  # Different color gradient
  labs(
    title = "PC1 vs PC2 with Violent Crime Rate",
    x = "PC1",
    y = "PC2",
    color = "Violent Crime Per Population"
  ) 

####################
##### Task 3.3 #####
####################

# Get the number of rows in the dataset
n <- nrow(data) 

# Set a random seed for reproducibility
set.seed(12345)

# Partition 50% of the data for the training set
id <- sample(1:n, floor(n * 0.5))
train <- data[id, ]
test <- data[-id, ]

# Scale the training data (features and target)
scaler2 <- preProcess(train, method = c("center", "scale"))
train_scaled <- predict(scaler2, train)
test_scaled <- predict(scaler2, test)


# Train/fit the linear regression model, Violent crime per pop is the target variable, . adds all columns - takes away columns.
fit <- lm(ViolentCrimesPerPop ~ . , data = train_scaled)
summary(fit)

# Predict and calculate MSE for training data
pred_train_scaled <- predict(fit, train_scaled)  # get predictions for training data
mse_train_scaled <- mean((train_scaled$ViolentCrimesPerPop - pred_train_scaled)^2)  # calculate training MSE
mse_train_scaled # 0.2752071

# Calculate MSE for test data
pred_test_scaled <- predict(fit, test_scaled)  # get predictions for training data
mse_test_scaled <- mean((test_scaled$ViolentCrimesPerPop - pred_test_scaled)^2)  # calculate training MSE
mse_test_scaled # 0.4248011

####################
##### Task 3.4 #####
####################

# Initialize storage variables and counter
train_MSE <- list()   # Will store training errors for each iteration
test_MSE <- list()    # Will store test errors for each iteration
k <- 0                # Counter to keep track of iterations

# Find which column contains the target variable (ViolentCrimesPerPop) to make it dynamic
target_col <- grep("ViolentCrimesPerPop", colnames(train_scaled))

# Prepare matrices for training and testing
# Separate features (X) from target values (y)
train_X <- as.matrix(train_scaled[, -target_col])  # All columns except target
test_X <- as.matrix(test_scaled[, -target_col])    # All columns except target
train_y <- as.matrix(train_scaled[, target_col])   # Only target column
test_y <- as.matrix(test_scaled[, target_col])     # Only target column

# Cost function that will be used in optimization
# 1. Tracks iteration number
# 2. Calculates predictions and errors
# 3. Stores errors for later analysis
cost_function <- function(theta) {
  # Increment iteration 
  .GlobalEnv$k <- .GlobalEnv$k + 1
  
  # Make predictions using matrix multiplication
  train_pred <- train_X %*% theta
  test_pred <- test_X %*% theta
  
  # Calculate MSE for both sets (MSE is our loss function)
  cost_train <- mean((train_y - train_pred)^2)
  cost_test <- mean((test_y - test_pred)^2)
  
  # Store MSE in global lists
  .GlobalEnv$train_MSE[[k]] <- cost_train
  .GlobalEnv$test_MSE[[k]] <- cost_test
  
  # Return training cost (this is what optim will try to minimize)
  return(cost_train)
}

# Run the optimization using BFGS method
# Start with all parameters (theta)(the coefficients) set to zero
initial_theta <- rep(0, ncol(train_X))
result <- optim(par = initial_theta, 
                fn = cost_function,
                method = "BFGS")

# Convert lists of errors to vectors for easier plotting
train_errors <- unlist(train_MSE)
test_errors <- unlist(test_MSE)

# Start plotting from iteration 500
# Create a data frame for plotting
plot_data <- data.frame(
  iteration = 500:length(train_errors),
  train = train_errors[500:length(train_errors)],
  test = test_errors[500:length(test_errors)]
)

# Find the optimal iteration number using early stopping criterion
# Look for the iteration with minimum test error (after iteration 500)
optimal_iteration <- which.min(test_errors[500:length(test_errors)]) + 499

# Create the plot using ggplot2
library(ggplot2)
ggplot(plot_data, aes(x = iteration)) +
  geom_line(aes(y = train, color = "Training Error")) +
  geom_line(aes(y = test, color = "Test Error")) +
  geom_vline(xintercept = optimal_iteration, 
             linetype = "dashed", 
             color = "black") +
  annotate("text", 
           x = optimal_iteration + 500, 
           y = max(plot_data$train), 
           label = paste("Optimal iteration:", optimal_iteration)) +
  labs(title = "Training and Test Errors vs Iteration",
       x = "Iteration", 
       y = "Mean Squared Error",
       color = "Error Type") +
  theme_minimal()

# Print the results
cat("\nResults of early stopping analysis:\n")
cat("Optimal iteration number:", optimal_iteration, "\n")
cat("Training MSE at optimal iteration:", train_errors[optimal_iteration], "\n")
cat("Test MSE at optimal iteration:", test_errors[optimal_iteration], "\n")





