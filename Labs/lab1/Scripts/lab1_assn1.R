#### INSTALL NECESSARY PACKAGES ####
install.packages("kknn")
library(kknn)

#### DIVIDE THE DATA ####

# Load the data into a variable
data <- read.csv("optdigits.csv", header=F) 

# Get the number of rows in the dataset
n <- dim(data)[1]

# Set a random seed for reproducibility
set.seed(12345)

# Partition 50% of the data for the training set
id <- sample(1:n, floor(n * 0.5))
train <- data[id, ]

# Partition 25% of the data for the validation set
id1 <- setdiff(1:n, id)
set.seed(12345)
id2 <- sample(id1, floor(n * 0.25))
valid <- data[id2, ]

# Use the rest for the test set
id3 <- setdiff(id1, id2)
test <- data[id3, ]


#### FIT A K-NEAREST NEIGHBOR MODEL TO TRAIN DATA ####

# Fitting = Learning = Training...?

# Fit the model on training data and test on training data
model_train <- kknn(as.factor(V65) ~ ., train = train, test = train, k = 30, kernel = "rectangular")
train_pred <- fitted(model_train)
confusion_matrix_train <- table(Truth = train$V65, Predicted = train_pred)
confusion_matrix_train

# Define the misclassification error function
missclass <- function(truth, predicted) {
  n <- length(truth)
  return(1 - sum(diag(table(truth, predicted))) / n)
}

# Misclassification errors for the training data
train_misclass_error <- missclass(train$V65, train_pred)
print(train_misclass_error) #0.04500262

# Fit the model on training data and test on test data
model_test <- kknn(as.factor(V65) ~ ., train = train, test = test, k = 30, kernel = "rectangular")
test_pred <- fitted(model_test)
confusion_matrix_test <- table(Truth = test$V65, Predicted = test_pred)
confusion_matrix_test

# Misclassification errors for the training data
test_misclass_error <- missclass(test$V65, test_pred)
print(test_misclass_error) #0.05329154

#### TASK 1.3 ####
  # Get probabilities of each class for each case in the training data
train_probs <- model_train$prob
  # Filter cases where the true label is "8"
eight_indices <- which(train$V65 == 8)

  # Get the probabilities for the correct class (8) for each case
eight_probs <- train_probs[eight_indices, "8"]

# Sort by probability: highest for easiest, lowest for hardest
sorted_indices <- order(eight_probs, decreasing = TRUE)
easiest_indices <- eight_indices[sorted_indices[1:2]]   # Two highest probabilities
hardest_indices <- eight_indices[sorted_indices[(length(sorted_indices)-2):length(sorted_indices)]]  # Three lowest probabilities

# Function to visualize an 8x8 matrix of a digit with index
visualize_digit <- function(data_row, label, index) {
  # Reshape to 8x8 matrix
  digit_matrix <- matrix(as.numeric(data_row), nrow = 8, ncol = 8, byrow = TRUE)
  # Plot heatmap
  heatmap(digit_matrix, Rowv = NA, Colv = NA, scale = "none", col = heat.colors(256),
          main = paste("Digit", label, "- Index", index))
}

# Visualize easiest cases for digit "8" with indices
for (i in easiest_indices) {
  visualize_digit(train[i, -ncol(train)], train$XV65[i], i)  # Include index
}

# Visualize hardest cases for digit "8" with indices
for (i in hardest_indices) {
  visualize_digit(train[i, -ncol(train)], train$V65[i], i)  # Include index
}


#### TASK 1.4 ####

Kvalues <- 1:30
train_errors <- numeric(length(Kvalues)) #creates a empty set of vectors
valid_errors <- numeric(length(Kvalues)) #creates a empty set of vectors

for (k in Kvalues) {
  # Fit KNN model on training data and predict on training data
  model_train <- kknn(as.factor(V65) ~ ., train = train, test = train, k = k, kernel = "rectangular")
  train_pred <- fitted(model_train)
  train_errors[k] <- missclass(train$V65, train_pred)  # Store training error
  
  # Fit KNN model on training data and predict on validation data
  model_valid <- kknn(as.factor(V65) ~ ., train = train, test = valid, k = k, kernel = "rectangular")
  valid_pred <- fitted(model_valid)
  valid_errors[k] <- missclass(valid$V65, valid_pred)  # Store validation error
}

# Set up plot for training errors with improved readability
plot(Kvalues, train_errors, type = "o", col = "blue", pch = 16, lwd = 2, cex = 1.5, 
     xlab = "K Value", ylab = "Misclassification Error", 
     main = "Training and Validation Errors vs. K", 
     ylim = range(c(train_errors, valid_errors)), cex.lab = 1.5, cex.main = 1.5)

# Add validation errors with a thicker line and larger points
lines(Kvalues, valid_errors, type = "o", col = "red", pch = 16, lwd = 2, cex = 1.5)

# Add grid lines for readability
grid(nx = NULL, ny = NULL, lty = 2, col = "gray")

# Add a legend with larger font size
legend("bottomright", legend = c("Training Error", "Validation Error"), 
       col = c("blue", "red"), pch = 16, lty = 1, lwd = 2, cex = 1.2)

# Add a vertical line at the optimal K value
optimal_k <- which.min(valid_errors)
abline(v = Kvalues[optimal_k], col = "green", lty = 2, lwd = 2)
text(Kvalues[optimal_k], valid_errors[optimal_k] + 0.002, labels = paste("Optimal K =", Kvalues[optimal_k]), col = "black", pos = 4)

#Estimate the test error for k=optimal_k
model_test <- kknn(as.factor(V65) ~ ., train = train, test = test, k = 8, kernel = "rectangular")
test_pred <- fitted(model_test)
test_misclass_error <- missclass(test$V65, test_pred)
print(test_misclass_error) #0.03761755


#### TASK 1.5 ####

# Define the cross-entropy error function
CE <- function(truth, probability_matrix) {
  epsilon <- 1e-15  # Small constant to avoid log(0)
  cross_entropy <- 0
  
  # Loop over each sample
  for (i in 1:length(truth)) {
    # Get the true class for the i-th sample
    true_class <- as.integer(truth[i])+1
    
    # Compute cross-entropy for this sample
    cross_entropy <- cross_entropy - log(probability_matrix[i, true_class] + epsilon)
  }
  
  # Return the average cross-entropy error
  return(cross_entropy / length(truth))
}

# Define the range of K values
Kvalues <- 1:30
cross_entropy_errors <- numeric(length(Kvalues))# Create an empty vector for cross-entropy errors
epsilon <- 1e-15  # Small constant to avoid log(0)

# Loop over each K value
for (k in Kvalues) {
  # Fit KNN model on training data and predict on validation data
  model_valid <- kknn(as.factor(V65) ~ ., train = train, test = valid, k = k, kernel = "rectangular")
  
  # Extract predicted probabilities for each class
  probs <- model_valid$prob
  valid_labels <- valid$V65
  
  
  
  # Calculate cross-entropy error for the validation set and store it
  cross_entropy_errors[k] <- CE(valid_labels, probs)
}

# Plot Cross-Entropy Error vs. K
plot(Kvalues, cross_entropy_errors, type = "o", col = "blue", pch = 16,
     xlab = "K Value", ylab = "Cross-Entropy Error", main = "Cross-Entropy Error vs. K")

# Identify and mark the optimal K (minimum cross-entropy)
optimal_k <- which.min(cross_entropy_errors)
abline(v = Kvalues[optimal_k], col = "green", lty = 2, lwd = 2)
points(Kvalues[optimal_k], cross_entropy_errors[optimal_k], col = "red", pch = 19)
text(Kvalues[optimal_k], cross_entropy_errors[optimal_k], labels = paste("Optimal K =", Kvalues[optimal_k]), pos = 3)


  
