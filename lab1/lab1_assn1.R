#### INSTALL NECESSARY PACKAGES ####
install.packages("kknn")
install.packages("ggplot2")
install.packages("knitr")
library(kknn)
library(ggplot2)
library(knitr)


#### DIVIDE THE DATA ####

# Load the data into a variable
data <- read.csv("optdigits.csv") 

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
model_train <- kknn(as.factor(X0.26) ~ ., train = train, test = train, k = 30, kernel = "rectangular")
train_pred <- fitted(model_train)
confusion_matrix_train <- table(Truth = train$X0.26, Predicted = train_pred)
confusion_matrix_train

# Define the misclassification error function
missclass <- function(truth, predicted) {
  n <- length(truth)
  return(1 - sum(diag(table(truth, predicted))) / n)
}

# Misclassification errors for the training data
train_misclass_error <- missclass(train$X0.26, train_pred)
print(train_misclass_error)


# Fit the model on training data and test on test data
model_test <- kknn(as.factor(X0.26) ~ ., train = train, test = test, k = 30, kernel = "rectangular")
test_pred <- fitted(model_test)
confusion_matrix_test <- table(Truth = test$X0.26, Predicted = test_pred)
confusion_matrix_test

# Misclassification errors for the training data
test_misclass_error <- missclass(test$X0.26, test_pred)
print(test_misclass_error)


