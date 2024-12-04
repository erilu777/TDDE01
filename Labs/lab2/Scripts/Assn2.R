#### INSTALL NECESSARY PACKAGES ####
install.packages("tree")
library(tree)


#### TASK 2.1 - Divide the Data ####
# Load the data into a variable
data <- read.csv("bank-full.csv", 
                 header = T, sep=";", 
                 stringsAsFactors = TRUE) # Note: read.csv2 takes the separator as ; as default

# Remove duration column from data set
data <- data[, !names(data) %in% c("duration")]

# Get the number of rows in the dataset
n <- dim(data)[1]

# Set a random seed for reproducibility
set.seed(12345)

# Partition 40% of the data for the training set
id <- sample(1:n, floor(n * 0.4))
train <- data[id, ]

# Partition 30% of the data for the validation set
id1 <- setdiff(1:n, id)
set.seed(12345)
id2 <- sample(id1, floor(n * 0.3))
valid <- data[id2, ]

# Use the rest for the test set
id3 <- setdiff(id1, id2)
test <- data[id3, ]




#### Task 2.2 - Different Decision Tree Models ####

# Model A - Default fit
fit_default <- tree(y ~ ., data = train)
train_pred_default <- predict(fit_default, train, type = "class") # Prediction on train data
valid_pred_default <- predict(fit_default, valid, type = "class") # Prediction on validation data

# Calculate misclassification errors
train_mis_default <- mean(train_pred_default != train$y) # 0.1048441
valid_mis_default <- mean(valid_pred_default != valid$y) # 0.1092679


# Model B - Smallest allowed node size = 7000
# This means that a node must have minimum 7000 observations in order to be able to split
# However, a split of 14'000 can be 10'000 to left and 4'000 to right

n_train <- dim(train)[1] # Nr of obeservations (rows) in train data

# Fit the model with a smallest allowed node size of 7000
fit_min_node <- tree(y ~ ., 
                     data = train, 
                     control = tree.control(nobs = n_train, minsize = 7000))  # Set the minimum node size to 7000

train_pred_min_node <- predict(fit_min_node, train, type = "class") # Prediction on train data
valid_pred_min_node <- predict(fit_min_node, valid, type = "class") # Prediction on validation data

# Calculate misclassification errors
train_mis_min_node <- mean(train_pred_min_node != train$y) # 0.1048441
valid_mis_min_node <- mean(valid_pred_min_node != valid$y) # 0.1092679


# Model C - Minimum deviance = 0.0005
# This parameter controls the minimum deviance for a node to be split further. 
# The lower the value, the more sensitive the tree will be to potential splits.

# Fit the tree model with a minimum deviance of 0.0005
fit_min_dev <- tree(y ~ ., 
                         data = train, 
                         control = tree.control(nobs = n_train, mindev = 0.0005))

train_pred_min_dev <- predict(fit_min_dev, train, type = "class") # Prediction on train data
valid_pred_min_dev <- predict(fit_min_dev, valid, type = "class") # Prediction on validation data

# Calculate misclassification errors
train_mis_min_dev <- mean(train_pred_min_dev != train$y) # 0.09400575
valid_mis_min_dev <- mean(valid_pred_min_dev != valid$y) # 0.1119221

# Visualize all trees
plot(fit_default)
text(fit_default, pretty = 0)
summary(fit_default)

plot(fit_min_node)
text(fit_min_node, pretty = 0)
summary(fit_min_node)

plot(fit_min_dev)
text(fit_min_dev, pretty = 0)
summary(fit_min_dev)




#### TASK 2.3 - Optimal Tree Depth (Number of Leaves) ####

# Clarification of Termnial Nodes, Leaves and Depth:
# Terminal nodes = Leaves = Final nodes in a decision tree with no further splits = Classification decisions
# Depth = Length of the longest path from the root node to a terminal node (leaf)


# Use model 2C
fit_full_tree <- fit_min_dev # Fitted with full depth from start = fully grown decision tree

# Create arrays to store deviance values for 1 to 50 terminal nodes (leaves)
trainDeviance <- rep(2,50)
validDeviance <- rep(2,50)

# Iterate over possible number of terminal nodes (leaves) from 1 to 50
for (i in 2:50) {
  # Prune the tree to have i terminal nodes
  prunedTree <- prune.tree(fit_full_tree, best = i) # best = nr of terminal nodes (or leaves)
  
  # Make predictions on validation data
  valid_pred <- predict(prunedTree, newdata = valid, type = "tree")
  
  # Store deviance values
  trainDeviance[i] = deviance(prunedTree)
  validDeviance[i] = deviance(valid_pred)
}

# Plot train and validation deviances on nr of terminal nodes (leaves)
plot(2:50, trainDeviance[2:50], type = "b", col = "red",
     ylim = c(min(c(trainDeviance[3:50], validDeviance[3:50])), max(c(trainDeviance, validDeviance))), # Size of y-axis
     xlab = "Number of Leaves", ylab = "Deviance", 
     main = "Deviance vs. Number of Leaves")
points(2:50, validDeviance[2:50], type = "b", col = "blue")
legend("topright", legend = c("Training Deviance", "Validation Deviance"), 
       col = c("red", "blue"), pch = 19)

# Find optimal number of leaves <==> where validDeviance is minimized
optimal_leaves <- which.min(validDeviance[2:50]) + 1 # Add 1 because starting from 2
optimal_leaves # 22

# Visualize tree with depth 22
optimalTree <- prune.tree(fit_full_tree, best = optimal_leaves)
plot(optimalTree)
text(optimalTree, pretty = 0)
summary(optimalTree)

# Most important variables for decision making in this tree (top nodes):
# poutcome, month, contact




#### TASK 2.4 - Confusion Matrix, Accuracy & F1 Score ####

# Confusion matrix based on test data
pred_test <- predict(optimalTree, newdata = test, type = "class") # Predictions on test set
confusion_matrix <- table(True = test$y, Predicted = pred_test)
print(confusion_matrix)

# Compute necessary variables
n_test <- dim(test)[1] # Nr of obeservations (rows) in train data
TP <- confusion_matrix["yes", "yes"] # True Positive. 214
TN <- confusion_matrix["no", "no"] # True Negative.   11872
FP <- confusion_matrix["no", "yes"] # False Positive  107
FN <- confusion_matrix["yes", "no"] # False Negative  1371

# Accuracy based on test data = (TP + TN) / n
accuracy <- (TP + TN) / n_test
accuracy # 0.8910351

# Precision, Recall, and F1 Score
precision <- TP / (TP + FP) # 0.6666667
recall <- TP / (TP + FN) # = True Positive Rates = sensitivity = recall = 0.1350157
F1 <- (2 )

# Accuracy does not take imbalanced classes into account, but F1 score does
# Imbalanced classes meaning very large number of 1 and very small number of 0










