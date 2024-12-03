#### INSTALL NECESSARY PACKAGES ####
install.packages("tree")
library(tree)


#### TASK 2.1 ####
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


#### Task 2.2 ####

# Model 1 - Default fit
fit_default <- tree(y ~ ., data = train)
train_pred_default <- predict(fit_default, train, type = "class") # Prediction on train data
valid_pred_default <- predict(fit_default, valid, type = "class") # Prediction on validation data

# Calculate misclassification errors
train_mis_default <- mean(train_pred_default != train$y) # 0.1048441
valid_mis_default <- mean(valid_pred_default != valid$y) # 0.1092679


# Model 2 - Smallest allowed node size = 7000
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


# Model 3 - Minimum deviance = 0.0005
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


#### TASK 2.3 ####