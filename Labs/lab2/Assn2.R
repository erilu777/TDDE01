#### INSTALL NECESSARY PACKAGES ####
install.packages("tree")
library(tree)
#### DIVIDE THE DATA ####

# Load the data into a variable
data <- read.csv("bank-full.csv", header = T, sep=";") #read.csv2 takes the separator as ; as defult

data <- data[, !names(data) %in% c("duration")]

# Get the number of rows in the dataset
n <- dim(data)[1]

# Set a random seed for reproducibility
set.seed(12345)

# Partition 50% of the data for the training set
id <- sample(1:n, floor(n * 0.4))
train <- data[id, ]

# Partition 25% of the data for the validation set
id1 <- setdiff(1:n, id)
set.seed(12345)
id2 <- sample(id1, floor(n * 0.3))
valid <- data[id2, ]

# Use the rest for the test set
id3 <- setdiff(id1, id2)
test <- data[id3, ]

#### Task 2.2 ####
#Default fit
fit_default <- tree(as.factor(y) ~ ., data = train)
train_pred_default <- predict(fit_default, train, type = "class")
valid_pred_default <- predict(fit_default, valid, type = "class")

train_mis_default <- mean(train_pred_default != train$y) # 0.1142446
valid_mis_default <- mean(valid_pred_default != valid$y) # 0.1207697



