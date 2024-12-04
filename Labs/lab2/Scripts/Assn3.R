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


#### TASK 3.1 ####
features<- data %>% select(-ViolentCrimesPerPop)

# Use caret to scale and center the data
scaler <- preProcess(features, method = c("center", "scale"))
data_scaled <- predict(scaler, data) #all scaled variables except ViolentCrimesPerPop

# Compute covariance matrix
cov_matrix <- cov(data_scaled)

# Perform PCA using eigen
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
n_commponets #35


#calculate the two most significant components, 
first_component_proportion <- eigen_percent[1]
first_component_proportion # 0.2502494
second_component_proportion <- eigen_percent[2]
second_component_proportion # 0.1693082

#### TASK 3.2 ####

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
  labs(title = "Trace Plot for First Principal Component (PC1)",
       x = "Features",
       y = "Contribution") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))  # Adjust font size for y-axis labels


# Identify the top 5 features with the largest absolute loadings
top5_features <- sort(abs(loadings), decreasing = TRUE)[1:5]
top5_feature_names <- names(top5_features)

# Print the top 5 features
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

#### Task 3.3 ####

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


# Train/fit the linear regression model, Violent crime per pop is the target variable, . add all columns - takes away columns.
fit <- lm(ViolentCrimesPerPop ~ . , data = train_scaled)
summary(fit)

# Calculate MSE for training data
pred_train_scaled <- predict(fit, train_scaled)  # get predictions for training data
mse_train_scaled <- mean((train_scaled$ViolentCrimesPerPop - pred_train_scaled)^2)  # calculate training MSE
mse_train_scaled # 0.2752071

# Calculate MSE for test data
pred_test_scaled <- predict(fit, test_scaled)  # get predictions for training data
mse_test_scaled <- mean((test_scaled$ViolentCrimesPerPop - pred_test_scaled)^2)  # calculate training MSE
mse_test_scaled # 0.4248011

#### Task 3.4 ####

train_MSE <- list()
test_MSE <- list()
k <-0

# Dynamically find the index of "ViolentCrimesPerPop"
target_col <- grep("ViolentCrimesPerPop", colnames(data.train.scaled))

# Prepare the training and test data
train <- as.matrix(data.train.scaled[, -target_col])  
test <- as.matrix(data.test.scaled[, -target_col])    
train_true <- as.matrix(data.train.scaled[, target_col])  
test_true <- as.matrix(data.test.scaled[, target_col])   

cost_function <- function(theta){
  
  .GlobalEnv$k <- .GlobalEnv$k+1
  .GlobalEnv$train_MSE[[k]] <- cost_train
  .GlobalEnv$test_MSE[[k]] <- cost_test
  return()
}











