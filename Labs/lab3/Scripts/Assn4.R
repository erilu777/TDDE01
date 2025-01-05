
#### Install packages ####
install.packages("neuralnet")
library(neuralnet)

#Set SEED
set.seed(1234567890)

#### Assignment 4.1 ####

#Given code from assignment
Var <- runif(500, 0, 10)
mydata <- data.frame(Var, Sin=sin(Var))
tr <- mydata[1:25,] # Training
te <- mydata[26:500,] # Test
# Random initialization of the weights in the interval [-1, 1]
set.seed(1234567890)
winit <- runif(31,-1,1)
set.seed(1234567890)

nn_logi <- neuralnet(Sin ~ Var, tr, hidden = 10, startweights = winit, act.fct = "logistic")
# Plot of the training data (black), test data (blue), and predictions on the test data (red)
print(nn_logi$weights)
plot(tr, cex=2, main="Training Data, Test Data, and NN Predictions")
points(te, col = "blue", cex=1)
points(te[,1],predict(nn_logi,te), col="red", cex=1)
legend("bottomleft", 
       legend = c("Training Data", "Test Data", "NN Predictions"), 
       col = c("black", "blue", "red"), 
       pch = 1, 
       cex = 0.8)

print(nn_logi$weights)
nn_logi
#### Assignment 4.2 ####
# Neural network with the activation function to be linear f(x) = x
nn_linear <- neuralnet(Sin ~ Var, tr, hidden = 10, act.fct = function(x) x, startweights = winit)

#ReLU function
ReLU <- function (x){
  ifelse(x >= 0, x, 0) #in the report explain why we did  not use return or pmax()
}
# Neural network with the activation function ReLU f(x)=x,  ifelse(x >= 0, x, 0)
nn_ReLU <- neuralnet(Sin ~ Var, tr, hidden = 10, act.fct = ReLU, startweights = winit)

#Softplus function
softplus <- function(x){
  log(1+exp(x))
}
# Neural network with the activation function softplus f(x)= log(1+exp(x)
nn_softplus <- neuralnet(Sin ~ Var, tr, hidden = 10, act.fct = softplus, startweights = winit)

plot(tr, cex=2, ylim = c(-1, 1.5), main="Predictions of NN models with Linear, ReLU, and Softplus\n activation functions, compared to Training and Test data"
)
points(te, col = "blue") # Test data points
points(te[,1], predict(nn_linear, te), col="red") # Linear model predictions
points(te[,1], predict(nn_ReLU, te), col="green") # ReLU model predictions
points(te[,1], predict(nn_softplus, te), col="orange") # Softplus model predictions
legend("bottomleft", 
       legend = c("Training Data", "Test Data", "Linear", "ReLU", "Softplus"), 
       col = c("black", "blue", "red", "green", "orange"), 
       pch = 1, cex = 0.8)



#### Assignment 4.3 ####
set.seed(1234567890)
Var<- runif(500, 0, 50)

mydata2 <- data.frame(Var, Sin=sin(Var))
test2 <- mydata2

# Set up the plot with x-axis limits between 0 and 50
plot(tr, cex = 1, xlim = c(0, 50), ylim = c(-12, 1.5), main = "Sine Function Predictions", xlab = "Var", ylab = "Sin(Var)")

# Add test data points in blue
points(test2, col = "blue", cex = 1)

# Add predictions from the neural network in red
points(test2[, 1], predict(nn_logi, test2), col = "red", cex=1)
abline(h = -7.5, col = "green", lwd = 2)

legend("bottomleft", legend = c("Training Data", "Test Data", "NN Predictions 
on test data", "sin(var) = - 7.5"), 
       col = c("black", "blue", "red", "green"), pch = c(1, 1, 1), cex = 0.8)

#### Assignment 4.4 ####
# Print the weights
print(nn_logi$weights)

nn_logi

# Look at the weights

#### Assignment 4.5 ####

# Sample points
set.seed(1234567890)  # Reset seed for reproducibility
Var <- runif(500, 0, 10)

# Create data frame but now sin(x) is input and x is output!
mydata_inverse <- data.frame(Var = Var, Sin = sin(Var))

# Train neural network with flipped relationship
# Sin ~ Var (from earlier) becomes Var ~ Sin
nn_inverse <- neuralnet(Var ~ Sin, mydata, startweights = winit, hidden = 10, threshold = 0.1)

# Plot results
plot(mydata_inverse$Sin, mydata_inverse$Var, 
     main = "Predicting x from sin(x)",
     xlab = "sin(x)",
     ylab = "x",
     col = "blue",
     cex = 0.7)
points(mydata_inverse$Sin, predict(nn_inverse, mydata_inverse), 
       col = "red",
       cex = 0.7)
legend("bottomleft",
       legend = c("Actual", "Predicted"),
       col = c("blue", "red"),
       pch = 1,
       cex = 0.8)








