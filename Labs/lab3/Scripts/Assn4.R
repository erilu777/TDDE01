
#### Install packages ####
install.packages("neuralnet")
library(neuralnet)

#Set SEED
set.seed(1234567890)

#### Assignment 4.1 ####
#Given code from assignment
Var <- runif(500, 0, 10)
Var
mydata <- data.frame(Var, Sin=sin(Var))
tr <- mydata[1:25,] # Training
te <- mydata[26:500,] # Test


# Random initialization of the weights in the interval [-1, 1]
winit <- runif(31, -1, 1) # Your code here

nn_logi <- neuralnet(Sin ~ Var, tr, hidden = 10, startweights = winit)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn_logi,te), col="red", cex=1)



#### Assignment 4.2 ####
# Neural network with the activation function to be linear f(x) = x
nn_linear <- neuralnet(Sin ~ Var, tr, hidden = 10, act.fct = function(x) x, startweights = winit)

#ReLU function
ReLU <- function (x){
  ifelse(x >= 0, x, 0) #in the report explain why we did  not use return or pmax()
}
# Neural network with the activation function to be ReLU f(x)=x,  ifelse(x >= 0, x, 0)
nn_ReLU <- neuralnet(Sin ~ Var, tr, hidden = 10, act.fct = ReLU, startweights = winit)

#Softplus function
softplus <- function(x){
  log(1+exp(x))
}
#Neural network with the activation function to be softplus f(x)= log(1+exp(x)
nn_softplus <- neuralnet(Sin ~ Var, tr, hidden = 10, act.fct = softplus, startweights = winit)

plot(tr, cex=2)
points(te, col = "blue", cex=2)
points(te[,1],predict(nn_linear,te), col="red", cex=1)
points(te[,1],predict(nn_ReLU,te), col="green", cex=1)
points(te[,1],predict(nn_softplus,te), col="orange", cex=1)

#### Assignment 4.3 ####

Var<- runif(500, 0, 50)

mydata2 <- data.frame(Var, Sin=sin(Var))
test2 <- mydata2[1:500,]

# Set up the plot with x-axis limits between 0 and 50
plot(tr, cex = 2, xlim = c(0, 50), ylim = c(-7, 2), main = "Sine Function Predictions", xlab = "Var", ylab = "Sin(Var)")

# Add test data points in blue
points(test2, col = "blue", cex = 1)

# Add predictions from the neural network in red
points(test2[, 1], predict(nn_logi, test2), col = "red", cex = 1)
abline(h = -6, col = "green", lwd = 2)

legend("bottomleft", legend = c("Training Data", "Test Data", "NN Predictions", "sin(var) = - 6"), 
       col = c("black", "blue", "red", "green"), pch = c(1, 1, 1), cex = 0.8)

#### Assignment 4.4 #####




