# Lab 3 block 1 of 732A99/TDDE01/732A68 Machine Learning
# Author: jose.m.pena@liu.se
# Made for teaching purposes

################################################################################

# This template is from the file Lab3Block1_2021_SVMs_St.R
# ALL COMMENTS ARE MADE OF US STUDENTS IN ORDER TO FULLY UNDERSTAND THIS SCRIPT!

# TASK: Classify mail as spam/nonspam using Support Vector Machines

# Data Setup
install.packages("kernlab")
library(kernlab)
set.seed(1234567890)
data(spam) # spam dataset comes with kernlab package

# Shuffle the data 
foo <- sample(nrow(spam))
spam <- spam[foo,]

# Split the data
tr <- spam[1:3000, ]    # train: first 3000 observations
va <- spam[3001:3800, ] # validation: next 800 observations
trva <- spam[1:3800, ]  # combined train+validation: first 3800 observations
te <- spam[3801:4601, ] # test: last 801 observations

# Idea: 
# 1. Use tr to fit models with different C values (complexities). 
# 2. Select best model using va (min validation error)
# 3. Use te as a independent test set to estimate generalization error of chosen model

by <- 0.3
err_va <- NULL

# Trying different values of C, from 0.3 - 5.0 in increments of 0.3
for(i in seq(by,5,by)){
  
  # Fit a SVM (=filter) on train set, using Radial Basis Function kernel (rbfdot) with fixed width=0.05
  filter <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=i,scaled=FALSE) # Different C
  
  # Predict on validation set
  mailtype <- predict(filter,va[,-58]) # Target variable "type" in column 58 (=spam/nonspam)
  
  # Confusion matrix
  t <- table(mailtype,va[,58])
  
  # Validation error = (FP+FN)/n
  err_va <-c(err_va,(t[1,2]+t[2,1])/sum(t))
}

# err_va contains validation errors for each C (16 values)
err_va # [0.30375 0.22375 0.20125 0.17375 0.16750 0.16500 0.16625 0.16875 0.17000 0.16875 0.16750 0.16750 0.16875 0.16750 0.16750 0.16750]

min_err_va_pos <- which.min(err_va)  # Position 6 in the array
min_err_va <- err_va[min_err_va_pos] # 0.16500
C_min_err_va <- min_err_va_pos * by  # 1.8

# Select best model SVM with C = 1.8

# filer0 = SVM with C = 1.8 (based on minimum validation error = 0.16500 when C=1.8)
# Fitted on train dataset, prediction on validation dataset
filter0 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter0,va[,-58])
t <- table(mailtype,va[,58])
err0 <- (t[1,2]+t[2,1])/sum(t)
err0 # 0.16500 (obviously)

# filter1 = same model, now prediction on test
filter1 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter1,te[,-58])
t <- table(mailtype,te[,58])
err1 <- (t[1,2]+t[2,1])/sum(t)
err1 # 0.1672909

# Idea: Now that we've selected best C, train on more data to (hopefully) get a better model
# filter2 = same C, fitted on train+validation, prediction on test
filter2 <- ksvm(type~.,data=trva,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter2,te[,-58])
t <- table(mailtype,te[,58])
err2 <- (t[1,2]+t[2,1])/sum(t)
err2 # 0.1498127

# filter3 = same C, fitted on all data, prediction on test
filter3 <- ksvm(type~.,data=spam,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter3,te[,-58])
t <- table(mailtype,te[,58])
err3 <- (t[1,2]+t[2,1])/sum(t)
err3 # 0.01373283




# Questions

# 1. Which filter do we return to the user ? filter0, filter1, filter2 or filter3? Why?
# Answer: filter2 - because best generalization error, and predicted on independent dataset. With C chosen from training+validation.
# filter3 lower error, however tested on same data as it has been fitted to. Overfitting + biased gerneralization error.

# 2. What is the estimate of the generalization error of the filter returned to the user? err0, err1, err2 or err3? Why?
# Answer: err2 - generalization error estimate from a model not trained on test data. Corresponds to testing filter2.

# 3. Implementation of SVM predictions.




# Goal: Understand how an SVM makes predictions and replicate this process manually.
# i.e., Replicate what predict() does "under the hood"

# Theory: Once a SVM has been fitted to the training data, a new point is essentially classified
# according to THE SIGN of a linear combination of the KERNEL FUNCTION VALUES between the
# support vectors and the new point.

# From lab instructions:
# alphaindex: Return the indexes of the support vectors
# coef: The linear coefficients for the support vectors
# b: The negative intercept of the linear combination

# Extract constants from the filter3 model
sv <- alphaindex(filter3)[[1]] # Index of Support Vectors of model filter3
co <- coef(filter3)[[1]]       # Coefficients for each Support Vector
inte <- - b(filter3)           # Intercept of the linear combination (b() in kernlab reutrns negative intercept)
k <- NULL                      # Kernel function

# SVM with kernel K classifies new point x_new based of the sign of this function:
# f(x_i) = sum_j(a_j * K(sv_j, x_i)) + intercept, where a_j = coefficients for SV j

# Predict first 10 points in spam dataset
for(i in 1:10){ # We produce predictions for just the first 10 points in the dataset.
  k2 <- NULL
  
  # Compute the kernel values BETWEEN each support vector and the new point
  # The new point being spam[i,-58] meaning row i, column 58 (target variable = type)
  x_new <- spam[i, -58] # x_new is the new point
  
  # Iterate through all Support Vectors
  for(j in 1:length(sv)){
    # Define support vector
    sv_x <- spam[sv[j], -58] # Support Vector on index j
    
    # Convert data frame rows into numeric vectors 
    # rbfkernel() function requires this
    sv_x <- as.numeric(sv_x)
    x_new <- as.numeric(x_new)
    
    # We need the kernel function value K(sv_x, x_new)
    # The kernel is RBF (Radial Basis Function) with sigma=0.05
    # Define RBF kernel function:
    rbfkernel <- rbfdot(sigma = 0.05)
    
    # Compute K(sv_x, x_new)
    k_val <- rbfkernel(sv_x, x_new)
    
    # Multiply by the corresponding coefficient
    k2 <- c(k2, k_val * co[j]) 
  }
  # Sum over all Support Vector and add intercept
  k <- c(k, sum(k2) + inte)
}

# k now holds the decision value for the first 10 points in spam dataset
k
predict(filter3,spam[1:10,-58], type = "decision")

# The two lines above indeed has the same output!
