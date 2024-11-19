# ASSIGNMENT 3
# Logistic regression and basis function expansion

# Load necessary library
library(ggplot2)

# Load the data
data <- read.csv("pima-indians-diabetes.csv")

# Rename the columns
colnames(data) <- c("Pregnancies", "PlasmaGlucoseConcentration", "BloodPressure", "SkinfoldThickness",
                    "Insulin", "BMI", "DiabetesPedigreeFunction", "Age", "Diabetes")


#### Assignment 1.1 ####
# Scatterplot of diabetes observations in a Glucose vs Age graph

# Create the scatterplot
ggplot(data, aes(x = Age, y = PlasmaGlucoseConcentration, color = factor(Diabetes))) + # Define x/y-axes
  geom_point(alpha = 0.6) +  # Add points with 40% transparency
  labs( # Labels for the plot
    title = "Plasma Glucose Concentration vs Age",
    x = "Age",
    y = "Plasma Glucose Concentration",
    color = "Actual Diabetes"
  ) +
  theme_minimal() +  # Clean theme
  scale_color_manual(values = c("green", "red"), labels = c("No", "Yes")) # Customize color/label for Diabetes


#### Assignment 1.2 ####
# Predictions of Diabetes based on a logistic regression model trained with Glucose & Age as features

# Train the logistic regression model
logistic_model <- glm(Diabetes ~ PlasmaGlucoseConcentration + Age, data = data, family = binomial)

# Make predictions for all observations!

# Predicted probabilities for target variable Diabetes
data$Predicted_Probabilities <- predict(logistic_model, type = "response") # Values between 0 and 1
# Convert predicted probabilities into binary classifications based on threshold = 0.5
data$Predicted_Classifications <- ifelse(data$Predicted_Probabilities > 0.5, 1, 0) # Values either 0 or 1

# Probabilistic equation of the estimated model
coefficients <- coef(logistic_model) # Extract the coefficients
intercept <- coefficients[1]
coef_glucose <- coefficients[2]
coef_age <- coefficients[3]
cat("Logistic Regression - Probabilistic Equation:\n")
cat(sprintf("logit(P(y = 1)) = %.4f + %.4f * Glucose + %.4f * Age\n", 
            intercept, coef_glucose, coef_age)) # Print the result

# Training misclassification error
error <- mean(data$Predicted_Classifications != data$Diabetes)
cat(sprintf("\nTraining Misclassification Error: %.2f%%\n", error * 100)) # Print the result

# Plot the predicted Diabetes values based on Glucos & Age
ggplot(data, aes(x = Age, y = PlasmaGlucoseConcentration, color = factor(Predicted_Classifications))) + # Define x/y-axes
  geom_point(alpha = 0.6) +  # Add points with 40% transparency
  labs( # Labels for the plot
    title = "Plasma Glucose Concentration vs Age",
    x = "Age",
    y = "Plasma Glucose Concentration",
    color = "Predicted Diabetes"
  ) +
  theme_minimal() +  # Clean theme
  scale_color_manual(values = c("green", "red"), labels = c("No", "Yes")) # Customize color/label for Diabetes


#### Assignment 1.3 ####
# Decision boundary of the estimated logistic regression model

# The line where probability of y=1 is equal to 0.5 <==> logit function = 0
# logit(P(y=1)) = w0 + w1*Glucose + w2*Age = 0 <==> Age = -(w0/w2)-(w1*Glucose)/w2

# Function to calculate decision boundary between classes Glucose & Age
decision_boundary <- function(glucose) {
  -(intercept/coef_age)-(coef_glucose/coef_age)*glucose
}

# Define Glucose range for boundary line
glucose_range <- data$PlasmaGlucoseConcentration # Use all Glucose values
age_boundary <- decision_boundary(glucose_range)

# Add the decision boundary in predicted Diabetes plot
ggplot(data, aes(x = Age, y = PlasmaGlucoseConcentration, color = factor(Predicted_Classifications))) + # Define x/y-axes
  geom_point(alpha = 0.6) +  # Add points with 40% transparency
  # Decision boundary line
  geom_line(aes(x = age_boundary, y = glucose_range), color="blue", linetype="dashed", size=0.5) +
  labs( # Labels for the plot
    title = "Plasma Glucose Concentration vs Age",
    x = "Age",
    y = "Plasma Glucose Concentration",
    color = "Predicted Diabetes"
  ) +
  xlim(min(data$Age), max(data$Age)) + # Restrict the x-axis to the observed range of Age
  theme_minimal() +  # Clean theme
  scale_color_manual(values = c("green", "red"), labels = c("No", "Yes")) # Customize color/label for Diabetes


#### Assignment 1.4 ####
# Plotted graphs based on different r-values

# Convert predicted probabilities into binary classifications based on threshold = 0.2
data$Predicted_Classifications_02 <- ifelse(data$Predicted_Probabilities > 0.2, 1, 0) # Values either 0 or 1
# Convert predicted probabilities into binary classifications based on threshold = 0.8
data$Predicted_Classifications_08 <- ifelse(data$Predicted_Probabilities > 0.8, 1, 0) # Values either 0 or 1

# Plot r = 0.2
ggplot(data, aes(x = Age, y = PlasmaGlucoseConcentration, color = factor(Predicted_Classifications_02))) + # Define x/y-axes
  geom_point(alpha = 0.6) +  # Add points with 40% transparency
  labs( # Labels for the plot
    title = "Plasma Glucose Concentration vs Age",
    x = "Age",
    y = "Plasma Glucose Concentration",
    color = "Predicted Diabetes"
  ) +
  theme_minimal() +  # Clean theme
  scale_color_manual(values = c("green", "red"), labels = c("No", "Yes")) # Customize color/label for Diabetes

# Plot r = 0.8
ggplot(data, aes(x = Age, y = PlasmaGlucoseConcentration, color = factor(Predicted_Classifications_08))) + # Define x/y-axes
  geom_point(alpha = 0.6) +  # Add points with 40% transparency
  labs( # Labels for the plot
    title = "Plasma Glucose Concentration vs Age",
    x = "Age",
    y = "Plasma Glucose Concentration",
    color = "Predicted Diabetes"
  ) +
  theme_minimal() +  # Clean theme
  scale_color_manual(values = c("green", "red"), labels = c("No", "Yes")) # Customize color/label for Diabetes


#### Assignment 1.5 ####

