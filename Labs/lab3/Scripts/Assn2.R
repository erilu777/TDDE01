#### LAB 3, ASSIGNMENT 2 ####

#### SET SEED ####
set.seed(1234567890)

#### Install necessary packages ####
install.packages("geosphere")

library(geosphere)

#### Code given in the lab ####
stations <- read.csv("stations.csv", fileEncoding = "latin1")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")
bandwidth_geo <- 50000 # distance in meters (..?)
bandwidth_date <- 5 # number of days
bandwidth_time <- 4 # number of hours

# The point to predict
latitude <- 65.63141 
longitude <- 22.02253 
interest_date <- as.Date("2015-01-15") 
prediction_hours <- c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24)

# the vectors that will be filled with predictions
pred_temperatures_sum <- vector(length=length(prediction_hours))
pred_temperatures_prod <- vector(length=length(prediction_hours))

# Ensure the `date` column is in Date format
st$date <- as.Date(st$date) 
# Filter the data to keep rows with dates on or before the interest date
filtered_data <- subset(st, date <= interest_date)

# Function used to calculate the gaussian
gaussian_kernel <- function(x, h){
  return(exp(-(x^2) / (2 * h^2)))
}

### Gaussian kernel for distance
geo_distances <- distHaversine(
  cbind(filtered_data$longitude, filtered_data$latitude),
  c(longitude, latitude)
)

K_geo <- gaussian_kernel(geo_distances, bandwidth_geo)

### Gaussian kernel for date
date_distances <- (as.numeric(difftime(interest_date, filtered_data$date, units = "days")))
K_date <- gaussian_kernel(date_distances, bandwidth_date)

# Extract hours from the time strings in filtered_data
filtered_data$hour <- as.numeric(substr(filtered_data$time, 1, 2))

# Calculate predictions for each hour we want to predict
for(i in 1 : length(prediction_hours)){
  target_hour <- prediction_hours[i] # The hour we're trying to predict

  # Calculate time differences
  time_distances <- abs(filtered_data$hour - target_hour)
  # for example, 23 and 01 should have a 2 hour difference, not 23-1=22
  time_distances <- pmin(time_distances, 24 - time_distances)  
  
  # Calculate time kernel
  K_time <- gaussian_kernel(time_distances, bandwidth_time)
  
  # Combine kernels by summing
  K_sum_combined <- K_geo + K_date + K_time
  
  # Combine kernels by multiplying
  K_prod_combined <- K_geo * K_date * K_time
  
  # Calculate weighted average temperatures (both methods)
  pred_temperatures_sum[i] <- sum(K_sum_combined * filtered_data$air_temperature) / sum(K_sum_combined)
  pred_temperatures_prod[i] <- sum(K_prod_combined * filtered_data$air_temperature) / sum(K_prod_combined)
  
  # Product prediction lower, more "strict" (needs to be close in all directions)
  cat("Time:", target_hour, "- Sum:", round(pred_temperatures_sum[i], 4), 
      "°C, Product:", round(pred_temperatures_prod[i], 4), "°C\n")
}

### PLOT THE KERNEL VALUE AS A FUNCTION OF DISTANCE

# Create sequence of distances/differences to plot
geo_seq <- seq(0, 120000, length.out=100)  # 0 to 100km
date_seq <- seq(0, 14, length.out=100)     # 0 to 14 days
time_seq <- seq(0, 12, length.out=100)     # 0 to 12 hours

# Calculate kernel values
geo_kernel_values <- gaussian_kernel(geo_seq, bandwidth_geo)
date_kernel_values <- gaussian_kernel(date_seq, bandwidth_date)
time_kernel_values <- gaussian_kernel(time_seq, bandwidth_time)

# Create plots
par(mfrow=c(1,3))  # 1 row, 3 columns of plots

# Geographic distance kernel
plot(geo_seq/1000, geo_kernel_values, type="l", 
     xlab="Geographic Distance (km)", ylab="Kernel Value",
     main="Geographic Kernel")

# Date distance kernel
plot(date_seq, date_kernel_values, type="l",
     xlab="Date Distance (days)", ylab="Kernel Value",
     main="Date Kernel")

# Time distance kernel
plot(time_seq, time_kernel_values, type="l",
     xlab="Time Distance (hours)", ylab="Kernel Value",
     main="Time Kernel")


# EXTRA, not part of lab but to see the and compare to actual values
# close in geographical distance and date:
# Sort all measurements by distance to our point
st$distance_to_lulea <- distHaversine(
  cbind(st$longitude, st$latitude), 
  c(22.02253, 65.63141)
)

# Find stations close to Luleå (within 100km)
nearby_stations <- subset(st, distHaversine(cbind(longitude, latitude), c(22.02253, 65.63141)) < 100000)

# Look at measurements near our date of interest (within 2 weeks)
nearby_data <- subset(nearby_stations, 
                      abs(as.numeric(difftime(date, as.Date("2015-01-15"), units="days"))) < 14)

# Print relevant information
print("Nearby measurements:")
print(nearby_data[, c("station_number", "station_name", "date", "time", "air_temperature")])

# Looking at this data, the product predictions seem better,
# the sum predictions guessed too high
