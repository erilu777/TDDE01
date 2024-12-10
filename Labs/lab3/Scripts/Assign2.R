
#### SET SEED ####
set.seed(1234567890)

#### Install necessary packages ####
install.packages("geosphere")

library(geosphere)

#### Code given in the lab ####
stations <- read.csv("stations.csv", fileEncoding = "latin1")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")
h_distance <- 50000 # distance in meters
h_date <- 7 #days
h_time <- 5 #hours
latitude <- 65.63141 # The point to predict (OLivers home in Luleå)
longitude <- 22.02253 #

interest_date <- as.Date("2015-01-15") # Oliver remebers this day as cold as hell)
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", 
           "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")

temp <- vector(length=length(times))


# Students’ code here

# Ensure the `date` column is in Date format
st$date <- as.Date(st$date) 
# Filter the data to keep rows with dates on or before the interest date
filtered_data <- subset(st, date <= interest_date)


# Gaussian kernel for distance
distances <- distHaversine(
  cbind(filtered_data$longitude, filtered_data$latitude),
  c(longitude, latitude)
)
K_distance <- exp(-(distances^2) / (h_distance^2))

# Gaussian kernel for date
date_difference <- (as.numeric(difftime(filtered_data$date, interest_date, units = "days")))
K_date <- exp(-(date_difference^2) / (h_date^2))

# Gaussian kernel for time (should probably be changed)

# Convert times in the dataset to hours past midnight
filtered_data$hour <- as.numeric(substr(filtered_data$time, 1, 2)) +
  as.numeric(substr(filtered_data$time, 4, 5)) / 60

# Convert times of interest to hours
prediction_hours <- c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24)


# Example for one prediction time (adjust for all times in a loop)
time_difference <- abs(filtered_data$hour - prediction_hour)





plot(temp, type="o")

