# Yu-Chi Huang
# Mario Carrillo

# Clean environment
if (!is.null(dev.list())) {
  dev.off()
}
rm(list=ls())

# Load dependencies
source('getmodels.R')

# This function computes multiple metrics
# to select the best model
measure_goodness_models = function(models, data){
  
  # Compute the largest model required by Mallow's Cp
  fit_largest <- lm(models[length(models)], data=data)
  d <- length(fit_largest$coefficients)
  N <- length(fit_largest$residuals)
  s_hat_square_max <- sum(residuals(fit_largest)^2)*(1/(N-d))
  
  # Initialize result arrays
  adjusted_R_square <- array(dim=(length(models)))
  residual_mean_square <- array(dim=(length(models)))
  Mallows_Cp <- array(dim=(length(models))) 
  
  for(i in 1:length(models)){
    fit <- lm(models[i], data=data)
    d <- length(fit$coefficients)
    N <- length(fit$residuals)
    residual_sum_squares <- sum(residuals(fit)^2)
    total_sum_squares <- sum((data$count-mean(data$count))^2)
    adjusted_R_square[i] <- 1-((residual_sum_squares/(N-d))/(total_sum_squares/(N-1)))
    residual_mean_square[i] <-  residual_sum_squares/(N-d)
    Mallows_Cp[i] <- residual_sum_squares/s_hat_square_max-(N-2*d)
    cat('Adjusted R^2: ', adjusted_R_square[i], ', ', models[i])
    cat('\nResidual Mean Square: ', residual_mean_square[i], ', ', models[i])
    cat('\nMallow\'s Cp: ', Mallows_Cp[i], ', ', models[i])
    cat('\n-----------------------\n')
  }
  cat('***Best results***\n')
  cat('Adjusted R^2: ', max(adjusted_R_square), ', ', models[which.max(adjusted_R_square)])
  cat('\nResidual Mean Square: ', min(residual_mean_square), ', ', models[which.min(residual_mean_square)])
  cat('\nMallow\'s Cp: ', min(Mallows_Cp), ', ', models[which.min(Mallows_Cp)])
  cat('\n-----------------------\n')
}

# Load data
data <- read.csv('E-Scooters.csv')

# Set variables according the E07 document
Y <- "count"
variables <- c("date",
          "temp",
          "atemp",
          "humidity",
          "windspeed",
          "registered")

# Use all combinations of the variables to
# formulate models
models <- getmodels(Y, variables)

# Convert the column date to a numeric format
data$date=as.numeric(as.POSIXct(data$date))

# First view of the data. The e-scooters rentals
# over time.
plot(x=data$date,
     y=data$count,
     xlab="date",
     ylab="count",
     main="date vs count",
     col='blue',
     pch=20)

measure_goodness_models(models, data)

# Analyze selected model
model <- lm(formula="count~date+temp+registered", data=data)
par(mfrow=c(2,2))
plot(model)
print(summary(model))

