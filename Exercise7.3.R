#Yu-Chi Huang
#Mario Carrillo

all_subset_regression = function(data){
    models_names <- getmodels('count', c('date', 'temp', 'atemp', 'humidity', 'windspeed', 'registered'))

    #get largest model for Mallow's Cp
    fit_largest = lm(models_names[length(models_names)], data = data)
    d = length(fit_largest$coefficients)-1
    N=length(fit_largest$residuals)
    s_hat_square_max = sum(residuals(fit_largest)^2)*(1/(N-d))
    adjusted_R_square <- residual_mean_square <- Mallows_Cp <- array(dim=(length(models_names)))

    for(i in 1:length(models_names)){
      fit = lm(models_names[i], data = data)
      d = length(fit$coefficients)-1
      N=length(fit$residuals)
      residual_sum_squares = sum(residuals(fit)^2)
      total_sum_squares = sum((data$count-mean(data$count))^2)
      adjusted_R_square[i] = 1-((residual_sum_squares/(N-d))/(total_sum_squares/(N-1)))
      #adjusted_R_square[i] = summary(fit)$adj.r.squared
      residual_mean_square[i] =  residual_sum_squares/(N-d)
      Mallows_Cp[i] = residual_sum_squares/s_hat_square_max-(N-2*d)
    }

    cat('Adjusted R^2: ', max(adjusted_R_square), ', ', models_names[which.max(adjusted_R_square)])
    cat('\nResidual Mean Square: ', min(residual_mean_square), ', ', models_names[which.min(residual_mean_square)])
    cat('\nMallow\'s Cp: ', min(Mallows_Cp), ', ', models_names[which.min(Mallows_Cp)])
}

data = read.csv("E-Scooters.csv")
data$date=as.numeric(as.POSIXct(data$date, format = "%Y-%m-%d"))/24/60/60

source('getmodels.r')

#adjusted R^2, residual mean square, Mallow's Cp
all_subset_regression(data)

#scatter plot
pairs(~date+temp+atemp+humidity+windspeed+registered+count, data=data, main='Scatterplot')

#boxplot
par(mfrow=c(2, 3))
for(colname in c('temp', 'atemp', 'humidity', 'windspeed', 'registered', 'count')){
  boxplot(data[colname], main=colname)
}

#correlation
for(colname in c('date', 'temp', 'atemp', 'humidity', 'windspeed', 'registered')){
  cat('\ncorrelation between count and ', colname, ': ', cor(data['count'], data[colname]))
}
