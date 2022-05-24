#Yu-Chi Huang
#Mario Carrillo

Polynomial_Regression = function(x, y, d, q, alpha){
  #degree=4
  fit1 = lm(y~poly(x,d))
  D = sum(fit1$residuals^2)
  lines(x, fit1$fitted.values, col='red')
  #degree=2
  fit2 = lm(y~poly(x, q))
  D0 = sum(fit2$residuals^2)
  lines(x, fit2$fitted.values, col='blue')
  
  N=length(y)
  R=(N-d)/(d-q)*(D0-D)/D
  f_alpha=qf((1-alpha), d-q, N-d)
  
  return(R > f_alpha)
}

data <- read.csv('Phd_important_data.csv')
d = 4
q = 2
alpha = 0.05
plot(data$time, data$data, xlab = 'Time', ylab = 'Data')
result = Polynomial_Regression(data$time, data$data, d, q, alpha)
if(result){
  cat('H0 is rejected')
}else{
  cat('H0 is not rejected')
}
