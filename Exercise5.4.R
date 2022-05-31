#Yu-Chi Huang
#Mario Carrillo

data = read.csv('data_sheet05.csv')
x = data$X
y = data$Y
N = length(x)

#a)
fit = lm(y ~ x+log(x))
new = data.frame(x=4.5)
predicted_x = predict(fit, new)
cat('predicted value for x=4.5: ', predicted_x)

#b)
x_matrix = matrix(c(rep(1, N), x, log(x)), ncol=3)
xt_times_x = solve(t(x_matrix) %*% x_matrix)
a = c(1, 4.5, log(4.5))
sigma_squared = var(fit$residuals)
s_beta_squared = sigma_squared*(t(a)%*%xt_times_x%*%a)
lower_bound = predicted_x - sqrt(3*qf(0.95, 3, N-3)*s_beta_squared)
upper_bound = predicted_x + sqrt(3*qf(0.95, 3, N-3)*s_beta_squared)
cat('\nuppder bound for predicted value: ', upper_bound)
cat('\nlower bound for predicted value: ', lower_bound)

#c) d)
lower_confidence_band <- function(x, xtx, sigmasquared, d, q, N){
  a = c(1, x, log(x))
  sbetasquared = sigmasquared*(t(a)%*%xtx%*%a)
  fgamma = qf(0.95, q, N-d)
  betahat = predict(fit, data.frame(x=x))
  return(betahat-sqrt(q*fgamma*sbetasquared))
}

upper_confidence_band <- function(x, xtx, sigmasquared, d, q, N){
  a = c(1, x, log(x))
  sbetasquared = sigmasquared*(t(a)%*%xtx%*%a)
  fgamma = qf(0.95, q, N-d)
  betahat = predict(fit, data.frame(x=x))
  return(betahat+sqrt(q*fgamma*sbetasquared))
}

d=3
q=3
plot(x, y, xlab = 'x', ylab = 'y')
lines(x, fitted.values(fit), col='blue')
lines(x, sapply(x, lower_confidence_band, xtx=xt_times_x, sigmasquared=sigma_squared, d=d, q=q, N=N), lty=2, col='green')
lines(x, sapply(x, upper_confidence_band, xtx=xt_times_x, sigmasquared=sigma_squared, d=d, q=q, N=N), lty=2, col='green')
points(4.5, predicted_x, col='red')
segments(x0=4.5, y0=upper_bound, x1=4.5, y1=lower_bound, col='pink', lty=3)
points(4.5, upper_bound, col='pink', lty=0)
points(4.5, lower_bound, col='pink', lty=0)
