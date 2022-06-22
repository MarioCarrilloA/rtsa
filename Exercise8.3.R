#Yu-Chi Huang
#Mario Carrillo

data = read.csv('security_log.csv')
for(i in data){
  s = as.numeric(stringr::str_replace_all(unlist(stringr::str_extract_all(i, "[0-9]{1};")), ';', ''))
  c = as.numeric(unlist(stringr::str_extract_all(i, "[0-9]{2}")))
}
data=data.frame('System_breakdown'=s, 'Cyber_attacks'=c)

#a)+b)
linear_regression = lm(data$System_breakdown~data$Cyber_attacks)
plot(x = data$Cyber_attacks, y=data$System_breakdown, xlab=' number of cyber attacks', ylab='fitted values for system failure')
abline(linear_regression)

logistic_regression = glm(data$System_breakdown~data$Cyber_attacks, family='binomial')
plot(x = data$Cyber_attacks, logistic_regression$fitted, pch=16, col='green', xlab=' number of cyber attacks', ylab='fitted values for system failure')
points(x = data$Cyber_attacks, y=data$System_breakdown)

#c)
estimate_probability <- function(x){
  e_u = exp(logistic_regression$coefficients[1]+logistic_regression$coefficients[2]*x)
  probability = e_u/(1+e_u)
  return(probability)
}
points(26, estimate_probability(26), pch=0, col='red')
lines(c(1:40), sapply(1:40, estimate_probability))
