#Yu-Chi Huang
#Mario Carrillo

generate_Y = function(a, b, m, N, sigma){
  x = seq(1/N, 1, 1/N)
  data = matrix(rep(a*x+b, m) + rnorm(N*m, 0, sigma), nrow=m)
  return(data)
}

Monte_Carlo_Experiment = function(alpha, a, b, m, N, sigma){
  y = generate_Y(a, b, m, N, sigma)
  x = seq(1/N, 1, 1/N)
  return(apply(y, 1, Test, x=x, alpha=alpha))
}

Test = function(x, y, alpha){
  fit = lm(y~x)
  Test_Decision = summary(fit)$coef[2,4] <= 0.05 #TRUE=Reject. FALSE=Choose
  return(Test_Decision)
}

Number_of_Correct_Desion = function(alpha, a, b, m, N, sigma){
  decisions = Monte_Carlo_Experiment(alpha, a, b, m, N, sigma)
  if(b==0){
    return(sum(!decisions))#correctly chose H0
  }else{
    return((sum(decisions)))#correctly rejected H0
  }
}

#a)
Monte_Carlo_Experiment(0.05, 1, 0, 1, 5, 2)
Monte_Carlo_Experiment(0.05, 0.8, 0.2, 1, 5, 2)
Monte_Carlo_Experiment(0.05, 0, 1, 1, 5, 2)

#b)
N = seq(5, 50, 1)
m = 100
Result1 = sapply(N, Number_of_Correct_Desion, alpha=0.05, a=1, b=0, m=100, sigma=2)
Result2 = sapply(N, Number_of_Correct_Desion, alpha=0.05, a=0.8, b=0.2, m=100, sigma=2)
Result3 = sapply(N, Number_of_Correct_Desion, alpha=0.05, a=0, b=1, m=100, sigma=2)

#c)
plot(N, (100-Result1)/100, xlab = 'N', ylab = ' Empirical Level and Empirical Power', ylim=c(0,1), col='red')
points(N, Result2/100, col='green')
points(N, Result3/100, col='blue')
legend('topright', legend=c('Wrongly Rejected H0(b=0)', 'Corectly Rejected H0(b=0.2)', 'Corectly Rejected H0(b=1)'), col = c('red', 'green', 'blue'))
