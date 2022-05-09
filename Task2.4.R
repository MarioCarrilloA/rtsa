#Yu-Chi Huang
# Mario Carrillo

set.seed(1)

generate_Y = function(m, N, a, sigma){
  data = matrix(a + rnorm(N*m, 0, sigma), nrow=m)
  return(data)
}

Estimator_T = function(x){
  return (sum(x/(length(x)+1)))
}
#1=sample mean, 2=sample median, 3=T
Monte_Carlo_Experiment = function(a, sigma, m, N, Estimator){
  Y = generate_Y(m, N, a, sigma)
  aEst = A(Y, Estimator)

}

A = function(y, estimator){
  return(switch(estimator, apply(y, 1, mean), apply(y, 1, median), apply(y, 1, Estimator_T)))
}

mse = function(aEst, a){
  cat(aEst-a, '\n')
  return(mean((aEst-a)^2))
}

#a)
aEst_SampleMean = Monte_Carlo_Experiment(2, 2, 100, 25, 1)
hist(aEst_SampleMean, breaks = seq(0.375, 3.625, 0.25), main="Estimate a by sample mean", xlab="aEst")
bias_SampleMean = mean(aEst_SampleMean - 2)
#b)
aEst_SampleMedian = Monte_Carlo_Experiment(2, 2, 100, 25, 2)
hist(aEst_SampleMedian, breaks = seq(0.375, 3.625, 0.25), main="Estimate a by sample median", xlab="aEst")
bias_SampleMedian = mean(aEst_SampleMedian - 2)

aEst_T = Monte_Carlo_Experiment(2, 2, 100, 25, 3)
hist(aEst_T, breaks = seq(0.375, 3.625, 0.25), main="Estimate a by estimator T", xlab="aEst")
bias_T = mean(aEst_T - 2)

cat('bias of sample mean: ', bias_SampleMean, '\nbias of sample median: ', bias_SampleMedian, '\nbias of T: ', bias_T, '\n')

#c)
N = ceiling(10^seq(2, 5, 0.5))

