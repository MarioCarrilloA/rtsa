#Yu-Chi Huang
#Mario Carrillo

data_s06 <- read.csv("data_s06.csv")
x <- data_s06$x
y <- data_s06$y
# number of training examples
n <- length(y)


plot(x, y)


gradientDesc <- function(x, y, learn_rate, threshold, n, max_iter) {
  b2 <- 0
  b1 <- 0
  yhat <- exp(b2 * x) + b1
  MSE <- sum((y - yhat) ^ 2) / n
  converged = F
  iterations = 0
  while(converged == F) {
    ## Implement the gradient descent algorithm
    b2_new <- b2 - learn_rate * ((1 / n) * (sum((yhat - y)* x)))
    b1_new <- b1 - learn_rate * ((1 / n) * (sum(yhat - y)))
    b2 <- b2_new
    b1 <- b1_new
    #cat(iterations, ' ', b1, ' ', b2, '\n')
    yhat <- exp(b2 * x) + b1
    MSE_new <- sum((y - yhat) ^ 2) / n
    #cat('NEW MSE: ', MSE_new, ' MSE: ', MSE, '\n')
    #cat('MSE-MSE_new: ', MSE-MSE_new, '\n\n')
    if(MSE_new < MSE){
      if(MSE-MSE_new <= threshold) {
        lines(x, b1+exp(b2*x), col = "red")
        converged = T
        cat("b1:", b1, "b2:", b2)
        sigma_square <- sum( ( (b1 + exp(b2*x)) - y)^2 )/(n)
        cat("\nvariance:" , sigma_square)
      }
      else
        MSE = MSE_new
    }
    
    iterations = iterations + 1
    if(iterations > max_iter) { 
      lines(x, b1+exp(b2*x), col = "red")
      converged = T
      print('---')
      cat("Optimal intercept:", b1, "Optimal slope:", b2, '\n')
    }
  }
}


gradientDesc(x, y, 0.01, 0.00001, n, 1000)
