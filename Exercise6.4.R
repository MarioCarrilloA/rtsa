#Yu-Chi Huang
#Mario Carrillo

data <- read.table("mygardens.RData",header=T) 
#a)
plot(x=1:20, y=data$ozone, xlab='index', ylab='ozone')
legend('topright', legend=c('A', 'B'), col=c('red', 'blue'), lty=1, cex=0.5)
mean <- mean(data$ozone)
abline(h=mean)
a_sum <- 0
a_num <- 0
b_sum <- 0
b_num <- 0
for(x in 1:20){
  clip(x, x, data$ozone[x], mean)
  if(data$garden[x]=='A'){
    a_sum <- a_sum+data$ozone[x]
    a_num <- a_num+ 1
    abline(v=x, col='red')
  }
  else{
    b_sum <- b_sum+data$ozone[x]
    b_num <- b_num + 1
    abline(v=x, col='blue')
  }

}
a_mean <- a_sum/a_num
b_mean <- b_sum/b_num
a_sum_of_squares <- 0
b_sum_of_squares <- 0
colors <- c("#00AFBB", "#E7B800")
colors <- colors[data$garden]
plot(x=1:20, y=data$ozone, xlab='index', ylab='ozone')
abline(h=a_mean, col='red')
abline(h=b_mean, col='blue')
legend('topright', legend=c('A', 'B'), col=c('red', 'blue'), lty=1, cex=0.5)
for(x in 1:20){
  if(data$garden[x]=='A'){
    points(x=x, y=data$ozone[x], col='red')
    clip(x, x, data$ozone[x], a_mean)
    abline(v=x, col='red')
    a_sum_of_squares <- a_sum_of_squares + (data$ozone[x]-a_mean)^2
  }else{
    points(x=x, y=data$ozone[x], col='blue')
    clip(x, x, data$ozone[x], b_mean)
    abline(v=x, col='blue')
    b_sum_of_squares <- b_sum_of_squares + (data$ozone[x]-b_mean)^2
  }
}
#b)
mean <- mean(data$ozone)
sum_of_squares <- sum((data$ozone-mean)^2)
cat('sum of squares: ', sum_of_squares, '\na sum of squares: ', a_sum_of_squares, '\nb sum of squares: ', b_sum_of_squares)
DI <- a_sum_of_squares + b_sum_of_squares
R <- (20-2)/(2-1)*(sum_of_squares-DI)/DI
cat('\nR is F_(p-1, N-p)-distributed? ', R>qf(0.95, 2-1, 20-2), '\n\n')
#c)
result <- anova(lm(data$ozone~factor(data$garden)))
print(result)
