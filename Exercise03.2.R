#Yu-Chi Huang
#Mario Carrillo
library(ellipse)

data <- read.csv('children.csv') 
splited_data <- matrix(unlist(strsplit(data$age.weight, split = ';')), ncol = 2, byrow = T)
colnames(splited_data) <- c('age', 'weight')
d <- data.frame(age=splited_data[, 'age'], weight=splited_data[, 'weight'])
x=as.numeric(d$age)
y=as.numeric(d$weight)

fit <- lm(y ~ x, data=d)
b1 = fit$coefficients[1]
b2 = fit$coefficients[2]
plot(ellipse::ellipse(fit, center = c(b1, b2), level=0.95), type="l", col="blue", xlab="b1", ylab="b2")

points(b1, b2, pch=5)
abline(v=b1, h=b2, col='gray')
