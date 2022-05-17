# Yu-Chi Huang
# Mario Carrillo

# Clean environment
rm(list=ls())

filename <- "wavy.txt"
data <- read.csv(filename, sep=",")
x <- data$x
Y <- data$y

a <-lm(Y ~ cos(x) + sin(x))
a$coefficients