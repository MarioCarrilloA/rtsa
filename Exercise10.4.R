#Yu-Chi Huang
#Mario Carrillo

#a)
Rho1 = function(b){
  return(b/(1+b^2))
}
b= seq(-5, 5, 0.2)
plot(b, Rho1(b))
lines(b, Rho1(b))

#b)
Epsilon = rnorm(501, 0, 1)
Epsilon_t = Epsilon[2:501]
Epsilon_i = Epsilon[1:500]*0.5
Epsilon_ii = Epsilon[1:500]*(-0.5)
par(mfrow=c(1,2))
plot(1:500, Epsilon_t+Epsilon_i, ylim = c(-4, 4), main='b=0.5')
plot(1:500, Epsilon_t+Epsilon_ii, ylim = c(-4, 4), main='b=-0.5')

#c)
Epsilon_ci = Epsilon[1:500]*1/3
Epsilon_cii = Epsilon[1:500]*3
par(mfrow=c(1,2))
plot(1:500, Epsilon_t+Epsilon_ci, ylim = c(-10, 10), main='b=1/3')
plot(1:500, Epsilon_t+Epsilon_cii, ylim = c(-10, 10), main='b=3')
print(Rho1(1/3))
print(Rho1(3))
