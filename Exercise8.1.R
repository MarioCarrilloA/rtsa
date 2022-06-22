#Yu-Chi Huang
#Mario Carrillo

data = read.csv('osteoarthritis_treatment.csv')
for(i in data){
  time = as.numeric(stringr::str_replace_all((unlist(stringr::str_extract_all(i, ";[0-9]{2}"))), ';', ''))
}
data_male = time[c(TRUE, FALSE)]
data_female = time[c(FALSE, TRUE)]

#a)
data_A = time[1:10]
data_B = time[11:20]
data_C = time[21:30]
#equal variance
boxplot(data_A, data_B, data_C)
summary(data_A)
summary(data_B)
summary(data_C)
data = data.frame('time'=time, 'treatment' = c(rep('A', 10), rep('B', 10), rep('C', 10)))
fit <- lm(data$time~data$treatment, data=data)
plot(fit, 1)
bartlett.test(data$time~data$treatment)
#normality
hist(data_A)
hist(data_B)
hist(data_C)
ggpubr::ggqqplot(data, 'time', facet.by = 'treatment')

#c) + d)
p=3
q=2
m=5
N=p*q*m

Y_bar = sum(time)/N
#treatment
Y_A = sum(time[1:10])/(q*m)
Y_B = sum(time[11:20])/(q*m)
Y_C = sum(time[21:30])/(q*m)
Y_treatment_bar = c(Y_A, Y_B, Y_C)
D1 = q*m*sum((Y_treatment_bar-Y_bar)^2)
#gender
Y_male = sum(data_male)/(p*m)
Y_female = sum(data_female)/(p*m)
Y_gender_bar = c(Y_male, Y_female)
D2 = p*m*sum((Y_gender_bar-Y_bar)^2)
#R
Y_gender_bar = rep(c(Y_male, Y_female), 15)
Y_treatment_bar = c(rep(Y_A, 10), rep(Y_B, 10), rep(Y_C, 10))
DR = sum((time-Y_treatment_bar-Y_gender_bar+Y_bar)^2)
R1 = (N-p-q+1)/(p-1)*D1/DR
if(R1>qf(0.95, p-1, N-p-q+1)){
  print('There is an influence of the treatment on the time to pain relief.')
}else{
  print('NOT significant')
}
R2 = (N-p-q+1)/(q-1)*D2/DR
if(R1>qf(0.95, q-1, N-p-q+1)){
  print('There is an influence of the gender on the time to pain relief.')
}else{
  print('NOT significant')
}

#b)
Y_ij = c(rep(c(sum(data_male[1:5])/m, sum(data_female[1:5])/m), 5), rep(c(sum(data_male[6:10])/m, sum(data_female[6:10])/m), 5), rep(c(sum(data_male[11:15])/m, sum(data_female[11:15])/m), 5))
D12 = sum((time-Y_ij)^2)
R = (N-p*q)/(p*q-p-q+1)*(DR-D12)/D12
if(R>qf(0.95, (p-1)*(q-1), N-p*q)){
  print('There is an interaction between treatment and gender.')
}else{
  print('NO interaction')
}
