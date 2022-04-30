#Yu-Chi Huang
# Mario Carrillo

roll = function(n){
  coin <- c('heads', 'tails')
  result<-sample(coin, size = n, replace = TRUE)

  return(table(result)/length(result))
}

simulation= function(){
  times <- seq(10, 200, 10)
  #times <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  frequency_list<-list()
  f<- c()

  for(i in times){
    relative_frequency<-roll(i)
    
    writeLines('\nrelative frequency')
    cat(i, 'times\n')
    print(relative_frequency)

    frequency_list<-c(frequency_list, relative_frequency["heads"])
  }
  
  writeLines('\nthe relative frequency of 6 against N')
  t<-10
  for (k in frequency_list){
    cat(t, ' times: ',k, '\n')
    t<-t+10
    f<-c(f, k)
  }

  my_bar<-barplot(f, xlab = 'times', ylab = ' relative frequency', main = 'the relative frequency of heads against N', ylim=c(0, 1), names.arg=times)
  text(my_bar, f+0.03, round(f, digits = 2))
  #plot(times, frequency_list, xlab = 'times', ylab = ' relative frequency', main = 'the relative frequency of 6 against N', ylim=range(0,1), type = "o")
}

j=0
repeat{
  j=j+1
  cat('\n\nROUND ',j)
  simulation()
  if(j==3)
    break
}
