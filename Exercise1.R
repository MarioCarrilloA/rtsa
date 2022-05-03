# Yu-Chi Huang
# Mario Carrillo

# Clean env
rm(list=ls())

flip <- function(N) {
  coin <- c('heads', 'tails')
  result<-sample(coin, size = N, replace = TRUE)

  return(table(result)/length(result))
}

# Simulate flip coin
simulation <- function(e) {
  times <- seq(10, 200, 10)
  frequency_list<-list()
  f <- c()

  for(i in times) {
    relative_frequency <- flip(i)
    writeLines('\nrelative frequency')
    cat(i, 'times\n')
    print(relative_frequency)
    frequency_list <- c(frequency_list, relative_frequency["heads"])
  }

  writeLines('\nThe relative frequency of heads against N')
  t <- 10
  for(k in frequency_list) {
    cat(t, ' times: ',k, '\n')
    t <- t + 10
    f <- c(f, k)
  }

  out_img <- sprintf("E1_experiment_%d.png", e)
  png(file=out_img,  width=800, height=400, pointsize=10,)
  my_bar<-barplot(f,
                  xlab = 'N times',
                  ylab = 'Relative frequency',
                  main = 'The relative frequency of heads against N',
                  ylim=c(0, 1),
                  col = rgb(0.2,0.4,0.6,0.6),
                  names.arg=times)
  text(my_bar, f + 0.03, round(f, digits = 2))
  dev.off()
}

# Repeat experiment 3 Times
for(e in 1:3) {
  simulation(e)
  print(sprintf("Experiment: %d", e))
}
