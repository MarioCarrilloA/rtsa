# Uncomment to install latex2exp package
#install.packages("latex2exp")

# Clean environment
if (!is.null(dev.list())) {
  dev.off()
}
rm(list=ls())

library("latex2exp")

plot_indicator <- function(N, alpha, delta){
  # Uncomment to create PNG output image
  #########################################################
  #out_dir <- "./outs"
  #img_name <- sprintf("e_%d_%.2f_%f",N, alpha, delta)
  #out_img <- sprintf("%s/%s.png", out_dir, img_name)
  #dir.create(file.path(out_dir), showWarnings = FALSE)
  #png(file=out_img, width=800, height=600, pointsize=20)
  #########################################################
  
  # Init k to N range
  k <- seq(1:N)
  # Adjust chart margins
  minf1 <- abs(1^{alpha})
  maxf1 <- abs(N^{alpha})
  maxf2 <- delta * sqrt(N)
  minf2 <- delta * sqrt(1)
  ylim <-c(min(minf1/2, minf2/2), max(maxf1, maxf2))
  
  # Write title 
  t <- TeX(
    sprintf(
    r'(Indicator function: $k=$%d, $\alpha=$%.2f, $\delta=%f$)',
    N, alpha, delta))
  
  plot(k,
       abs(k^{alpha}),
       col="red",
       xlab=TeX(r'($k$)'),
       ylab="functions", 
       type='l',
       main=t,
       ylim=ylim)
  lines(k, delta * sqrt(k), type="l", col="blue")
  
  # Legends
  par(cex = 0.8)
  legend("topright",
         legend=c(TeX(r'($k^{\alpha}$)'),
                  TeX(r'($\delta \sqrt{k}$)')),
         lty=c(1, 1),
         col=c("red", "blue"))
  
  # Uncomment to create PNG output image
  #dev.off()
}

# alpha higher 1/2
plot_indicator(N=10000, alpha=1, delta=0.5)

# alpha equal to 1/2
plot_indicator(N=10000, alpha=0.5, delta=0.5)

# alpha inside interval
plot_indicator(N=100, alpha=0.25, delta=0.5)

# alpha equal to 0
plot_indicator(N=100, alpha=0, delta=0.4)

# alpha lower than 0
plot_indicator(N=100, alpha=-0.5, delta=0.5)

