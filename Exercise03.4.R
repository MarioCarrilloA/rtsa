# Yu-Chi Huang
# Mario Carrillo

# Clean environment
rm(list=ls())


make_plots <- function(id) {
  # Load data
  filename <- sprintf("data%d.txt", id)
  data <- read.table(filename, sep=",")
  x <- data$V1
  Y <- data$V2

  # Fit model
  a <- lm(Y~x)

  # Create output directory
  out_dir <- "./outs"
  dir.create(file.path(out_dir), showWarnings = FALSE)

  # Out PDF
  out_pdf <- sprintf("%s/data%d.pdf", out_dir, id)
  pdf(file=out_pdf, width=12, height=8)

  # Out PNG image (NOTE: At the moment PDF and PNG cannot work
  # at the same time. Therefore, if you uncomment this part, it
  # is necessary to comment PDF part).
  #
  #out_img <- sprintf("%s/data%d.png", out_dir, id)
  #png(file=out_img, width=1200, height=800, pointsize=20)

  # Plot the 4 charts in a 2x2 grid
  par(mfrow=c(2,2))


  # Scatter plot
  plot(x, Y, xlab="x", ylab="Y", main="Scatter Plot")
  t<-seq(, 1, 0.1)
  abline(a, col="red")

  # Residual plot vs fitted
  plot(a, c(1, 1))

  # Q-Q Plot
  plot(a, c(2, 2))

  # Hat matrix vs squared residuals
  plot(a$residuals^2,
       lm.influence(a)$hat,
       main="Squared residuals vs Hii",
       xlab="Residuals^2",
       ylab="Leverage points (Hii)",
       xlim=c(min(a$residuals^2), max(a$residuals^2)),
       ylim=c(min(lm.influence(a)$hat), max(lm.influence(a)$hat)))

  # Title
  mtext(filename,
        side= 3,
        line=-3,
        cex=2,
        outer=TRUE,
        col="blue")
  dev.off()
}

for(id in seq(1:5)) {
  make_plots(id)
}

