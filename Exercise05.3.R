# Yu-Chi Huang
# Mario Carrillo

# Clean environment
if (!is.null(dev.list())) {
  dev.off()
}
rm(list=ls())


# Uncomment to create PNG output image
#########################################################
#out_dir <- "./outs"
#out_img <- sprintf("%s/e5.3_a.png", out_dir)
#dir.create(file.path(out_dir), showWarnings = FALSE)
#png(file=out_img, width=800, height=600, pointsize=20)
#########################################################

# Load data, fit model and plot them
load(file="steam_temperature.RData", d <- new.env())
d$steam.temp
x <- d$steam.temp$temperature
Y <- d$steam.temp$steam
m <- lm(Y~x)
plot(x,
     Y,
     xlab="AVG atmospheric temperature (F°)",
     ylab="Steam (punds)",
     main="E5.3 temperature vs steam",
     col.main = "purple",
     col="blue")
abline(m, col="red")

new_data = c(0)
temp_increment <- 5
# It seems that R does not work with index=0 :(, then we need +1
for (i in 1:(37 - 26 + 1)) {
  cat(sprintf("%d %d %f\n", i, i + 13, x[i + 13]+5))
  new_data[i] <- x[i + 13] + temp_increment
}

# Convert it to data.frame for easier handling
new_x <- data.frame(x=new_data)
new_x

# Make predictions and plot them with confidence intervals
predlm <- predict(m, newdata=new_x, interval="confidence", level=0.95)
prd <- predlm[,1]
lwr <- predlm[,2]
upr <- predlm[,3]
points(new_data, prd, col="blue", pch=20)
points(new_data, upr, col="magenta", pch=2)
points(new_data, lwr, col="magenta", pch=6)

# Calculate the mean of predictions and plot it
predlm_mean <- mean(predlm)
lx <- new_data
ly <- rep(predlm_mean, length(new_data)) # Repeat mean value per xpos
lines(lx, ly, lty="dotted", col="brown")

# Calculate confidence band with scheffe
library(lava)
bands <- scheffe(m, new_x)
upr_band <- bands[,2]
lwr_band <- bands[,3]
lines(new_data, upr_band, col="green", lty=2)
lines(new_data, lwr_band, col="green", lty=2)

# Legends
par(cex = 0.6)
legend("bottomleft",
      legend=c("Dataset",
               "Predictions",
               "Upper confidence Interval",
               "Lower confidence Interval",
               "Model",
               "Estimated mean",
               "Confidence bands"),
      pch=c(1, 20, 2, 6, NA, NA, NA),
      lty=c(NA, NA, NA, NA, 1, 2, 2),
      col=c("blue",
            "blue",
            "magenta",
            "magenta", "red",
            "brown",
            "green"))

# Uncomment to create PNG output image
#dev.off()