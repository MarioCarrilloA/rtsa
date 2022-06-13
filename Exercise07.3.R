# Yu-Chi Huang
# Mario Carrillo

# Clean environment
if (!is.null(dev.list())) {
  dev.off()
}
rm(list=ls())

create_formulas <-function(Y, vars) {
  size <- 1:length(vars)
  # Get all combinations from our vector vars
  # and convert results to vector
  all_combinations <- unlist(
      lapply(size, combn, x=vars, simplify=FALSE),
      recursive=FALSE)

  # Empty list
  formulas <- list()
  for (combination in all_combinations) {
    i <- 1
    formula <- Y
    for(var in combination) {
      if (i == 1) {
        formula <- paste(formula, var, sep="~")
      } else {
        formula <- paste(formula, var, sep="+")
      }
      i <- i+1
    }
    formulas <- append(formulas, formula)
  }
  return(formulas)
}


# Load data
data <- read.csv('E-Scooters.csv')

# Set variables according the E07 document
Y <- "count"
vars <- c("date",
          "temp",
          "atemp",
          "humidity",
          "windspeed",
          "registered")

# Create formulas according to the combination
# between our variables
formulas <- create_formulas(Y, vars)

# Convert the column date to a numeric format
data$date=as.numeric(as.POSIXct(data$date))

# Fit models and compute metrics
for (f in formulas) {
  # Fit model
  model <- lm(formula=f, data=data)

  # Get adjusted R square
  adjrsqr <- summary(model)$adj.r.squared

  # Get residual mean square
  d=length(model$coefficients)
  N=length(model$residuals)
  rms <- sum(model$residuals^2)/(N-d)

  # print results
  print(sprintf("%s  ARS:%f RMS:%f",f, adjrsqr, rms))

}

