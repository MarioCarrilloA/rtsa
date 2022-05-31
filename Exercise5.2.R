#Yu-Chi Huang
#Mario Carrillo

calculate_RSS <- function(linear_model){
  predictive_residuals <- residuals(linear_model)/(1-lm.influence(linear_model)$hat)
  RSS <- sum(predictive_residuals^2)
  return(RSS)
}

calculate_R_squared <- function(linear_model, names){
  cat('Adjusted R_squared:\n')
  for (i in 1:15){
    cat(names[i],': ', summary(linear_model[[i]])$adj.r.squared, '\n')
  }
}

calculate_rms <- function(linear_models, names){
  cat('\nResidual mean-square:\n')
  d <- c(2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5)
  for (i in 1:15){
    cat(names[i],': ', deviance(linear_models[[i]])/(43-d[i]), '\n')
  }
}

calculate_Mallows_Cp <- function(linear_models, names, full_model){
  cat('\nMallows Cp:\n')
  for (i in 1:15){
    cat(names[i],': ', ols_mallows_cp(linear_models[[i]], full_model), '\n')
  }
}
Backward_Elimination <- function(data){
  set.seed(123)
  train_control <-trainControl(method = 'cv', number = 10)
  step_model <- train(BSAAM ~ ., data = data,
                      method = 'leapBackward',
                      tuneGrid = data.frame(nvmax = 1:4),
                      trControl = train_control)
  cat('\nBackward Elimination\n')
  print(step_model$results)
  print(step_model$bestTune)
  print(summary(step_model$finalModel))
}

library(olsrr)
library(tidyverse)
library(caret)
library(leaps)
load('Precipitation.rdata')
print(colnames(Precipitation[2:5]))
source('getmodels.r')

models_names <- getmodels('BSAAM', colnames(Precipitation[2:5]))

mylm <- lm(Precipitation$BSAAM ~ Precipitation$APSAB + Precipitation$APSLAKE)
mylm2 <- lm(Precipitation$BSAAM ~ Precipitation$APSAB + Precipitation$OPRC)
models <- list(lm(Precipitation$BSAAM ~ Precipitation$APSAB),
            lm(Precipitation$BSAAM ~ Precipitation$APSLAKE),
            lm(Precipitation$BSAAM ~ Precipitation$OPRC),
            lm(Precipitation$BSAAM ~ Precipitation$OPSLAKE),
            lm(Precipitation$BSAAM ~ Precipitation$APSAB + Precipitation$APSLAKE), 
            lm(Precipitation$BSAAM ~ Precipitation$APSAB + Precipitation$OPRC),
            lm(Precipitation$BSAAM ~ Precipitation$APSAB + Precipitation$OPSLAKE),
            lm(Precipitation$BSAAM ~ Precipitation$APSLAKE + Precipitation$OPRC),
            lm(Precipitation$BSAAM ~ Precipitation$APSLAKE + Precipitation$OPSLAKE),
            lm(Precipitation$BSAAM ~ Precipitation$OPRC + Precipitation$OPSLAKE),
            lm(Precipitation$BSAAM ~ Precipitation$APSAB + Precipitation$APSLAKE + Precipitation$OPRC),
            lm(Precipitation$BSAAM ~ Precipitation$APSAB + Precipitation$APSLAKE + Precipitation$OPSLAKE),
            lm(Precipitation$BSAAM ~ Precipitation$APSAB + Precipitation$OPRC + Precipitation$OPSLAKE),
            lm(Precipitation$BSAAM ~ Precipitation$APSLAKE + Precipitation$OPRC + Precipitation$OPSLAKE),
            lm(Precipitation$BSAAM ~ Precipitation$APSAB + Precipitation$APSLAKE + Precipitation$OPRC + Precipitation$OPSLAKE))

# print(summary(models[[1]])$adj.r.squared)
# calculate_R_squared(models)
# n <- matrix(0, nrow = 15)
# for (i in 0:15){
#   cat(m[i],': ', n[i], '\n')
# }
# d <- c(2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5)
# print(d[1])
summary(mylm2)$adj.r.squared
deviance(mylm2)
calculate_R_squared(models, models_names)
calculate_rms(models, models_names)
full_model <- lm(Precipitation$BSAAM ~ ., data = Precipitation)
calculate_Mallows_Cp(models, models_names, full_model)
Backward_Elimination(Precipitation)
