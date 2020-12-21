# Wipe the environment
rm(list = ls(all = TRUE))

source("Functions.R")

# Function for making dataset with 3 depth-2 interactions
# noise is a vector of noise with length 1000, default standard normal
make.int.data = function(noise = rnorm(1000)) {
  data = as.data.frame(replicate(10, rnorm(1000)))
  names(data) = c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10")
  
  coeffs = runif(7, -2, 2)
  
  truth = coeffs[1] * data$x1 * data$x2 + coeffs[2] * data$x3 * data$x4 + 
    coeffs[3] * data$x5 * data$x6 + coeffs[4] * data$x7 + coeffs[5] * data$x8 + 
    coeffs[6] * data$x9 + coeffs[7] * data$x10
  y = truth + noise
  
  quartiles = quantile(y, prob = c(.25, .75))
  split = runif(1, quartiles[1], quartiles[2])
  
  true.classes = ifelse(truth > split, 1, 0)
  classes = ifelse(y > split, 1, 0)
  
  return(cbind(data, y, truth, classes, true.classes))
}

# Combine functions to run a full trial
# Generates data and models for the data
# Makes plots
# Returns mean squared errors of each model
run.int.test = function(noise = rnorm(1000)) {
  data = make.int.data()
  
  true.classes = data[, ncol(data)]
  truth = data[, (ncol(data) - 2)]
  
  class.data = data[, c(1:(ncol(data) - 4), ncol(data) - 1)]
  names(class.data)[ncol(class.data)] = "y"
  
  data = data[, 1:(ncol(data) - 3)]
  
  fits = make.fits(data[train, ])
  class.fits = make.class.fits(class.data[train, ])
  
  mains = c("GAM", "Boosted Tree - Interaction Depth 1", "Boosted Tree - Interaction Depth 2")
  xlabs = rep("Fitted Values", 3)
  ylabs = rep("Response Values", 3)
  
  errors = evaluate.models(fits, data[-train, ], truth[-train], mains, xlabs, ylabs)
  class.errors = evaluate.class.models(class.fits, class.data[-train, ], true.classes[-train],
                                       mains = mains)
  
  return(list(errors, class.errors))
}

# Function for running the actual tests for a given noise distribution
# Pass in the number of trials as trials
# Pass in the noise (from a distribution) as noise
# Pass in the intended plot title as main
# Outputs plots
# Returns a list of information on errors:
#   mean of regression errors
#   variance of regression errors
#   mean of misclassification percentages
#   variance of misclassification percentages
run.int.trials = function(trials, noise = rnorm(1000), main) {
  errors = replicate(trials, run.int.test(noise = noise))
 
  reg.error = matrix(rep(NA, trials * 4), ncol = trials)
  class.error = matrix(rep(NA, trials * 4), ncol = trials)
  
  for (i in 1:trials) {
    for (j in 1:4) {
      reg.error[j, i] = errors[[2 * i - 1]][j]
      class.error[j, i] = errors[[2 * i]][j]
    }
  }
  
  mean.reg.error = apply(reg.error, 1, mean)
  var.reg.error = apply(reg.error, 1, var)
  
  mean.class.error = apply(class.error, 1, mean)
  var.class.error = apply(class.error, 1, var)
  
  error.info = list(mean.reg.error, var.reg.error, mean.class.error, var.class.error)
  
  plot(reg.error[1, ], type = 'b', xlab = "Trial", ylab = "Error", main = main,
       ylim = c(0, max(reg.error)))
  for (i in 2:4) {
    points(reg.error[i, ], type = 'b', col = i)
  }
  
  plot(class.error[1, ], type = 'b', xlab = "Trial", ylab = "Accuracy", main = main,
       ylim = c(0, max(class.error)))
  for (i in 2:4) {
    points(class.error[i, ], type = 'b', col = i)
  }
  
  return(error.info)
}

# Actual investigative work

# This will also make a ton of plots, I guess

norm.errors = run.int.trials(20, main = "Gaussian Noise")
laplace.errors = run.int.trials(20, noise = r(laplace)(1000), main = "Laplace Noise")
cauchy.errors = run.int.trials(20, noise = rcauchy(1000), main = "Cauchy Noise")




