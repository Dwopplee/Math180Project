# Some basic setup stuff

# Make a training subset to use
train = sample(1000, 750)

# Add our libraries
# Boosted trees
library(gbm)
# GAMs
library(gam)
# Laplace for noise
library(distr)
# For plotting ROC curves
library(ROCR)

laplace = DExp()

# Shared functions

# Function for making a GAM
# Pass in training data
# Pass in family -- either gaussian or binomial
make.gam.fit = function(train, family = gaussian) {
  last.pred = ncol(train) - 1
  
  ss.dfs = rep(NA, last.pred)
  for (i in 1:last.pred) {
    ss.fit = smooth.spline(train[, i], train[, ncol(train)])
    ss.dfs[i] = ss.fit$df
  }
  
  splines.forms = cbind(rep("s(x", last.pred), 1:last.pred, rep(", df = ", last.pred), ss.dfs, rep(")", last.pred))
  
  splines = rep(NA, last.pred)
  
  for(i in 1:last.pred) {
    splines[i] = paste(splines.forms[i, ], collapse = "")
  }
  
  formula = paste(c("y ~", paste(splines, collapse = " + ")), collapse = " ")
  
  formula = as.formula(formula)
  
  return(gam(formula, data = train, family = family))
}

# Makes a GAM and 2 boosted trees
# 1 tree has interaction depth d = 1
# Other has d = 2
make.fits = function(train) {
  gam.fit = make.gam.fit(train)
  bt1.fit = gbm(y ~ ., data = train, distribution = "gaussian", n.trees = 1000, cv.folds = 4)
  bt2.fit = gbm(y ~ ., data = train, distribution = "gaussian", n.trees = 1000, cv.folds = 4, 
                interaction.depth = 2)
  
  return(list(gam.fit, bt1.fit, bt2.fit))
}

make.class.fits = function(train) {
  gam.fit = make.gam.fit(train, binomial)
  bt1.fit = gbm(y ~ ., data = train, n.trees = 1000, cv.folds = 4)
  bt2.fit = gbm(y ~ ., data = train, n.trees = 1000, cv.folds = 4, interaction.depth = 2)
  
  return(list(gam.fit, bt1.fit, bt2.fit))
}

# Returns mean squared error of model 
# Pass model in as fit
# Pass test-set in as test
#   Response should be last column in test
mse = function(fit, test) {
  fitted = predict(fit, newdata = test)
  return(mean((fitted - test[, ncol(test)])^2))
}

# Returns percentage of accurate predictions
# Pass in model as fit
# Pass in test-set as test
#   Response should be last column in test
acc = function(fit, test) {
  fitted = predict(fit, newdata = test, type = 'response')
  fitted = ifelse(fitted > 0.5, 1, 0)
  
  return(length(which(fitted == test[, ncol(test)])) / length(fitted))
}

# Added ROC curve code too late to utilize properly
# Not really sure how it works/how to interpret
# Not going to include in draft; should be in final
evaluate.class = function(fit, test, main, plot = FALSE) {
  if (plot) {
    fitted = predict(fit, newdata = test, type = 'response')
    fitted = ifelse(fitted > 0.5, 1, 0)
    pred = prediction(fitted, test[, ncol(test)])
    perf = performance(pred, "tpr", "fpr")
    plot(perf, colorize = TRUE, main = main)
  }
  
  return(acc(fit, test))
}

# Scatter plot of predictions (on x) vs truth (on y)
# y = x (ideal result) in red as comparison
# fit and test are same as mse
# main, xlab, and ylab correspond to graph labels
plot.preds = function(fit, test, main, xlab, ylab) {
  fitted = predict(fit, newdata = test)
  plot(fitted, test[, ncol(test)], xlab = xlab, ylab = ylab, main = main)
  abline(0, 1, col = 'red')
}

# Does both mse and plot.preds
# Takes the same inputs as plot.preds
evaluate = function(fit, test, main, xlab, ylab, plot = FALSE) {
  if (plot) {
    plot.preds(fit, test, main, xlab, ylab)  
  }
  return(mse(fit, test))
}

# Applies evaluate over a list of fits
# Also plots best possible outcome
# Returns vector of fit MSEs and best MSE
# fits is a list of fits
# test is the test set data
#   the response should be the last column
# truth is the vector of test-set predictions made by the true model
# mains, xlabs, and ylabs are character vectors corresponding to graph labels
evaluate.models = function(fits, test, truth, mains, xlabs, ylabs, plot = FALSE) {
  if (plot) {
    grid.size = ceiling(sqrt(length(fits) + 1))
    par(mfrow = c(grid.size, grid.size))
    plot(truth, test[, ncol(test)], main = "True Model", xlab = "Fitted Values", 
         ylab = "Response Values")
    abline(0, 1, col = 'red')  
  }
  
  mses = mapply(evaluate, fit = fits, main = mains, xlab = xlabs, ylab = ylabs, 
                USE.NAMES = FALSE, MoreArgs = list(test = test, plot = plot))
  
  true.mse = mean((truth - test[, ncol(test)])^2)
  return(c(mses, true.mse))
}


# Applies acc over a list of fits
# Returns vector of fit accuracies and best accuracy
# fits is a list of fits
# test is the test set data
#   the reponse should be the last column
# truth is the vector of responses without noise
evaluate.class.models = function(fits, test, truth, plot = FALSE, mains) {
  if (plot) {
    grid.size = ceiling(sqrt(length(fits) + 1))
    par(mfrow = c(grid.size, grid.size))
    print(length(truth))
    print(length(test[, ncol(test)]))
    pred = prediction(truth, test[, ncol(test)])
    perf = performance(pred, "tpr", "fpr")
    plot(perf, colorize = TRUE, main = "True Model")
  }
  
  accs = mapply(evaluate.class, fits, main = mains, MoreArgs = list(test = test, plot = plot),
                USE.NAMES = FALSE)
  
  true.acc = length(which(truth == test[, ncol(test)])) / length(truth)
  
  return(c(accs, true.acc))
}
