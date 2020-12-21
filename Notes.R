# Wipe the environment
rm(list = ls(all = TRUE))

# Enable usage of our functions
source("Functions.R")

# Base case in one dimension - smoothing splines

# Make some data - linear

ss.lin.x = rnorm(1000)
# y linearly related to x
ss.lin.y = ss.lin.x + rnorm(1000)

# Fit a smoothing spline, cross validating for smoothing parameter lambda
ss.lin.fit = smooth.spline(ss.lin.x, ss.lin.y)

# Plot data
plot(ss.lin.x, ss.lin.y, xlab = "x", ylab = "y", main = "Linear", col = 'grey')
# Plot the smoothing spline in red
lines(ss.lin.fit, col = 'red', lwd = 2)
# Plot the true model in black dashed line
abline(0, 1, lty = 2, lwd = 2)

# Make some data - cubic

ss.cub.x = rnorm(1000, 1)
# y is a 3rd degree polynomial
ss.cub.y = 0.25 * ss.cub.x^3 + 0.5 * ss.cub.x^2 + ss.cub.x + rnorm(1000, sd = 2)

# Fit a smoothing spline, cross validating for smoothing parameter lambda
ss.cub.fit = smooth.spline(ss.cub.x, ss.cub.y)

# Plot data
plot(ss.cub.x, ss.cub.y, xlab = "x", ylab = "y", main = "Cubic", col = 'grey')
# Plot the smoothing spline in red
lines(ss.cub.fit, col = 'red', lwd = 2)
# Plot the true model in black dashed line
curve(0.25 * x ^ 3 + 0.5 * x ^ 2 + x, add = TRUE, lwd = 2, lty = 2)

# Combine previous models

gam.2.y = ss.lin.x + 0.25 * ss.cub.x^3 + 0.5 * ss.cub.x^2 + ss.cub.x + rnorm(1000)

# Fit a GAM of the previous smoothing splines on training data

# Use previous degrees of freedom
ss.cub.df = ss.cub.fit$df
ss.lin.df = ss.lin.fit$df
gam.2.data = as.data.frame(cbind(ss.lin.x, ss.cub.x, gam.2.y))
names(gam.2.data) = c("lin.x", "cub.x", "y")
gam.2.train = gam.2.data[train, ]
gam.2.test = gam.2.data[-train, ]
gam.2.fit = gam(y ~ s(lin.x, df = 2.3) + s(cub.x, df = 7.2), data = gam.2.train)

# Plot the generated model for the linear term and the true model
plot.Gam(gam.2.fit, se = TRUE, terms = "s(lin.x, df = 2.3)", col = 2, xlab = "x", ylab = "y")
abline(0, 1, lty = 2, lwd = 2)
# Do the same for the cubic term
plot.Gam(gam.2.fit, se = TRUE, terms = "s(cub.x, df = 7.2)", col = 2, xlab = "x", ylab = "y")
curve(0.25 * x ^ 3 + 0.5 * x ^ 2 + x, add = TRUE, lwd = 2, lty = 2)

# We can fit a boosted tree to the same data, though probably poorly
bt.2.fit = gbm(y ~ lin.x + cub.x, data = gam.2.train, distribution = "gaussian", n.trees = 1000, cv.folds = 5)

# Plot the effects of each variable (alone) on output
plot(bt.2.fit, i = "lin.x", xlab = "x")
plot(bt.2.fit, i = "cub.x", xlab = "x")

# Plot GAM against true values with noise
plot(predict(gam.2.fit, newdata = gam.2.test), gam.2.y[-train], xlab = "Fitted Values", ylab = "Response Values")
abline(0, 1, col = 2)

# Plot GAM against true model (no noise)
plot(predict(gam.2.fit, newdata = gam.2.test), ss.lin.x[-train] + 0.25 * ss.cub.x[-train]^3
     + 0.5 * ss.cub.x[-train]^2 + ss.cub.x[-train], xlab = "Fitted Values", ylab = "True Values")
abline(0, 1, col = 2)

# MSE of GAM, boosted tree, and true model
gam.2.err = mean((predict(gam.2.fit, newdata = gam.2.test) - gam.2.y[-train])^2)
bt.2.err = mean((predict(bt.2.fit, newdata = gam.2.test) - gam.2.y[-train])^2)
true.2.err = mean(((ss.lin.x[-train] + 0.25 * ss.cub.x[-train]^3 + 0.5 * ss.cub.x[-train]^2
       + ss.cub.x[-train]) - gam.2.y[-train])^2)

plot(predict(bt.2.fit, newdata = gam.2.test), gam.2.y[-train], xlab = "Fitted Values", ylab = "Response Values")
abline(0, 1, col = 2)
plot(predict(bt.2.fit, newdata = gam.2.test), ss.lin.x[-train] + 0.25 * ss.cub.x[-train]^3
     + 0.5 * ss.cub.x[-train]^2 + ss.cub.x[-train], xlab = "Fitted Values", ylab = "True Values")
abline(0, 1, col = 2)

