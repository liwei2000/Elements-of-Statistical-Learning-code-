library(splines)
library(ISLR)
# regression splines can be fit by constructing an appropriate matrix of basis functions.
# The bs() function generate the entire matrix of basis functions for splines with the specified set of knots.
attach(Wage)
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
fit = lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
pred = predict(fit, newdata = list(age = age.grid), se = T)
plot(age, wage, col = 'gray')
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2*pred$se, lty = 'dashed')
lines(age.grid, pred$fit - 2*pred$se, lty = 'dashed')

dim(bs(age, knots = c(25, 40, 60)))
# we could also use the df option to produce a spline with knots at uniform quantiles of the data
dim(bs(age, df = 6))
attr(bs(age, df = 6), 'knots')
# The bs() function also has a degree argument, so we can fit splines of any degree, rather than the default degree of 3

# In order to instead fit a natural spline, we use the ns() function. 
# Here we fit a natural spline with four degrees of freedom
fit2 = lm(wage ~ ns(age, df = 4), data = Wage)
pred2 = predict(fit2, newdata = list(age = age.grid), se = T)
lines(age.grid, pred2$fit, col = 'red', lwd = 2)

# In order to fit a smoothing spline, we use the smooth.spline() function
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey") 
title("Smoothing Spline")
fit = smooth.spline(age, wage, df = 16)
fit2 = smooth.spline(age, wage, cv = TRUE)
fit2$df 
lines(fit, col = 'red', lwd = 2)
lines(fit2, col = "blue", lwd = 2)
# When we specify df = 16, the function determine which value of lambda leads to 16 degrees of freedom. In the second call to 
#	smooth.spline, we select the smoothness level by cross-validation
# In order to perform local regression, we use the loess() function 
plot(age, wage, xlim = agelims, cex = .5, col = 'darkgrey')
title('Local Regression')
fit = loess(wage ~ age, span = .2, data = Wage)
fit2 = loess(wage ~ age, span = .5, data = Wage)
lines(age.grid, predict(fit, data.frame(age = age.grid)), col = "red", lwd  = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)), col = "blue", lwd = 2)
legend('topright', legend = c("Span = 0.2", "Span = 0.5"), col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)