# We now fit a GAM to predict wage using natural spline functions of year and age, treating education as a qualitative predictors.
library(ISLR)
library(splines)
attach(Wage)
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
gam1 = lm(wage ~ns(year, 4) + ns(age, 5), education, data = Wage)
# In order to fit more general sorts of GAMs, using smoothing splines or other components that cannot expressed in terms of basis
# functions and then fit using least squares regression, we will need to use the gam library in R 
# The s() function, which is part of the gam library, is used to indicate that we would like to use a smoothing spline.
# We use the gam() function in order to fit a GAM using these components.
library(gam)
gam.m3 = gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
par(mfrow=c(1,3))
plot(gam.m3, se = TRUE, col = "blue")
# Even though gam1 is not of class gam but rather of class lm, we can still use plot.gam() on it.
plot.gam(gam1, se = TRUE, col = "red")
gam.m1 = gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 = gam(wage ~ year + s(age, 5) + education, data = Wage)
anova(gam.m1, gam.m2, gam.m3, test = "F")
preds = predict(gam.m2, newdata = Wage)

# We can also use local regression fits as building blocks in a GAM, using the lo() function
gam.lo = gam(wage ~ s(year, 4) + lo(age, span = 0.7) + education, data = Wage)
plot.gam(gam.lo, se = TRUE, col = "green")
# We can also use the lo() function to create iteractions before calling the gam() function.
gam.lo.i = gam(wage ~ lo(year, age, span = .5) + education, data = Wage)
# We can plot the resulting two-dimensional surface if we first install the akima package.
library(akima)
plot(gam.lo.i)

# In order to fit a logistic regression GAM, we once again use the I() function in constructing the binary 
# response variable, and set family = binomial
gam.lr = gam(I(wage>250)~year+s(age, df = 5) + education, family = binomial, data = Wage)
par(mfrow = c(1,3))
plot(gam.lr, se = T, col = "green")