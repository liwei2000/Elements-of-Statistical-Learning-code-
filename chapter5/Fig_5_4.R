# Fitting nonlinear logistic regression models to the South African heart disease data.

# read data
setwd("/Users/fanfan/github/Elements of Statistical Learning code/chapter5/")
library(splines)
Data <- read.csv("South African Heart Disease.data.txt")
Data <- Data[, -1]

# We explore nonlinearities in the functions using natural splines
# Using four natural spline bases for each term in the model
fitFull <- glm(chd ~ ns(sbp, df = 4)+ns(tobacco, df = 4)+ns(ldl, df = 4)+ns(adiposity, df = 4)
	+ns(typea, df = 4)+ns(obesity, df = 4)+ns(alcohol, df = 4)+ns(age, df = 4)+famhist, data = Data, family = "binomial")
# We carried out a backward stepwise deletion process. The AIC stats was used to drop terms
fit <- step(fitFull)

# plot 
opar <- par(mfrow = c(3,2))
plot.gam(fit, se = TRUE, col = "green")