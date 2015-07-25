# Polynomial Regression and Step Functions
library(ISLR)
attach(Wage)
fit = lm(wage~poly(age, 4), data = Wage)
# The poly() command allows us to avoid having to write out a long formula with powers of age.
# The function returns a matrix whose columns are a basis of orthogonal polynomials.
coef(summary(fit))

# we can also use poly() to obtain age, age^2, age^3 directly. We can do this by using the raw = TRUE
fit2 = lm(wage~poly(age, 4, raw = T), data = Wage)
coef(summary(fit2))
# There are several other equvalent ways of fitting this model
fit2a = lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage)
# Takeing care to protect terms like age^2 via the wrapper function I(), the ^ symbol has a sepcial meaning in formual
coef(fit2a)

# any function call cbind() inside a formula also serves as a wrapper
fit2b = lm(wage ~ cbind(age, age^2, age^3, age^4), data = Wage)

# We now create a grid of values for age at which we want predictions, and the call the generic predict() function,
# specifying that we want standard errors as well
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
preds = predict(fit, newdata= list(age = age.grid), se = TRUE)
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
par(mfrow = c(1,2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex = .5, col = 'darkgrey')
title('Degree-4 Polynomial', outer = T)
lines(age.grid, preds$fit, lwd = 2, col = 'blue')
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# In performing a polynomial regression we must decide on the degree of the polynimial to use
# One way to do this is by using hypothesis tests. We use anova() function, in this case, we fit five different models 
#	and sequentially compare the simpler model to the more complex model.
fit.1 = lm(wage ~ age, data = Wage)
fit.2 = lm(wage ~ poly(age, 2), data = Wage)
fit.3 = lm(wage ~ poly(age, 3), data = Wage)
fit.4 = lm(wage ~ poly(age, 4), data = Wage)
fit.5 = lm(wage ~ poly(age, 5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
# in this case, instead of using the anova() function, we could have obtained these p-values more succinctly by expoilting 
# the fact that poly() creates orthogonal polynimials
coef(summary(fit.5))
# However, the ANOVA method works whether or not we used orthogonal polynomials; it alse works when we have other terms in
# the model as well.
fit.1 = lm(wage ~ education + age, data = Wage)
fit.2 = lm(wage ~ education + poly(age, 2), data = Wage)
fit.3 = lm(wage ~ education + poly(age, 3), data = Wage)
anova(fit.1, fit.2, fit.3)

# As an alternative to using hypothesis tests and ANOVA, we could choose the polynomial degree usding cross-validation

# Next we consider the task of prediction whether an individual earns more than $250,000 per year
fit = glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial)
preds = predict(fit, newdata = list(age = age.grid), se = T)
# Calculating the confidence intervals is slightly more involved than in the linear regression case. The default prediction 
# type for a glm() model is type = "link", this means we get predictions for the logit
pfit = exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))
# Note that we could have directly computed the probabilities by selecting the type = "response" option in the predict() function.
preds = predict(fit, newdata = list(age = age.grid), type = "response", se = T)
# However, the corresponding confidence intervals would not have been sensible because we would end up with negative probabilities!
plot(age, I(wage > 250), xlim = agelims, type = "n", ylim = c(0, .2))
points(jitter(age), I((wage>250)/5), cex = .5, pch = "|", col = 'darkgrey')
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# In order to fit a step function, we use cut() function
table(cut(age, 4))
fit = lm(wage~ cut(age, 4), data= Wage)
coef(summary(fit))