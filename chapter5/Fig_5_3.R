# Pointwise variance curves for four different models

library(splines)
library(MASS)
# define several functions
logit <- function(H, coef) {
	exp(H%*%coef)/(1 + exp(H%*%coef))
}
pointvar <- function(H, coef) {
	W <- diag(as.vector(logit(H, coef)*(1-logit(H, coef))))
	sigma <- ginv(t(H)%*%W%*%H)
	diag(H%*%sigma%*%t(H))
}


# generate data
set.seed(50)
X <- runif(50)
Y <- rnorm(50)
Data <- data.frame(X1 = rep(1, 50), X2 = X, Y = Y)

# Using four different models fit the data
fitGloLin <- lm(Y~.-1, data = Data)
coefGL <- coef(summary(fitGloLin))[,1]
HGL <- cbind(rep(1, 50), X)

fitGloCub <- lm(Y~poly(X2, 3), data = Data)
coefGC <- coef(summary(fitGloCub))[,1]
HGC <- cbind(rep(1, 50), matrix(poly(X,3), nrow = 50))

fitCubSpl <- lm(Y~bs(X2, knots = c(.33, .66)), data = Data)
coefCS <- coef(summary(fitCubSpl))[,1]
HCS <- cbind(rep(1, 50), bs(X, knots = c(.33,.66)))

fitNatSpl <- lm(Y~ns(X2, knots = seq(.10, .90, .16)), data = Data)
coefNS <- coef(summary(fitNatSpl))[,1]
HNS <- cbind(rep(1, 50), ns(X, knots = seq(.10, .90, .16)))

# plot
orderx <- order(X)
Xorder <- X[orderx]
varGL <- pointvar(HGL, coefGL)[orderx]
varGC <- pointvar(HGC, coefGC)[orderx]
varCS <- pointvar(HCS, coefCS)[orderx]
varNS <- pointvar(HNS, coefNS)[orderx]
ylim = range(c(varGL, varGC, varCS, varNS))
plot(Xorder, varGL, type = 'l', lwd = 1, col = "orange", xlab = "X", ylab = "Pointwise Variances", ylim = ylim)
lines(Xorder, varGC, type = 'l', lwd = 1, col = "red")
lines(Xorder, varCS, type = 'l', lwd = 1, col = "green")
lines(Xorder, varNS, type = 'l', lwd = 1, col = "blue")
legend("topright", c("Global Linear", "Global Cubic Polynomial", "Cubic Splines", "Natural Cubic"), col = c("orange", "red", "green", "blue"), lty = 1)