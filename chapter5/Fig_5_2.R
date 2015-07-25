# Generate data
set.seed(1)
x <- seq(-1, 7, by=0.1)
n <- length(x)
eps <- sample(0.3*abs(x), length(x))*rnorm(n)
y <- cos(x) + eps

quantiles <- quantile(x, probs = c(0.33, 0.67))
index1 <- x < quantiles[1]
x1 <- x[index1]
index2 <- (x >= quantiles[1])&(x < quantiles[2])
x2 <- x[index2]
index3 <- (x > quantiles[2])
x3 <- x[index3]

par(mfrow = c(2,2))
# plot 5.2(a)
disc_cubic_spline1 <- lm(y[index1]~poly(x1, 3))
disc_cubic_spline2 <- lm(y[index2]~poly(x2, 3))
disc_cubic_spline3 <- lm(y[index3]~poly(x3, 3))
plot(x, y, lwd = 1, ann = FALSE, xaxt = 'n', yaxt = 'n')
title(main = "Discontinuous")
lines(x, cos(x), col = "blue", type = "l", lwd = 2)
abline(v = quantiles, lty = 2)
lines(x1, disc_cubic_spline1$fitted.values, col = 'green', lwd = 2)
lines(x2, disc_cubic_spline2$fitted.values, col = 'green', lwd = 2)
lines(x3, disc_cubic_spline3$fitted.values, col = 'green', lwd = 2)
# generate data frame for continuous splines
x_knot1 <- x - quantiles[1]
x_knot1[x_knot1 < 0] <- 0
x_knot2 <- x - quantiles[2]
x_knot2[x_knot2 < 0] <- 0
X = data.frame(h1 = rep(1, length(x)), h2 = x, h3 = x^2, h4 = x^3, h5 = x_knot1^3, h6 = x_knot2^3, h7 = x_knot1^2, h8 = x_knot2^2,
				h9 = x_knot1, h10 = x_knot2, y = y)
# plot 5.2(b)
conFit <- lm(y~.-1, data = X)
plot(x, y, lwd = 1, ann = FALSE, xaxt = 'n', yaxt = 'n')
title(main = "Continuous")
lines(x, cos(x), col = "blue", type = "l", lwd = 2)
abline(v = quantiles, lty = 2)
lines(x, conFit$fitted.values, col = 'green', lwd = 2)
# plot 5.2(c)
conFisderFit <- lm(y~.-1-h9-h10, data = X)
plot(x, y, lwd = 1, ann = FALSE, xaxt = 'n', yaxt = 'n')
title(main = "Continuous First Derivative")
lines(x, cos(x), col = "blue", type = "l", lwd = 2)
abline(v = quantiles, lty = 2)
lines(x, conFisderFit$fitted.values, col = 'green', lwd = 2)
# plot 5.2(d)
conSecderFit <- lm(y~.-1-h9-h10-h7-h8, data = X)
plot(x, y, lwd = 1, ann = FALSE, xaxt = 'n', yaxt = 'n')
title(main = "Continuous Second Derivative")
lines(x, cos(x), col = "blue", type = "l", lwd = 2)
abline(v = quantiles, lty = 2)
lines(x, conSecderFit$fitted.values, col = 'green', lwd = 2)
