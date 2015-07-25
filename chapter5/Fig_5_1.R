# Generate data
x <- seq(-1, 7, by=0.1)
n <- length(x)
eps <- 0.2*rnorm(n)
y <- cos(x) + eps

quantiles <- quantile(x, probs = c(0.33, 0.67))
index1 <- x < quantiles[1]
x1 <- x[index1]
index2 <- (x >= quantiles[1])&(x < quantiles[2])
x2 <- x[index2]
index3 <- (x > quantiles[2])
x3 <- x[index3]

par(mfrow = c(2,2))
# plot 5.1(a)
mean1 <- mean(y[index1])
mean2 <- mean(y[index2])
mean3 <- mean(y[index3])
plot(x, y, lwd = 1, ann = FALSE, xaxt = 'n', yaxt = 'n')
title(main = "Piecewise Constant")
lines(x, cos(x), col = "blue", type = "l", lwd = 2)
abline(v = quantiles, lty = 2)
lines(x1, rep(mean1, length(x1)), col = "green", type = "l", lwd = 2)
lines(x2, rep(mean2, length(x2)), col = "green", type = "l", lwd = 2)
lines(x3, rep(mean3, length(x3)), col = "green", type = "l", lwd = 2)
# plot 5.1(b)
pl1 <- lm(y[index1]~x1)
coef1 <- pl1$coefficients
X1 <- rbind(1, x1)
Y1 <- t(coef1)%*%X1
pl2 <- lm(y[index2]~x2)
coef2 <- pl2$coefficients
X2 <- rbind(1, x2)
Y2 <- t(coef2)%*%X2
pl3 <- lm(y[index3]~x3)
coef3 <- pl3$coefficients
X3 <- rbind(1, x3)
Y3 <- t(coef3)%*%X3
plot(x, y, lwd = 1, ann = FALSE, xaxt = 'n', yaxt = 'n')
title(main = "Piecewise Linear")
lines(x, cos(x), col = "blue", type = "l", lwd = 2)
abline(v = quantiles, lty = 2)
lines(x1, Y1, col = "green", lwd = 2)
lines(x2, Y2, col = "green", lwd = 2)
lines(x3, Y3, col = "green", lwd = 2)
# plot 5.1(c)
cpl <- lm(y~bs(x, knots = quantiles, degree = 1))
plot(x, y, lwd = 1, ann = FALSE, xaxt = 'n', yaxt = 'n')
title(main = "Continuous Piecewise Linear")
lines(x, cos(x), col = "blue", type = "l", lwd = 2)
abline(v = quantiles, lty = 2)
lines(x, cpl$fitted.values, col = 'green', lwd = 2)