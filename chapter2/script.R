set.seed(4)
mean1 <- c(1,0)
sigma1 <- diag(rep(1, 2))
mean2 <- c(0,1)
sigma2 <- diag(rep(1, 2))
library(MASS)
m1 <- mvrnorm(10, mean1, sigma1)
m2 <- mvrnorm(10, mean2, sigma2)
X1 <- NULL
X2 <- NULL
for (i in 1:100)
{
	id <- sample(10, 1)
	m <- m1[id,]
	x <- mvrnorm(1, m, diag(rep(1/5,2)))
	X1 <- rbind(X1,x)
}
for (i in 1:100)
{
	id <- sample(10, 1)
	m <- m2[id,]
	x <- mvrnorm(1, m, diag(rep(1/5, 2)))
	X2 <- rbind(X2, x)
}
X <- rbind(X1, X2)
x1 <- X[,1]
x2 <- X[,2]
Y <- c(rep(1,100), rep(-1, 100))
xlims <- range(X[,1])
ylims <- range(X[,2])

Xgrid <- NULL
xgrid <- seq(xlims[1], xlims[2], length.out = 100)
ygrid <- seq(ylims[1], ylims[2], length.out = 100)
for (i in 1:100)
{
	for (j in 1:100)
	{
		x <- c(xgrid[i], ygrid[j])
		Xgrid <- rbind(Xgrid, x)
	}
}

plot(X2, col = "orange", xlim = xlims, ylim = ylims, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
lines(X1, type = "p", col = "blue")

# Linear Regression
fit <- lm(Y~x1+x2)
coef <- coef(fit)
a <- coef[1]/(-coef[3])
b <- coef[2]/(-coef[3])
abline(a, b, col = "darkgrey", lwd = 2)
predLinear <- predict(fit, newdata = data.frame(x1 = Xgrid[,1], x2 = Xgrid[,2]))
lines(Xgrid[predLinear < 0,], col = "orange", type = "p", pch = 20, cex = .1)
lines(Xgrid[predLinear > 0, ], col = "blue", type = "p", pch = 20, cex = .1)

# kNN classification
library(class)
dev.new()
predKNN <- knn(X, Xgrid, Y, k = 15)
predKnnProb <- knn(X, Xgrid, Y, k =15, prob = TRUE)
plot(X2, col = "orange", xlim = xlims, ylim = ylims, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
lines(X1, type = "p", col = "blue")
lines(Xgrid[predKNN == -1,], col = "orange", type = "p", pch = 20, cex = .1)
lines(Xgrid[predKNN == 1,], col = "blue", type = "p", pch = 20, cex = .1)

# Bayes decision boundary
dens <- function(x, m) {
	exp(-5/2*sum((x-m)^2))
}
dens1 <- NULL 
dens2 <- NULL
for (j in 1:dim(Xgrid)[1]) {
	dens.1 <- 0
	dens.2 <- 0
	for (i in 1:10) {
		m.1 <- m1[i,]
		m.2 <- m2[i,]
		dens.2 <- dens.2 + dens(Xgrid[j,],m.2)
		dens.1 <- dens.1 + dens(Xgrid[j,],m.1)
	}
	dens1 <- rbind(dens1,dens.1)
	dens2 <- rbind(dens2,dens.2)
}
dev.new()
plot(X2, col = "orange", xlim = xlims, ylim = ylims, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
lines(X1, type = "p", col = "blue")
lines(Xgrid[(dens1 - dens2) <0,], col = "orange", type = "p", pch = 20, cex = .1)
lines(Xgrid[(dens1 - dens2) >0,], col = "blue", type = "p", pch = 20, cex = .1)
lines(Xgrid[abs(dens1 - dens2)/(dens1 + dens2) < 8*1e-3,], type= "l", col = "black")