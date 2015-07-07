# Fig3.7   Comparison of subset-selection techniques on a simulated problem
library(MASS)
mean <- rep(0,31)
sigma <- matrix(rep(0, 31^2), ncol = 31) + diag(rep(1, 31))
# generate data
MseBest <- rep(0,31)
MseFStep <- rep(0,31)
MseBStep <- rep(0,31)
library(leaps)
set.seed(10)
for (j in 1:50) {
	X <- mvrnorm(300, mean, sigma)
	beta <- rep(0, 31)
	index <- sample(31, 10)
	beta[index] <- rnorm(10, 0, 0.4)
	epsilon <- rnorm(300, 0, 6.25)
	y <- X%*%(beta) + epsilon
	data <- data.frame(y, X) #dimnames = list(paste(1:300), c("y", paste(1:31))))
	BestFit <- regsubsets(y ~., data = data, intercept = F, nvmax = 31)
	FstepFit <- regsubsets(y ~., data = data, intercept = F, method = "forward", nvmax = 31)
	BstepFit <- regsubsets(y ~., data = data, intercept = F, method = "backward", nvmax = 31)
	for (i in 1:31) {
		betaBestFiti <- rep(0, 31)
		coef <- coef(BestFit, i)
		names(betaBestFiti) <- names(data)[-1]
		betaBestFiti[names(coef)] <- coef
		MseBest[i] <- sum((beta - betaBestFiti)^2) + MseBest[i]
		betaFStepFiti <- coef(FstepFit, i)
		MseFStep[i] <- sum((beta - betaFStepFiti)^2) + MseFStep[i]
		betaBStepFiti <- coef(BstepFit, i)
		MseBStep[i] <- sum((beta - betaBStepFiti)^2) + MseBStep[i]
	}
}
plot(MseBest/50, type = "b", pch = 20, cex = 1, col = "black")