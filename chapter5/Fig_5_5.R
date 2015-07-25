# Phoneme Recognition
# In this example we use splines to reduce flexibility rather than increase it
# functional modeling

# preprocess the data
setwd("/Users/fanfan/github/Elements of Statistical Learning code/chapter5/")
Data <- read.csv("phoneme.data.txt")
Data <- Data[,-c(1,dim(Data)[2])]
Data = Data[(Data[,"g"] == "aa")+(Data[,"g"] == "ao") == 1,]
library(splines)
set.seed(100)

opar <- par(mfrow = c(2,1))
# plot
matplot(t(as.matrix(Data[Data[,"g"] == "aa",1:256][sample(1:695,15),])), type = "l", col = "green", main = "Phoneme Examples",
	xlab = "Frequency", ylab = "Log-periodogram")
matlines(t(as.matrix(Data[Data[,"g"] == "ao",1:256][sample(1:1022,15),])), type = "l", col = 'red')
legend('topright',c("aa", "ao"), col = c("green", "red"), lty = 1)

# Raw Logistic Regression
rawfit <- glm(g~., data = Data, family = "binomial")
rawcoef <- coef(summary(rawfit))[-1, 1]
plot(rawcoef, main = "Raw and Restricted Logistic Regression", xlab = "Frequency", ylab = "Logistic Regression Coefficients", 
	type = "l", col = "grey", ylim = c(-.4,0.4))
abline(h = 0, col = "grey")
# Restricted Logistic Regression
H <- ns(1:256, df = 12)
X <- as.matrix(Data[,1:256])
Xstar <- X%*%H
resfit <- glm(Data[,"g"] ~ Xstar, family = "binomial")
rescoef <- coef(summary(resfit))[-1,1]
rescoef <- H%*%(as.matrix(rescoef))
lines(rescoef, col = "red", type = "l", lwd = 2)