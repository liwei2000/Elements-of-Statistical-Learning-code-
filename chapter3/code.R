# Fig3.5   best subset selection
data <- read.table("/Users/fanfan/R/ESL/prostate.data.txt")
prostate <- data[,-ncol(data)]
train <- data[,ncol(data)]
library(leaps)
fit <- regsubsets(lpsa ~.,data = prostate,subset = train, nvmax = 8)
fitSummary <- summary(fit)
rss <- c(sum((prostate[train,9] - mean(prostate[train,9]))^2)
, fitSummary$rss)
plot(rss, xlab = "Subset Size k", ylab = "Residual Sum-of-Squates", type= "b", col = "red", ylim = c(0,100), pch = 20)

# Fig3.7
prostate[,1:8] <- scale(prostate[,1:8])
fitLS <- lm(lpsa ~., data = prostate, subset = train)
coefLS <- coef(fitLS)
predictLS <- predict(fitLS, prostate[!train,])
testerrLS <- mean((predictLS - prostate[!train, ncol(prostate)])^2)

# Fig3.10
library(glmnet)
x <- prostate[,1:8]
y <- prostate[,9]
out <- glmnet(as.matrix(x), y, alpha = 1)
plot(out, col = "blue")
