train <- read.table("zip.train.txt")
train <- train[(train[,1] == 2)|(train[,1] == 3),]
test <- read.table("zip.test.txt")
test <- test[(test[,1] == 2)|(test[,1] == 3),]
trainX <- train[,2:ncol(train)]
trainY <- train[,1]
testX <- test[,2:ncol(train)]
testY <- test[,1]

# linear regression classification
fit <- lm(V1~., data = train)
trainPredLinear <- fit$fitted
trainPredLinear[trainPredLinear < 2.5] = 2
trainPredLinear[trainPredLinear > 2] = 3
testPredLinear <- predict(fit, newdata = test)
testPredLinear[testPredLinear < 2.5] = 2
testPredLinear[testPredLinear > 2] = 3
linTrainError <- mean(trainPredLinear == trainY)
linTestError <- mean(testPredLinear == testY)


# knn classification
library(class)
knnTrainError <- NULL
knnTestError <- NULL
for(i in c(1,3,5,7,15)){
	testPredKnn <- knn(trainX, testX, trainY, k = i)
	trainPredKnn <- knn(trainX, trainX, trainY, k = i)
	trainError <- mean(trainY == trainPredKnn)
	knnTrainError <- c(knnTrainError, trainError)
	testError <- mean(testY == testPredKnn)
	knnTestError <- c(knnTestError, testError)
}

# output
cat("linTrainError =",linTrainError,"\n","\blinTestError = ", linTestError,"\n")
cat("knnTrainError =",knnTrainError,"\n","\bknnTestError = ", knnTestError,"\n")
