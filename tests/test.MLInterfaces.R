### simulateData
# test extreme values
library(nlcv)

library(a4Core)
myEset <- simulateData(nCols = 40, nRows = 1000, nEffectRows = 0)

### check the converter works as expected
set.seed(120)
x <- matrix(rnorm(1000*20), ncol=20)
y <- sample(c(1:4), size=20, replace=TRUE)

traindf <- cbind.data.frame(t(x[,1:15]), y = y[1:15])
alldf <- cbind.data.frame(t(x), y)

pamrMLObj <- pamrML(y ~ ., traindf)

nlcv:::pamrIconverter(obj = pamrMLObj, data = alldf, trainInd = 1:15)

### test pamrI for an ExpressionSet
EsetStrongSignal <- simulateData(nCols = 40, nRows = 1000, nEffectRows = 10, nNoEffectCols = 0,
    betweenClassDifference = 3, withinClassSd = 0.5)

library(MLInterfaces)
idxTrain <- sample(1:40, 20)
mlobj <- MLearn(type ~ .,
    data = EsetStrongSignal,
    .method = pamrI,
    trainInd = idxTrain)

mlobj

# nlda (to check export of predict.lda)

mlNldaObj <- MLearn(type ~ .,
	data = EsetStrongSignal,
	.method = nldaI,
	trainInd = idxTrain)



