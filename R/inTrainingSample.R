#' Function to define a learning sample based on balanced sampling
#' 
#' This function takes in a factor with class labels of the total dataset,
#' draws a sample (balanced with respect to the different levels of the factor)
#' and returns a logical vector indicating whether the observation is in the
#' learning sample (\code{TRUE}) or not (\code{FALSE}).
#' 
#' @param y factor with the class labels for the total data set
#' @param propTraining proportion of the data that should be in a training set;
#' the default value is 2/3.
#' @param classdist distribution of classes; allows to indicate whether your
#' distribution 'balanced' or 'unbalanced'. The sampling strategy for each run
#' is adapted accordingly.
#' @return logical vector indicating for each observation in \code{y} whether
#' the observation is in the learning sample (\code{TRUE}) or not
#' (\code{FALSE})
#' @author Willem Talloen and Tobias Verbeke
#' @keywords manip
#' @examples
#' 
#'   ### this example demonstrates the logic of sampling in case of unbalanced distribution of classes
#'   y <- factor(c(rep("A", 21), rep("B", 80)))
#'   
#'   nlcv:::inTrainingSample(y, 2/3, "unbalanced") 
#'   table(y[nlcv:::inTrainingSample(y, 2/3, "unbalanced")])  # should be 14, 14 (for A, B resp.)
#'   table(y[!nlcv:::inTrainingSample(y, 2/3, "unbalanced")]) # should be 7, 66  (for A, B resp.) 
#' 
inTrainingSample <- function(y, propTraining = 2/3, classdist = c("balanced", "unbalanced")) # was: RandomPartition
{
	classdist <- match.arg(classdist)
	if (!(classdist %in% c("balanced", "unbalanced")))
		stop("'classdist' should be one of 'balanced' or 'unbalanced'")
	
	# sample sizes
	nTotalSample <- length(y)
	nTrainingSample <- round(propTraining * nTotalSample)
	nTestSample <- nTotalSample - nTrainingSample
	
	if (nlevels(factor(y) != 2))
		stop("'nlcv' currently only works for two-class problems")
	
	if (classdist == "balanced"){ 
		
		K <- nlevels(factor(y))
		trainingSampleRun <- NULL
		props    <- round(nTrainingSample / nTotalSample * table(y))
		props[1] <- nTrainingSample - sum(props[2:K])
		
		for (k in 1:K){
			y.num  <- as.numeric(factor(y))
			trainingSampleRun <- c(trainingSampleRun, sample(which(y.num == k))[1:props[k]])
		}
		
		res <- rep(FALSE, length = nTotalSample)
		res[trainingSampleRun] <- TRUE 
	} else {
		
		smallestClass <- names(sort(table(y)))[1]
		nSmallest <- sum(y == smallestClass)
		
		nSmallestTrain <- round(propTraining * nSmallest)
		nBiggestTrain <- nSmallestTrain
		nSmallestTest <- nSmallest - nSmallestTrain
		nBiggestTest <- nTotalSample - (nSmallestTest + nSmallestTrain + nBiggestTrain)
		
		
		# split up in smallest class indices and biggest class indices
		smallestIndices <- which(y == smallestClass)
		biggestIndices <- seq(along = y)[-smallestIndices]
		
		sampleSmallestTrain <- sample(smallestIndices, nSmallestTrain)
		# sampleSmallestTest <- smallestIndices[-sampleSmallestTrain]
		
		sampleBiggestTrain <- sample(biggestIndices, nBiggestTrain)
		# sampleBiggestTest <- biggestIndices[-sampleBiggestTrain]
		
		trainingSampleRun <- c(sampleSmallestTrain, sampleBiggestTrain)
		res <- rep(FALSE, length = nTotalSample)
		res[trainingSampleRun] <- TRUE
		
	}
	return(res)
	
}

