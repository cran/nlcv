#' compute a confusion matrix for the optimal number of features for a given
#' technique used in the nested loop cross validation
#' 
#' The observed and predicted classes are cross-tabulated for a given
#' classification technique used in the nested loop cross validation.  The
#' predicted class that is used to construct the confusion matrix is the class
#' that was predicted most of the time (\eqn{>= 50\%}{>= 50\%}) across all runs
#' of the nested loop.
#' @param x object for which a confusionMatrix should be produced, e.g.  one
#' produced by the \code{nlcv} function; for the print method, it is the object
#' to be printed
#' @param tech string indicating the classification technique for which the
#' confusion matrix should be returned
#' @param proportions logical indicating whether the cells of the matrix should
#' contain proportions (\code{TRUE}) or raw counts (\code{FALSE})
#' @param \dots Dots argument to pass additional parameters to the
#' \code{confusionMatrix} or \code{print} methods
#' @return \code{confusionMatrix} produces an object of class
#' \code{confusionMatrix} which directly inherits from the \code{ftable} class
#' (representing the confusion matrix)
#' @author Willem Talloen and Tobias Verbeke
#' @importFrom a4Core confusionMatrix
#' @importFrom stats ftable
#' @keywords manip
#' @export
confusionMatrix.nlcv <- function(x, tech, proportions = TRUE, ...){
	
  mcrObj <- mcrPlot(x, plot = FALSE)
  mcrSummary <- summary(mcrObj)
  # number of features: always optimal number of features
  nFeaturesOptim <- mcrSummary[tech, "nFeat_optim"]
  
  scoresObj <- scoresPlot(x, tech = tech, nfeat = nFeaturesOptim,
      plot = FALSE)
  observedClasses <- attr(x, "classVar")                      
  levelsObserved <- levels(observedClasses)
  oppObservedClasses <- factor(levelsObserved[(as.numeric(observedClasses) %% 2)+1],
      levels = levelsObserved)
  predictedClasses <- factor(levelsObserved[ifelse(scoresObj >= 0.5, 
              observedClasses, oppObservedClasses)], levels = levelsObserved)
  res <- ftable(predictedClasses ~ observedClasses, add.margins = TRUE)
  if (proportions) res <- prop.table(res)   
  # res <- addmargins(res)
  attr(res, "tech") <- tech
  attr(res, "nFeaturesOptim") <- nFeaturesOptim
  class(res) <- c("nlcvConfusionMatrix", "confusionMatrix", class(res)) # extends ftable
  return(res)                      
}

#' print object \code{nlcvConfusionMatrix}
#' @param x object of class \code{nlcvConfusionMatrix}
#' @param ... additional parameters for the \code{print} function
#' @return no returned value, the object is printed in the output
#' @export
print.nlcvConfusionMatrix <- function(x, ...){
  
  tech <- attr(x, "tech")
  nFeaturesOptim <- attr(x, "nFeaturesOptim")
  
  cat("Confusion matrix for classifier: ", tech, "\n")
  cat("Optimal number of features: ", nFeaturesOptim, "\n\n")
  NextMethod(x, ...)
}
