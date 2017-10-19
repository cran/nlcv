#' Produce a ROC plot for a classification model belonging to a given technique
#' and with a given number of features.
#' 
#' Produce a ROC plot for a classification model belonging to a given technique
#' and with a given number of features.
#' 
#' 
#' @param nlcvObj object of class 'nlcv' as produced by the nlcv function
#' @param tech technique; character of length one; one of 'dlda', 'lda',
#' 'nlda', 'qda', 'glm', 'randomForest', 'bagg', 'pam', 'svm' or 'ksvm'
#' @param nfeat number of features used in the classification model; numeric of
#' length one
#' @param main main title to be used for the ROC plot
#' @param globalAUCcol color for the global AUC (defaults to '#FF9900')
#' @param \dots further arguments for the plot call (such as sub e.g.)
#' @return A ROC plot is drawn to the current device
#' @author Tobias Verbeke
#' @importFrom grDevices rgb
#' @importFrom graphics plot lines
#' @importFrom ROCR prediction performance
#' @export
rocPlot <- function(nlcvObj,   # output object from nlcv 
    tech,      # name of the classification technique
    nfeat,     # number of features for which you would like to see
    main = NULL, 
    globalAUCcol = "#FF9900",
     ...){ # additional arguments such as main, sub etc.
  
  ### error checking
  if (!inherits(nlcvObj, "nlcv"))
    stop("nlcvObj is not of class 'nlcv'")
  
  if (!(tech %in% c("dlda", "lda", "nlda", "qda", "glm", 
            "randomForest", "bagg", "pam", "svm", "ksvm"))){
    stop("Invalid classification technique in 'tech' argument")
  }
  nFeaturesName <- paste("nfeat", nfeat, sep = "")
  if (is.null(nlcvObj$output[[tech]][[nFeaturesName]])){
    stop(paste("'nlcvObj' does not contain information for ", nfeat, " feature(s)", sep = ""))
  }    
  
  testScoresMat <- nlcvObj$output[[tech]][[nFeaturesName]][["testScoresMat"]]
  
  meanProbs <- apply(testScoresMat, 2, mean, na.rm = TRUE)
  nans <- is.na(meanProbs)
  meanProbs <- meanProbs[!nans]
  trueClasses <- attr(nlcvObj, "classVar")[!nans] # nlcvObj$trueClasses[!nans]
  predObj <- prediction(predictions = meanProbs, labels = trueClasses)
  
  globalAUC <- unlist(performance(predObj, "auc")@y.values)
  globalROC <- performance(predObj, measure = "sens", x.measure = "spec")
  
  rocList <- nlcvObj$output[[tech]][[nFeaturesName]][["ROC"]]
  
  mainTitle <- if (is.null(main)) paste(tech, ", ", nfeat, " feature(s)", sep = "")  else main
  plot(c(0, 1), c(0, 1),  type = "n",
       xlab = paste("1 -", attributes(rocList[[1]])$x.name), 
       ylab = attributes(rocList[[1]])$y.name,
       sub = paste("mean AUC: ", round(globalAUC, 3), sep  = ""),
       main = mainTitle,
       ...)
  
  
  for(iRoc in seq(along = rocList)){
     
    lines(x = 1 - attributes(rocList[[iRoc]])$x.values[[1]],
         y = attributes(rocList[[iRoc]])$y.values[[1]], 
         col = rgb(150, 150, 150, alpha = 50, maxColorValue = 255))
         #new = FALSE)
  }
  
  lines(x = 1 - attributes(globalROC)$x.values[[1]], 
        y = attributes(globalROC)$y.values[[1]],
        col = globalAUCcol, lwd = 2, lty = "dotted")
    # rgb(0,0,100, maxColorValue = 255)
  
}
