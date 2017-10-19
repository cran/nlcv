#' @import MASS
#' @importFrom methods new
MLIConverter.nlda = function(obj, data, trainInd) {
  teData = data[-trainInd,]
  trData = data[trainInd,]
  tepr = predict(obj, teData)$class
  trpr = predict(obj, trData)$class
  tesco =  predict(obj, teData)$posterior[,2] # 
  names(tepr) = rownames(teData)
  names(trpr) = rownames(trData)
  new("classifierOutput", testPredictions=factor(tepr),
      trainPredictions=factor(trpr), testScores=tesco, RObject=obj)
}

#' new MLInterfaces schema for lda from MASS
#' 
#' This interface keeps track of the predictions on the training and test set,
#' contrary to the ldaI interface that is made available in the MLInterfaces
#' package.
#' 
#' nldaI is an object of class 'learnerSchema' and can be used as such in calls
#' to MLearn (from MLInterfaces).
#' 
#' @name nldaI
#' @docType data
#' @seealso See Also \code{\link[MLInterfaces]{ldaI}}
#' @export
#' @importFrom methods new
#' @keywords manip
nldaI <- new("learnerSchema", packageName = "MASS", 
    mlFunName = "lda", converter = MLIConverter.nlda)

