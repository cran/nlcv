setOldClass("nlcv")

#' Methods for topTable
#' 
#' Methods for topTable. topTable extracts the top n most important features
#' for a given classification or regression procedure.
#' 
#' The top n features are extracted across all runs of the nested loop
#' cross-validation. After ranking on their frequency of selection, the top n
#' are retained and returned.
#' 
#' @name topTable-methods
#' @aliases topTable topTable-methods topTable,nlcv-method
#' @docType methods
#' @param fit object resulting from a classification or regression procedure
#' @param n number of features that one wants to extract from a table that
#' ranks all features according to their importance in the classification or
#' regression model
#' @param method method used to rank the features; one of \code{percentage}
#' (percentage of runs the feature is selected in the top n), \code{meanrank}
#' (mean rank of the feature across runs) or \code{medianrank} (median rank of
#' the feature across runs); \code{percentage} is the default method
#' @return a data frame of one column (\code{percentage}) with percentages
#' reflecting the frequency of selection of a feature in the top n across all
#' runs; the features are sorted on decreasing frequency.
#' @section Methods: \describe{
#' 
#' nlcv
#' 
#' \item{fit = "nlcv"}{nlcv objects are produced by \code{nlcv}} }
#' @author Willem Talloen and Tobias Verbeke
#' @keywords methods manip
#' @examples
#' 
#'   data(nlcvRF_SS)
#'   topTable(nlcvRF_SS, n = 7, method = "medianrank")
#' @importFrom stats median
#' @export
setMethod("topTable", "nlcv", 
    function(fit, n = 5, method = "percentage"){
		
  if (!inherits(fit, "nlcv")) 
    stop("The object is not of class 'nlcv'")
  if (!(method %in% c("percentage", "meanrank", "medianrank")))
  
  n <- match.arg(n)
  switch(method,
      percentage = {
        nRuns <- length(fit$features)
        selectedFeatures <- base::table(unlist(lapply(fit$features,
              function(x){names(x)[1:n]})))
        selectedFeatures <- selectedFeatures[order(selectedFeatures,
                decreasing = TRUE)][1:n]     
        selectedFeatures <- 100 * (selectedFeatures / nRuns)
        res <- data.frame(percentage = selectedFeatures)
      },
      meanrank = {
        orderedFeatureMatrix <- do.call("cbind", lapply(fit$features, function(x) base::rank(x[order(names(x))])))
        meansByFeature <- apply(orderedFeatureMatrix, 1, mean, na.rm = TRUE)
        selectedFeatures <- sort(meansByFeature)[1:n]
      res <- data.frame(meanrank = selectedFeatures)
      },
      medianrank = {
        orderedFeatureMatrix <- do.call("cbind", lapply(fit$features, function(x) base::rank(x[order(names(x))])))
        mediansByFeature <- apply(orderedFeatureMatrix, 1, median, na.rm = TRUE)
        selectedFeatures <- sort(mediansByFeature)[1:n]
        res <- data.frame(medianrank = selectedFeatures)
  })
  return(res)
})


