#' Wrapper around limma for the comparison of two groups
#' 
#' Wrapper around limma for the comparison of two groups
#' 
#' Basically, the wrapper combines the \code{lmFit}, \code{eBayes} and
#' \code{topTable} steps
#' 
#' @param object object of class ExpressionSet
#' @param group string indicating the variable defining the two groups to be
#' compared
#' @return \code{topTable} output for the second (i.e. slope) coefficient of
#' the linear model.
#' @author Tobias Verbeke
#' @references Smyth, G. K. (2004). Linear models and empirical Bayes methods
#' for assessing differential expression in microarray experiments.
#' \emph{Statistical Applications in Genetics and Molecular Biology}, Vol. 3,
#' No. 1, Article 3.
#' 
#' \url{http://www.bepress.com/sagmb/vol3/iss1/art3}
#' @importFrom stats model.matrix
#' @importFrom limma lmFit eBayes topTable
#' @keywords models regression
limmaTwoGroups <- function(object, group){
	
  f <- as.numeric(as.factor(pData(object)[, group]))
  if (length(unique(f)) != 2)
    stop("Use 'limmaTwoGroups' only with a 'group' variable having two group levels")
  design <- model.matrix(~ f)
  fit <- lmFit(object, design)
  fit <- eBayes(fit)
  fit$genes <- cbind(ROWNAMES = rownames(fit$genes), fit$genes)
  sortedResults <- limma::topTable(fit, coef = 2, number = dim(object)["Features"])
  # coef = 2 because we are not interested whether the intercept is significant 
  # but whether group 2 is significantly different from group 1
  return(sortedResults)
  
}


