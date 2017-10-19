#' xtable method for summary.mcrPlot objects
#' @param x object of class 'summary.mcrPlot' as produced by the
#' \code{summary.mcrPlot}
#' @param caption LaTeX caption, see the \code{xtable} help page
#' @param label LaTeX label, see the \code{xtable} help page
#' @param align alignment specification, see the \code{xtable} help page
#' @param digits number of digits to display, see the \code{xtable} help page
#' @param display format of the columns, see the \code{xtable} help page
#' @param \dots additional arguments to be passed to \code{xtable}
#' @return LaTeX table representing the summary of the mcrPlot output, i.e.
#' the optimal number of features, the mean MCR and the standard deviation on
#' the MCR for each of the classification methods used.
#' @author Willem Talloen and Tobias Verbeke
#' @seealso \code{\link{summary.mcrPlot}}, \code{\link{mcrPlot}},
#' \code{\link[xtable]{xtable}}
#' @keywords manip
#' @examples
#' 
#'   data(nlcvRF_SS)
#'   mp <- mcrPlot(nlcvRF_SS, plot = FALSE)
#'   smp <- summary(mp)
#'   xtable(smp)
#' @importFrom xtable xtable
#' @S3method xtable summary.mcrPlot
#' @export
xtable.summary.mcrPlot <- function(x, caption = NULL, label = NULL, align = NULL, 
    digits = NULL, display = NULL, ...){
  smpMat <- as.matrix(unclass(x))
  xtable(smpMat, caption = caption, label = label, align = align, 
         digits = digits, display = display, ...)
}

