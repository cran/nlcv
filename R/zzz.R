#' @importFrom utils packageDescription
.onAttach <- function(libname, pkgname){
  options(error = NULL)
  packageStartupMessage(paste("\nnlcv version ", packageDescription("nlcv")$Version, 
          "\n", sep = ""))
}
