#' Function providing a formula interface to pamr.train
#' 
#' Function that provides a classical R modelling interface, using a
#' \code{formula} and \code{data} argument
#' @param formula formula
#' @param data data frame
#' @param \dots further arguments to be passed to \code{pamr.train}
#' @return Object that is perfectly identical to the object returned by
#' \code{pamr.train}
#' @author Tobias Verbeke
#' @seealso \code{\link[pamr]{pamr.train}}
#' @keywords models
#' @examples
#' 
#'   set.seed(120)
#'   x <- matrix(rnorm(1000*20), ncol=20)
#'   y <- sample(c(1:4), size=20, replace=TRUE)
#'   alldf <- cbind.data.frame(t(x), y)
#'   pamrTrain(y ~ ., alldf)
#' @importFrom stats model.matrix model.frame model.response
#' @importFrom pamr pamr.train
#' @export
pamrTrain <- function(formula, data, ...){
  x <- model.matrix(formula, data)
  if ("(Intercept)" %in% colnames(x)){
    x <- x[,-which(colnames(x) %in% "(Intercept)")]
  }
  x <- t(x)
  mf <- model.frame(formula, data)
  resp <- model.response(mf)
  inputList <- list(x = x, y = resp)
  pamrObj <- pamr.train(data = inputList, ...)
  return(pamrObj)
}



#' Wrapper function around the pamr.* functions
#' 
#' The pamrML functions are wrappers around \code{pamr.train} and
#' \code{pamr.predict} that provide a more classical R modelling interface than
#' the original versions.
#' 
#' The name of the response variable is kept as an attribute in the
#' \code{pamrML} object to allow for predict methods that can be easily used
#' for writing converter functions for use in the \code{MLInterfaces}
#' framework.
#' 
#' @aliases pamrML
#' @param formula model formula
#' @param data data frame
#' @param \dots argument for the \code{parmTrain} function
#' @return For \code{pamrML} an object of class \code{pamrML} which adds an
#' attribute to the original object returned by \code{pamr.train} (or
#' \code{pamrTrain}).
#' 
#' The \code{print} method lists the names of the different components of the
#' \code{pamrML} object.
#' 
#' The \code{predict} method returns a vector of predicted values
#' @author Tobias Verbeke
#' @seealso \code{\link[pamr]{pamr.train}}, \code{\link[pamr]{pamr.predict}}
#' @keywords models
#' @examples
#' 
#'   set.seed(120)
#'   x <- matrix(rnorm(1000*20), ncol=20)
#'   y <- sample(c(1:4), size=20, replace=TRUE)
#'   # for original pam
#'   mydata <- list(x=x, y=y)
#'   mytraindata <- list(x=x[,1:15],y=factor(y[1:15]))
#'   mytestdata <-  list(x = x[,16:20], y = factor(y[16:20]))
#' 
#'   # for formula-based methods including pamrML
#'   alldf <- cbind.data.frame(t(mydata$x), y)
#'   traindf <- cbind.data.frame(t(mytraindata$x), y = mytraindata$y)
#'   testdf <- cbind.data.frame(t(mytestdata$x), y = mytestdata$y)
#' 
#'   ### create pamrML object
#'   pamrMLObj <- pamrML(y ~ ., traindf)
#'   pamrMLObj
#' 
#'   ### test predict method
#'   predict(object = pamrMLObj, newdata = testdf, 
#'       threshold = 1) # threshold compulsory
#' @importFrom Biobase exprs pData
#' @export
pamrML <- function(formula, data, ...){
  
  # TV: quick and (especially) dirty
  responseName <- as.character(formula[[2]])
  if (class(data) == "ExpressionSet"){
    data <- as.data.frame(t(exprs(data)))
    data <- cbind(data, pData(data)[[responseName]])
  }
  
  fit <- pamrTrain(formula, data, ...)
  
  attr(fit, "responseName") <- responseName # for the predict method
  class(fit) <- "pamrML"
  fit
}


#' predict \code{pamrML} object
#' @param object \code{pamrML} object
#' @param newdata new data
#' @param ... additional parameters for the \link{pamr.predict} function
#' @return output of the \code{pamr.predict} function
#' @importFrom stats predict
#' @importFrom pamr pamr.predict
#' @S3method predict pamrML
#' @export
predict.pamrML <- function(object, newdata, ...){
  
  if (class(newdata) == "ExpressionSet"){
    newx <- exprs(newdata)
  } else { # data frame
    # remove response if present (for an easy MLIConverter)
    responseName <- attr(object, "responseName")
    responsePos <- which(names(newdata) == responseName)
    if (length(responsePos)) newdata <- newdata[,-responsePos]
    newx <- t(data.matrix(newdata))
  }
  
  parList <- list(...)
  parList$fit <- object
  parList$newx <- newx
  
  if (is.null(parList$threshold)) 
    parList$threshold <- 1
  if (is.null(parList$type)) parList$type <- 
        c("class", "posterior", "centroid", "nonzero")
  if (is.null(parList$prior)) parList$prior <- object$prior
  if (is.null(parList$threshold.scale)) parList$threshold.scale <-
        object$threshold.scale
        
  res <- do.call("pamr.predict", parList)
  return(res)
}


#' print \code{pamrML} object
#' @param x object of class \code{pamrML}
#' @param \dots additional parameters for the \code{print} function
#' @export
print.pamrML <- function(x, ...){
  cat("pamrML S3 instance. components:\n")
  print(names(x), ...)
}

#' convert from \code{pamrML} to \code{classifierOutput}
#' @param obj object as returned by pamrML i.e. of class \code{pamrML} 
#' @param data original data used as input for MLearn
#' @param trainInd training indices used as input to MLearn
#' @return object of class \code{classifierOutput}
pamrIconverter <- function(obj, data, trainInd){
  teData <- data[-trainInd,]
  trData <- data[trainInd,]
  tepr <- predict(obj, teData)
  trpr <- predict(obj, trData)
  names(tepr) <- rownames(teData)
  names(trpr) <- rownames(trData)
  res <- new("classifierOutput", testPredictions = factor(tepr), 
      trainPredictions = factor(trpr), RObject = obj)
  return(res)
}



#' Instance of a learnerSchema for pamr models
#' 
#' This object is an instance of the learnerSchema object and will be typically
#' used as the \code{method} argument of an \code{MLearn} call.
#' 
#' @author Tobias Verbeke
#' @seealso \code{\link[MLInterfaces]{MLearn}}
#' @keywords models
#' @examples
#' 
#'   set.seed(120)
#'   x <- matrix(rnorm(1000*20), ncol=20)
#'   y <- sample(c(1:4), size=20, replace=TRUE)
#'   alldf <- cbind.data.frame(t(x), y)
#' 
#'   # assure it is a factor (otherwise error message)
#'   alldf$y <- factor(alldf$y) 
#'   library(MLInterfaces)
#'   (mlobj <- MLearn(y ~ .,
#'       data = alldf,
#'       .method = pamrI,
#'       trainInd = 1:15))
#' @importFrom methods new
#' @export
pamrI <- new("learnerSchema", packageName = "nlcv", 
    mlFunName = "pamrML", converter = pamrIconverter)
