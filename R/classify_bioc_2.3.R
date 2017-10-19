#' @importFrom stats as.formula binomial
#' @importFrom MLInterfaces MLearn dldaI ldaI confuMat qdaI glmI.logistic baggingI svmI ksvmI randomForestI testScores
#' @importFrom e1071 svm
#' @importFrom stats glm
#' @importFrom ipred bagging
#' @importFrom kernlab ksvm
#' @importFrom Biobase pData sampleNames
#' @importFrom ROCR prediction performance
classify_bioc_2.3 <- function(eset, trainingSample, testSample, classVar = "type",
    classifMethodList){
  
  trainingSample <- as.integer(trainingSample)
  error.rate   <- function(cm) 1 - sum(diag(cm)) / sum(cm)
  
  ## Classification with DLDA
  if (classifMethodList$dlda){
    dlda.predic  <- MLearn(formula = as.formula(paste(classVar, "~ .", sep = " ")), 
        data = eset,
        .method = dldaI,
        trainInd = trainingSample)
    
    dlda.error   <- error.rate(confuMat(dlda.predic))
  }
  ## Classification with plain LDA
  if (classifMethodList$lda){
    lda.predic  <- MLearn(formula = as.formula(paste(classVar, "~ .", sep = " ")), 
        data = eset,
        .method = ldaI,
        trainInd = trainingSample)
    
    lda.error   <- error.rate(confuMat(lda.predic))
  }
  ## Classification with LDA (for nlcv: returning testScores)
  if (classifMethodList$nlda){
    nlda.predic  <- MLearn(formula = as.formula(paste(classVar, "~ .", sep = " ")), 
        data = eset,
        .method = nldaI,
        trainInd = trainingSample,
        family = binomial)
    
    nlda.error <- error.rate(confuMat(nlda.predic))
    testS <- testScores(nlda.predic)
    
    allTrueClasses <- pData(eset)[[classVar]]
    names(allTrueClasses) <- sampleNames(eset)
    
    testTrueClasses <- allTrueClasses[names(testS)]
    
    
    predictionObj <- prediction(predictions = testS, labels = testTrueClasses)
    
    nlda.AUC <- unlist(performance(predictionObj, measure = "auc")@y.values)
    nlda.ROC <- performance(predictionObj, measure = "sens", x.measure = "spec")
  }
  
  ## Classification with QDA
  if (classifMethodList$qda){
    qda.predic  <- MLearn(formula = as.formula(paste(classVar, "~ .", sep = " ")), 
        data = eset,
        .method = qdaI,
        trainInd = trainingSample)
    
    qda.error   <- error.rate(confuMat(qda.predic))
  }
  ## Classification with logistic regression
  if (classifMethodList$glm){
    glm.predic  <- MLearn(formula = as.formula(paste(classVar, "~ .", sep = " ")), 
        data = eset,
        .method = glmI.logistic(threshold = 0.5), # TV should become argument !!
        trainInd = trainingSample,
        family = binomial)

    glm.error <- error.rate(confuMat(glm.predic))
    testS <- testScores(glm.predic)
    
    allTrueClasses <- pData(eset)[[classVar]]
    names(allTrueClasses) <- sampleNames(eset)
    
    testTrueClasses <- allTrueClasses[names(testS)]
    
    
    predictionObj <- prediction(predictions = testS, labels = testTrueClasses)
    
    glm.AUC <- unlist(performance(predictionObj, measure = "auc")@y.values)
    glm.ROC <- performance(predictionObj, measure = "sens", x.measure = "spec")
  }
  
  ## Classification with Nearest Neighbors 
  # knn.error <- numeric(3)
  # for(k in c(1,3,5))
  #   {
  # i            <- ((k-1)/2)+1
  #   knn.predic   <- knnB(eset, classVar, LearnSampRun, k = k, prob = FALSE)
  #   knn.error[i] <- error.rate(confuMat(knn.predic))
  #       }
  
  ## Classification with random forest
  if (classifMethodList$randomForest){
    randomForest.predic <- MLearn(formula = as.formula(paste(classVar, "~ .", sep = " ")), 
                        data = eset, 
                        .method = randomForestI, 
                        trainInd = trainingSample, 
                        ntree = 10000)
    
    randomForest.error  <- error.rate(confuMat(randomForest.predic))
  }
  ## Classification with bagging 
  if (classifMethodList$bagg){
    bagg.predic <- MLearn(formula = as.formula(paste(classVar, "~ .", sep = " ")), 
        data = eset, 
        .method = baggingI, 
        trainInd = trainingSample) 
    
    bagg.error <- error.rate(confuMat(bagg.predic))
  }
  
  ## Classification with PAM 
  if (classifMethodList$pam){
    pam.predic <- MLearn(formula = as.formula(paste(classVar, "~ .", sep = " ")), 
        data = eset, 
        .method = pamrI, 
        trainInd = trainingSample)
        
    pam.error <- error.rate(confuMat(pam.predic))
  }
  
  ## Classification with Support Vector Machines
  if (classifMethodList$svm){
    svm.predic <- MLearn(formula = as.formula(paste(classVar, "~ .", sep = " ")), 
        data = eset, 
        .method = svmI, 
        trainInd = trainingSample, 
        kernel = "linear")
    
    svm.error <- error.rate(confuMat(svm.predic))
  }
  #SVM with different cost and gamma settings
  #svm.error <- matrix(0, nrow = 3, ncol = 3)
  #for(cost in 0:2)
  #  {
  # for(gamma in (-1):1)
  #   {
  #     i       <- cost+1
  #     j       <- gamma+2
  #     svm.fit <- svmB(eset, classVar, LearnSampRun, 
  #         cost=2^cost, 
  #         gamma = 2^gamma/nrow(exprs(eset)),
  #         type="C-classification")
  #     svm.error[i,j] <- error.rate(confuMat(svm.fit))
  #   }
  #  }
  if (classifMethodList$ksvm){
    ksvm.predic <- MLearn(formula = as.formula(paste(classVar, "~ .", sep = " ")), 
        data = eset, 
        .method = ksvmI, 
        trainInd = trainingSample) 
    
    ksvm.error <- error.rate(confuMat(ksvm.predic))
  }

  
  ## Output
  res <- vector(mode = "list", length = 2 * length(classifMethodList))
  if (classifMethodList$dlda){
    res$dlda <- dlda.error
    res$dlda.predic <- dlda.predic
  }
  if (classifMethodList$lda){
    res$lda <- lda.error
    res$lda.predic <- lda.predic
  }
  if (classifMethodList$nlda){
    res$nlda <- nlda.error
    res$nlda.predic <- nlda.predic
    res$nlda.AUC <- nlda.AUC
    res$nlda.ROC <- nlda.ROC
  }
  if (classifMethodList$qda){
    res$qda <- qda.error
    res$qda.predic <- qda.predic
  }
  if (classifMethodList$glm){
    res$glm <- glm.error
    res$glm.predic <- glm.predic
    res$glm.AUC <- glm.AUC
    res$glm.ROC <- glm.ROC
  }
  if (classifMethodList$randomForest){
    res$randomForest <- randomForest.error
    res$randomForest.predic <- randomForest.predic
  }
  if (classifMethodList$bagg){
    res$bagg <- bagg.error
    res$bagg.predic <- bagg.predic
  }
  if (classifMethodList$pam){
    res$pam <- pam.error
    res$pam.predic <- pam.predic
  }
  if (classifMethodList$svm){
    res$svm <- svm.error
    res$svm.predic <- svm.predic
  }
  if (classifMethodList$ksvm){
    res$ksvm <- ksvm.error
    res$ksvm.predic <- ksvm.predic
  }
  return(res)
}
