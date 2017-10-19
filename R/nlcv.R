#' Nested Loop Cross-Validation
#' 
#' This function first proceeds to a feature selection and then applies five
#' different classification algorithms. 
#' 
#' @param eset ExpressionSet object containing the genes to classify
#' @param classVar String giving the name of the variable containing the
#' observed class labels, should be contained in the phenoData of \code{eset}
#' @param nRuns Number of runs for the outer loop of the cross-validation
#' @param propTraining Proportion of the observations to be assigned to the
#' training set. By default \code{propTraining = 2/3}.
#' @param classdist distribution of classes; allows to indicate whether your
#' distribution is 'balanced' or 'unbalanced'. The sampling strategy for each run
#' is adapted accordingly.
#' @param nFeatures Numeric vector with the number of features to be selected
#' from the features kept by the feature selection method. For each number n
#' specified in this vector the classification algorithms will be run using
#' only the top n features.
#' @param fsMethod Feature selection method; one of \code{"randomForest"} (default),
#' \code{"t.test"}, \code{"limma"} or \code{"none"}.
#' @param classifMethods character vector with the classification methods to be
#' used in the analysis; elements can be chosen among 
#' \code{"dlda"}, \code{"randomForest"}, \code{"bagg"}, \code{"pam"}
#' \code{"svm"}, \code{"glm"}, \code{"lda"}, \code{"nlda"}, \code{"dlda"}, \code{"ksvm"}.
#' The first 5 methods are selected by default
#' @param fsPar List of further parameters to pass to the feature selection
#' method; currently the default for \code{"randomForest"} is an empty
#' \code{list()} whereas for \code{"t.test"}, one can specify the particular
#' test to be used (the default being \code{list(test = "f"}).
#' @param geneID string representing the name of the gene ID variable in the
#' fData of the expression set to use; this argument was added for people who
#' use e.g. both Entrez IDs and Ensemble gene IDs
#' @param initialGenes Initial subset of genes in the ExpressionSet on which to
#' apply the nested loop cross validation procedure. By default all genes are
#' selected.
#' @param storeTestScores should the test scores be stored in the \code{nlcv}
#' object? Defaults to \code{FALSE}
#' @param verbose Should the output be verbose (\code{TRUE}) or not
#' (\code{FALSE}).
#' @return The result is an object of class 'nlcv'. It is a list with two
#' components, \code{output} and \code{features}.
#' 
#' De \code{output} component is a list of five components, one for each
#' classification algorithm used. Each of these components has as many
#' components as there are elements in the \code{nFeatures} vector. These
#' components contain both the error rates for each run (component
#' \code{errorRate}) and the predicted labels for each run (character matrix
#' \code{labelsMat}).
#' 
#' The \code{features} list is a list with as many components as there are
#' runs. For each run, a named vector is given with the variable importance
#' measure for each gene. For t test based feature selection, P-values are
#' used; for random forest based feature selection the variable importance
#' measure is given.
#' @note The variable importance measure used is the third column of the output
#' returned by the \code{randomForest} function.
#' @author Willem Talloen and Tobias Verbeke
#' @keywords htest
#' @importFrom Biobase pData featureNames exprs
#' @importFrom randomForest randomForest importance
#' @importFrom multtest mt.teststat
#' @importFrom MLInterfaces testPredictions
#' @export
nlcv <- function(eset,
		classVar = "type",
		nRuns = 2,          # total number of runs i.e. number of splits in training and test set
		propTraining = 2/3, # proportion of data in a training set                     
		classdist = c("balanced", "unbalanced"), 
		nFeatures = c(2, 3, 5, 7, 10, 15, 20, 25, 30, 35), #, 40),
		fsMethod = c("randomForest", "t.test", "limma", "none"),
		classifMethods = c("dlda", "randomForest", "bagg", "pam", "svm"),
		fsPar = NULL,
		initialGenes = seq(length.out = nrow(eset)),
		geneID = "ID",
		storeTestScores = FALSE,
		verbose = FALSE){
	
	fsMethod <- match.arg(fsMethod)
	if (!(fsMethod %in% c("randomForest", "t.test", "limma", "none")))
		stop("argument fsMethod should be one of 'randomForest', 't.test', 'limma' or 'none'")
	
	classifMethodList <- list(dlda = "dlda" %in% classifMethods,
			lda = "lda" %in% classifMethods,
			nlda = "nlda" %in% classifMethods,
			qda = "qda" %in% classifMethods,
			glm = "glm" %in% classifMethods,
			randomForest = "randomForest" %in% classifMethods,
			bagg = "bagg" %in% classifMethods,
			pam = "pam" %in% classifMethods,
			svm = "svm" %in% classifMethods,
			ksvm = "ksvm" %in% classifMethods)
	
	# check on classVar
	if (!is.factor(pData(eset)[,classVar]))
		stop("'classVar' should be a factor variable")
	
	classdist <- match.arg(classdist)
	
	# sort nFeatures (assumption of mcrPlot)
	nFeatures <- sort(nFeatures)
	
  ## initialize matrices (total, training, test)
  ## depends on classdist argument
  
  respVar <- pData(eset)[, classVar]
  nTotalSample <- length(respVar)  # total sample size
  
  if (classdist == "balanced"){
     
    nTrainingSample <- round(propTraining * nTotalSample) # sample size of training (=learning) set 
    nTestSample <- nTotalSample - nTrainingSample  #   "            validation (=test) set
     
  } else {
    
    smallestClass <- names(sort(table(respVar)))[1]
    nSmallest <- sum(respVar == smallestClass)
    
    nSmallestTrain <- round(propTraining * nSmallest)
    nBiggestTrain <- nSmallestTrain
    nSmallestTest <- nSmallest - nSmallestTrain
    nBiggestTest <- nTotalSample - (nSmallestTest + nSmallestTrain + nBiggestTrain)
    
    nTrainingSample <- nSmallestTrain + nBiggestTrain
    nTestSample <- nSmallestTest + nBiggestTest
  
  }
  
  totalSampleAllRuns <- matrix(0, nrow = nRuns, ncol = nTotalSample)
  trainingSampleAllRuns <- matrix(0, nrow = nRuns, ncol = nTrainingSample)
  testSampleAllRuns   <- matrix(0, nrow = nRuns, ncol = nTestSample)
  
  indicesTrainingSample <- matrix(NA, nrow = nRuns, ncol = nTotalSample)
  
  for (irun in 1:nRuns) {
    indicesTrainingSample[irun, ] <- inTrainingSample(pData(eset)[, classVar],
        propTraining = propTraining, classdist = classdist)
    trainingSampleAllRuns[irun, ] <- which(indicesTrainingSample[irun, ])
    testSampleAllRuns[irun, ]   <- which(!indicesTrainingSample[irun, ])
  }
	
	### output data structure for features used by the classifiers
	feat.outp <- vector(length = nRuns, mode = "list")
	
	### output data structure for errors and predicted classes
	
	## list nested in first level
	featuresList <- vector(length = length(nFeatures), mode = "list")
	names(featuresList) <- paste("nfeat", nFeatures, sep = "")
	
	## list nested in second level
	resultList <- if (storeTestScores){
				list(labelsMat = matrix(NA, nrow = nRuns, ncol = nTotalSample)
						, testScoresMat = matrix(NA, nrow = nRuns, ncol = nTotalSample)
						, errorRate = rep(NA, nRuns)
						, AUC = rep(NA, nRuns)
						, ROC = vector(mode = "list", length = nRuns)) # for performance objects
			} else  {
				list(labelsMat = matrix(NA, nrow = nRuns, ncol = nTotalSample)
						, errorRate = rep(NA, nRuns)
						, AUC = rep(NA, nRuns)
						, ROC = vector(mode = "list", length = nRuns)) # for performance objects
			}
	## nest second level in first level
	featuresList <- lapply(featuresList, function(x){x <- resultList; return(x)})
	
	## create output data structure
	output <- vector(length = length(classifMethods), mode = "list") 
	names(output) <- classifMethods
	output <- lapply(output, function(x){x <- featuresList; return(x)})
	
	for (irun in 1:nRuns){
		
		trainingSampleRun <- trainingSampleAllRuns[irun, ]
		testSampleRun <- testSampleAllRuns[irun, ]
		
		### Feature Selection
		
		switch(fsMethod,
				none = {
					
					orderedEset <- eset
					fNames <- featureNames(eset)
					featRF <- rep(1, length(fNames))
					names(featRF) <- fNames
					feat.outp[[irun]] <- featRF
					
					# nothing is done
				},
				randomForest = {
					
					# allow for customized use
					mtry <- if (is.null(fsPar$mtry)) as.integer(sqrt(length(initialGenes)))
							else fsPar$mtry
#          if (mtry > max(nFeatures))
#            stop("'mtry' component of 'fsPar' cannot exceed max(nFeatures)")
					ntree <- if (is.null(fsPar$ntree)) 500
							else fsPar$ntree
					
		          rf <- randomForest(x = t(exprs(eset[initialGenes, ])),
		              y = pData(eset)[,classVar], 
		              mtry= mtry,
		              importance=TRUE)
		          
		          importanceRf <- importance(rf)
		        
		          # TV: MLearn method blows up memory for some reason :-(
		      
		#          rf <- MLearn(formula = as.formula(paste(classVar, "~ .", sep = " ")),
		#              data = eset[initialGenes, ], 
		#              trainInd = as.integer(trainingSampleRun),
		#              .method = randomForestI,
		#              importance = TRUE,
		#              ntree = ntree,
		#              mtry = mtry)
		          
		          # order features from highest to lowest variable importance
		          orderedGenesRF <- order(importanceRf[, 3], decreasing = TRUE)
		          orderedEset <- eset[orderedGenesRF, ]
		          featRF <- importanceRf[orderedGenesRF, 3]
		          feat.outp[[irun]] <- featRF},
				
				t.test = {
					
					fsPar$test <- if (is.null(fsPar$test)) "f" else fsPar$test
					
					yl.num      <- as.numeric(factor(pData(eset)[trainingSampleRun, classVar])) - 1
					xl.mtt      <- exprs(eset[, trainingSampleRun])
					f.stat      <- mt.teststat(xl.mtt, yl.num, fsPar$test) # "f" by default
					orderedGenesTT <- order(f.stat, decreasing = TRUE)
					
					orderedEset <- eset[orderedGenesTT, ]
					featTT <- f.stat[orderedGenesTT]
					names(featTT) <- rownames(exprs(eset))[orderedGenesTT]
					feat.outp[[irun]] <- featTT},
				
				limma = {
					
					limmaTopTable <- limmaTwoGroups(eset[, trainingSampleRun],
							group = classVar) 
					          limmaTopTableGeneIDs <- as.character(limmaTopTable[, "ROWNAMES"])
#          if (geneID == "ENTREZID"){
#            # limmaTopTableGeneIDs <- limmaTopTableGeneIDs[!is.na(limmaTopTableGeneIDs)]
#            limmaTopTableGeneIDs <- paste(limmaTopTableGeneIDs, "_at", sep = "")
#          }
          
		          orderedEset <- eset[limmaTopTableGeneIDs, ] # features sorted by increasing P-values
		          featLimma <- limmaTopTable$P.Value
		          names(featLimma) <- limmaTopTableGeneIDs
							feat.outp[[irun]] <- featLimma
						
			}) # end of switch
		
		### Classification    
		for (iNumber in seq(along = nFeatures)){
			
			## use first nFeatures[iNumber]] rows (i.e. features)
			nFeaturesEset <- orderedEset[1:nFeatures[iNumber], ] 
			
			## wrapper for all classification algorithms      
			results  <- 
				# make conditional on BioC release    
				classify_bioc_2.3(nFeaturesEset, 
						trainingSample = trainingSampleRun,
						testSample = testSampleRun,
						classVar = classVar,
						classifMethodList = classifMethodList)
			
			## extract errors, predicted classes and (for continuous outputs) predicted values
			classVarLevels <- levels(pData(eset)[[classVar]])
			
			for (iMethod in classifMethods){  
				
				# MCR
				output[[iMethod]][[iNumber]][["errorRate"]][irun] <- results[[iMethod]]
				
				# predicted values
				iPredic <- paste(iMethod, "predic", sep = ".")
				if (!(iPredic %in% c("glm.predic", "nlda.predic"))){
					output[[iMethod]][[iNumber]][["labelsMat"]][irun,!indicesTrainingSample[irun, ]] <-
							as.character(testPredictions(results[[iPredic]]))
				} else {
					output[[iMethod]][[iNumber]][["labelsMat"]][irun,!indicesTrainingSample[irun, ]] <-
							classVarLevels[as.numeric(testPredictions(results[[iPredic]]))]
					
					# test scores  
					if (storeTestScores){
						output[[iMethod]][[iNumber]][["testScoresMat"]][irun,!indicesTrainingSample[irun, ]] <-
								testScores(results[[iPredic]])
					}
					
					# AUC
					iAUC <- paste(iMethod, "AUC", sep = ".")
					output[[iMethod]][[iNumber]][["AUC"]][irun] <- results[[iAUC]]
					
					# ROC curve (performance object)
					iROC <- paste(iMethod, "ROC", sep = ".")
					output[[iMethod]][[iNumber]][["ROC"]][irun] <- results[[iROC]]
				}
				
			}  
		} 
	}
	
	res <- list(output = output, features = feat.outp) #, trueClasses = pData(eset)[[classVar]]
	attr(res, "nFeatures") <- nFeatures
	tmpClassVar <- pData(eset)[, classVar]
	names(tmpClassVar) <- sampleNames(eset)
	attr(res, "classVar") <- tmpClassVar 
	class(res) <- "nlcv"
	return(res)
}
