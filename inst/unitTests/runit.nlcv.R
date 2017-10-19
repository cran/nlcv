# Unit Tests for the nlcv package
# 
# Tobias Verbeke
###############################################################################

### test on ALL data

test.nlcv <- function(){
  
  require("ALL") # example data
  data(ALL)
  # create B-Cell subset for ALL and turn into a two-class problem
  BCellSubset <- intersect(grep("^B", as.character(ALL$BT)),
      which(ALL$mol %in% c("BCR/ABL","NEG"))) 
  beset <- ALL[, BCellSubset]
  beset$mol.biol <- beset$mol.biol[,drop=TRUE]
  
  t.test_test_b <- nlcv(eset = beset, classVar = "mol.biol",
      fsMethod = "t.test")
  
  t.test_test_u <- nlcv(eset = beset, classVar = "mol.biol",
      fsMethod = "t.test", classdist = "unbalanced")
  
  limma_test_b <- nlcv(eset = beset, classVar = "mol.biol",
      fsMethod = "limma")
  
  limma_test_u <- nlcv(eset = beset, classVar = "mol.biol",
      fsMethod = "limma", classdist = "unbalanced")
  
  randomForest_test_b <- nlcv(eset = beset, classVar = "mol.biol",
       fsMethod = "randomForest")
   
  randomForest_test_u <- nlcv(eset = beset, classVar = "mol.biol",
       fsMethod = "randomForest", classdist = "unbalanced")
  return(randomForest_test_u)
}

test.topTable <- function(){
  require(nlcv)
  require(randomForest)
  require("ALL") # example data
  data(ALL)
  # create B-Cell subset for ALL and turn into a two-class problem
  BCellSubset <- intersect(grep("^B", as.character(ALL$BT)),
      which(ALL$mol %in% c("BCR/ABL","NEG"))) 
  beset <- ALL[, BCellSubset]
  beset$mol.biol <- beset$mol.biol[,drop=TRUE]
  nlcvObj <- nlcv(eset = beset, classVar = "mol.biol",
      fsMethod = "randomForest", classdist = "unbalanced")
  # checkException(topTable(nlcvObj)) # no n by default
  topTable(nlcvObj, n = 15)
}

