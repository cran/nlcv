###########################################################
# Script used to generate the data in the data/ directory #
###########################################################

library(nlcv)

set.seed(428)
nRuns <- 20  

### generate different ExpressionSet objects
EsetRandom <- simulateData(nCols = 40, nRows = 1000, nEffectRows = 0, nNoEffectCols = 0)
EsetStrongSignal <- simulateData(nCols = 40, nRows = 1000, nEffectRows = 10, nNoEffectCols = 0,
    betweenClassDifference = 3, withinClassSd = 0.5)
EsetWeakSignal <- simulateData(nCols = 40, nRows = 1000, nEffectRows = 5, nNoEffectCols = 0,
    betweenClassDifference = 1, withinClassSd = 0.6)
EsetStrongHeteroSignal <- simulateData(nCols = 40, nRows = 1000, nEffectRows = 5, nNoEffectCols = 5,
    betweenClassDifference = 3, withinClassSd = 0.5)
EsetWeakHeteroSignal <- simulateData(nCols = 40, nRows = 1000, nEffectRows = 5, nNoEffectCols = 5,
    betweenClassDifference = 1, withinClassSd = 0.6)

### generate nlcv objects for t test feature selection
nlcvTT_R <- nlcv(EsetRandom, classVar = "type", nRuns = nRuns,
    fsMethod = "t.test", verbose = TRUE)
save(nlcvTT_R, file = "../data/nlcvTT_R.rda", compress='xz')
nlcvTT_SHS <- nlcv(EsetStrongHeteroSignal, classVar = "type", nRuns = nRuns, 
    fsMethod = "t.test", verbose = TRUE)
save(nlcvTT_SHS, file = "../data/nlcvTT_SHS.rda", compress='xz')
nlcvTT_SS <- nlcv(EsetStrongSignal, classVar = "type", nRuns = nRuns, 
    fsMethod = "t.test", verbose = TRUE)
save(nlcvTT_SS, file = "../data/nlcvTT_SS.rda", compress='xz')
nlcvTT_WHS <- nlcv(EsetWeakHeteroSignal, classVar = "type", nRuns = nRuns, 
    fsMethod = "t.test", verbose = TRUE)
save(nlcvTT_WHS, file = "../data/nlcvTT_WHS.rda", compress='xz')
nlcvTT_WS <- nlcv(EsetWeakSignal, classVar = "type", nRuns = nRuns, 
    fsMethod = "t.test", verbose = TRUE)
save(nlcvTT_WS, file = "../data/nlcvTT_WS.rda", compress='xz')

### generate nlcv objects for random forest feature selection
nlcvRF_R <- nlcv(EsetRandom, classVar = "type", nRuns = nRuns, 
    fsMethod = "randomForest", verbose = TRUE)
save(nlcvRF_R, file = "../data/nlcvRF_R.rda", compress='xz')
nlcvRF_SHS <- nlcv(EsetStrongHeteroSignal, classVar = "type", nRuns = nRuns, 
    fsMethod = "randomForest", verbose = TRUE)
save(nlcvRF_SHS, file = "../data/nlcvRF_SHS.rda", compress='xz')
nlcvRF_SS <- nlcv(EsetStrongSignal, classVar = "type", nRuns = nRuns, 
    fsMethod = "randomForest", verbose = TRUE)
save(nlcvRF_SS, file = "../data/nlcvRF_SS.rda", compress='xz')
nlcvRF_WHS <- nlcv(EsetWeakHeteroSignal, classVar = "type", nRuns = nRuns, 
    fsMethod = "randomForest", verbose = TRUE)
save(nlcvRF_WHS, file = "../data/nlcvRF_WHS.rda", compress='xz')
nlcvRF_WS <- nlcv(EsetWeakSignal, classVar = "type", nRuns = nRuns, 
    fsMethod = "randomForest", verbose = TRUE)
save(nlcvRF_WS, file = "../data/nlcvRF_WS.rda", compress='xz')
