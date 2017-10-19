### R code from vignette source 'nlcv.Rnw'

###################################################
### code chunk number 1: init
###################################################

	if(!dir.exists("./graphs"))	dir.create("./graphs")
	


###################################################
### code chunk number 2: Setting
###################################################
  options(width=65)
  set.seed(123)


###################################################
### code chunk number 3: LoadLib
###################################################
library(nlcv)


###################################################
### code chunk number 4: Simulation
###################################################
EsetRandom <- simulateData(nCols = 40, nRows = 1000, nEffectRows = 0, nNoEffectCols = 0)


###################################################
### code chunk number 5: Simulation
###################################################
EsetStrongSignal <- simulateData(nCols = 40, nRows = 1000, nEffectRows = 10, 
	nNoEffectCols = 0, betweenClassDifference = 3, withinClassSd = 0.5)


###################################################
### code chunk number 6: Simulation
###################################################
EsetWeakSignal <- simulateData(nCols = 40, nRows = 1000, nEffectRows = 5, 
	nNoEffectCols = 0, betweenClassDifference = 1, withinClassSd = 0.6)


###################################################
### code chunk number 7: Simulation
###################################################
EsetStrongHeteroSignal <- simulateData(nCols = 40, nRows = 1000, nEffectRows = 5, 
	nNoEffectCols = 5, betweenClassDifference = 3, withinClassSd = 0.5)


###################################################
### code chunk number 8: Simulation
###################################################
EsetWeakHeteroSignal <- simulateData(nCols = 40, nRows = 1000, nEffectRows = 5, 
	nNoEffectCols = 5, betweenClassDifference = 1, withinClassSd = 0.6)


###################################################
### code chunk number 9: Simulation
###################################################
		geneX <- 1
		myData <- EsetStrongHeteroSignal
		xx <- pData(myData)$type
		yy <- exprs(myData)[geneX,]
		myTitle <- rownames(exprs(myData))[geneX]
	pdf(file = "./graphs/plotGeneSHS.pdf")
		boxplot(yy~xx,col='grey',xlab='',ylab='', main = myTitle, axes=FALSE)
		text(xx,yy,labels=colnames(exprs(myData)),col='blue',pos=4,cex=0.7)
		axis(1, at=1:2, labels=levels(xx));axis(2, las=2)
	dev.off()


###################################################
### code chunk number 10: nlcv (eval = FALSE)
###################################################
## 	nlcvTT_SS <- nlcv(EsetStrongSignal, classVar = "type", nRuns = 2, 
## 		fsMethod = "t.test", verbose = TRUE)


###################################################
### code chunk number 11: nlcv load_objects_20runs
###################################################
  # No Signal - Random data
  data("nlcvRF_R"); data("nlcvTT_R")
  # Strong Signal
  data("nlcvRF_SS"); data("nlcvTT_SS")
  # Weak Signal
  data("nlcvRF_WS"); data("nlcvTT_WS")
  # Strong, heterogeneous Signal
  data("nlcvRF_SHS"); data("nlcvTT_SHS")
  # Weak, heterogeneous Signal
  data("nlcvRF_WHS"); data("nlcvTT_WHS")


###################################################
### code chunk number 12: nlcv run_objects_20runs
###################################################
#	# Sidenote:  nlcvRF_SS (loaded in the previous chunk) was obtained with following code
#	nlcvRF_SS <- nlcv(EsetStrongSignal, classVar = "type", nRuns = 20, fsMethod = "randomForest", verbose = TRUE)
#	save(nlcvRF_SS, file = "nlcvRF_SS.rda")
#	nlcvTT_SS <- nlcv(EsetStrongSignal, classVar = "type", nRuns = 20, fsMethod = "t.test", verbose = TRUE)
#	save(nlcvTT_SS, file = "nlcvTT_SS.rda")
#	
#	Similarly for any other dataset, like EsetWeakSignal, WeakHeteroSignal, StrongHeteroSignal and EsetRandom


###################################################
### code chunk number 13: mcrPlot_RandomData
###################################################
# plot MCR versus number of features
pdf(file = "./graphs/mcrPlot_nlcv_R.pdf", width = 10, height = 5)
  layout(matrix(1:4, ncol = 2), height = c(6, 1, 6, 1))
  mcrPlot_RF_R <- mcrPlot(nlcvRF_R, plot = TRUE, optimalDots = TRUE, 
	layout = FALSE, main = 'RF selection')
  mcrPlot_TT_R <- mcrPlot(nlcvTT_R, plot = TRUE, optimalDots = TRUE, 
 	layout = FALSE, main = 'T selection')
  layout(1)
dev.off()


###################################################
### code chunk number 14: scoresPlot_RandomData
###################################################
pdf(file = "./graphs/ScoresPlot_nlcv_R.pdf", width = 10, height = 6)
scoresPlot(nlcvRF_R, "randomForest", 5)
dev.off()


###################################################
### code chunk number 15: selGenes
###################################################
outtable <- topTable(nlcvRF_R, n = 10)
xtable(outtable, label = "tab:selGenes_R",
		caption="Top 10 features across all runs of the nested loop cross-validation.")


###################################################
### code chunk number 16: Simulation
###################################################
		geneX <- 1
		myData <- EsetStrongSignal
		xx <- pData(myData)$type
		yy <- exprs(myData)[geneX,]
		myTitle <- rownames(exprs(myData))[geneX]
	pdf(file = "./graphs/plotGeneSS.pdf")
	boxplot(yy~xx,col='grey',xlab='',ylab='', main = myTitle, axes=FALSE)
	text(xx,yy,labels=colnames(exprs(myData)),col='blue',pos=4,cex=0.7)
	axis(1, at=1:2, labels=levels(xx));axis(2, las=2)
		dev.off()


###################################################
### code chunk number 17: RandomData
###################################################
# plot MCR versus number of features
pdf(file = "./graphs/mcrPlot_nlcv_SS.pdf", width = 10, height = 5)
  layout(matrix(1:4, ncol = 2), height = c(6, 1, 6, 1))
  mcrPlot_SSF_SS <- mcrPlot(nlcvRF_SS, plot = TRUE, optimalDots = TRUE, layout = FALSE, main = 'RF selection')
  mcrPlot_TT_SS <- mcrPlot(nlcvTT_SS, plot = TRUE, optimalDots = TRUE, layout = FALSE, main = 'T selection')
dev.off()


###################################################
### code chunk number 18: RandomData
###################################################
pdf(file = "./graphs/ScoresPlot_nlcv_SS.pdf", width = 10, height = 6)
scoresPlot(nlcvRF_SS, "randomForest", 5)
dev.off()


###################################################
### code chunk number 19: selGenes
###################################################
outtable <- topTable(nlcvRF_SS, n = 12)
xtable(outtable, label = "tab:selGenes_SS",
		caption="Top 20 features across all runs of the nested loop cross-validation.")


###################################################
### code chunk number 20: Simulation
###################################################
		geneX <- 1
		myData <- EsetWeakSignal
		xx <- pData(myData)$type
		yy <- exprs(myData)[geneX,]
		myTitle <- rownames(exprs(myData))[geneX]
	pdf(file = "./graphs/plotGeneWS.pdf")
	boxplot(yy~xx,col='grey',xlab='',ylab='', main = myTitle, axes=FALSE)
	text(xx,yy,labels=colnames(exprs(myData)),col='blue',pos=4,cex=0.7)
	axis(1, at=1:2, labels=levels(xx));axis(2, las=2)
		dev.off()


###################################################
### code chunk number 21: RandomData
###################################################
# plot MCR versus number of features
pdf(file = "./graphs/mcrPlot_nlcv_WS.pdf", width = 10, height = 5)
  layout(matrix(1:4, ncol = 2), height = c(6, 1, 6, 1))
  mcrPlot_WSF_WS <- mcrPlot(nlcvRF_WS, plot = TRUE, optimalDots = TRUE, layout = FALSE, main = 'RF selection')
  mcrPlot_TT_WS <- mcrPlot(nlcvTT_WS, plot = TRUE, optimalDots = TRUE, layout = FALSE, main = 'T selection')
dev.off()


###################################################
### code chunk number 22: ScoresPlot_nlcv_WS
###################################################
  pdf(file = "./graphs/ScoresPlot_nlcv_WS.pdf", width = 10, height = 6)
    scoresPlot(nlcvRF_WS, "svm", 7)
  dev.off()


###################################################
### code chunk number 23: selGenesNlcvTT_WS
###################################################
outtable <- topTable(nlcvTT_WS, n = 7)
xtable(outtable, label = "tab:selGenes_WS1",
		caption="Top 20 features selected with t-test across all runs of the nested loop cross-validation.")


###################################################
### code chunk number 24: selGenesNlcvRF_WS
###################################################
outtable <- topTable(nlcvRF_WS, n = 7)
xtable(outtable, label = "tab:selGenes_WS2",
		caption="Top 20 features selected with RF variable importance across all runs of the nested loop cross-validation.")


###################################################
### code chunk number 25: Simulation
###################################################
		geneX <- 1
		myData <- EsetStrongHeteroSignal
		xx <- pData(myData)$type
		yy <- exprs(myData)[geneX,]
		myTitle <- rownames(exprs(myData))[geneX]
	pdf(file = "./graphs/plotGeneSHS.pdf")
	boxplot(yy~xx,col='grey',xlab='',ylab='', main = myTitle, axes=FALSE)
	text(xx,yy,labels=colnames(exprs(myData)),col='blue',pos=4,cex=0.7)
	axis(1, at=1:2, labels=levels(xx));axis(2, las=2)
		dev.off()


###################################################
### code chunk number 26: mcrPlot_nlcv_SHS
###################################################
# plot MCR versus number of features
pdf(file = "./graphs/mcrPlot_nlcv_SHS.pdf", width = 10, height = 5)
  layout(matrix(1:4, ncol = 2), height = c(6, 1, 6, 1))
  mcrPlot_SHSF_SHS <- mcrPlot(nlcvRF_SHS, plot = TRUE, optimalDots = TRUE, layout = FALSE, main = 'RF selection')
  mcrPlot_TT_SHS <- mcrPlot(nlcvTT_SHS, plot = TRUE, optimalDots = TRUE, layout = FALSE, main = 'T selection')
dev.off()


###################################################
### code chunk number 27: scoresPlots
###################################################
pdf(file = "./graphs/ScoresPlot_nlcv_SHS.pdf", width = 10, height = 6)
  scoresPlot(nlcvTT_SHS, "pam", 7)
dev.off()
pdf(file = "./graphs/ScoresPlot_nlcv_SHS2.pdf", width = 10, height = 6)
  scoresPlot(nlcvTT_SHS, "randomForest", 7)
dev.off()
pdf(file = "./graphs/ScoresPlot_nlcv_SHS3.pdf", width = 10, height = 6)
  scoresPlot(nlcvRF_SHS, "randomForest", 7)
dev.off()


###################################################
### code chunk number 28: selGenes
###################################################
outtable <- topTable(nlcvTT_SHS, n = 7)
xtable(outtable, label = "tab:selGenes_SHS1",
		caption="Top 20 features selected with t-test across all runs of the nested loop cross-validation.")


###################################################
### code chunk number 29: selGenes
###################################################
outtable <- topTable(nlcvRF_SHS, n = 7)
xtable(outtable, label = "tab:selGenes_SHS2",
		caption="Top 20 features selected with RF variable importance across all runs of the nested loop cross-validation.")


###################################################
### code chunk number 30: Simulation
###################################################

geneX <- 1:4
myData <- EsetWeakHeteroSignal
xx <- pData(myData)$type
pdf(file = "./graphs/plotGeneWHS.pdf")
par(mfrow=c(2,2))
for (i in 1:4){
	yy <- exprs(myData)[geneX[i],]
	myTitle <- rownames(exprs(myData))[geneX[i]]
boxplot(yy~xx,col='grey',xlab='',ylab='', main = myTitle, axes=FALSE)
	text(xx,yy,labels=colnames(exprs(myData)),col='blue',pos=4,cex=0.85)
	axis(1, at=1:2, labels=levels(xx));axis(2, las=2)
}
par(mfrow=c(1,1))
		dev.off()


###################################################
### code chunk number 31: RandomData
###################################################
# plot MCR versus number of features
pdf(file = "./graphs/mcrPlot_nlcv_WHS.pdf", width = 10, height = 5)
  layout(matrix(1:4, ncol = 2), height = c(6, 1, 6, 1))
  mcrPlot_WHSF_WHS <- mcrPlot(nlcvRF_WHS, plot = TRUE, optimalDots = TRUE, layout = FALSE, main = 'RF selection')
  mcrPlot_TT_WHS <- mcrPlot(nlcvTT_WHS, plot = TRUE, optimalDots = TRUE, layout = FALSE, main = 'T selection')
dev.off()


###################################################
### code chunk number 32: RandomData
###################################################
pdf(file = "./graphs/ScoresPlot_nlcv_WHS.pdf", width = 10, height = 6)
scoresPlot(nlcvTT_WHS, "pam", 2)
dev.off()
pdf(file = "./graphs/ScoresPlot_nlcv_WHS0.pdf", width = 10, height = 6)
scoresPlot(nlcvTT_WHS, "pam", 10)
dev.off()
pdf(file = "./graphs/ScoresPlot_nlcv_WHS2.pdf", width = 10, height = 6)
scoresPlot(nlcvTT_WHS, "randomForest", 15)
dev.off()
pdf(file = "./graphs/ScoresPlot_nlcv_WHS3.pdf", width = 10, height = 6)
scoresPlot(nlcvRF_WHS, "randomForest", 5)
dev.off()


###################################################
### code chunk number 33: selGenes
###################################################
outtable <- topTable(nlcvTT_WHS, n = 10)
xtable(outtable, label = "tab:selGenes_WHS1",
		caption="Top 20 features selected with t-test across all runs of the nested loop cross-validation.")


###################################################
### code chunk number 34: selGenes
###################################################
outtable <- topTable(nlcvRF_WHS, n = 10)
xtable(outtable, label = "tab:selGenes_WHS2",
		caption="Top 20 features selected with RF variable importance across all runs of the nested loop cross-validation.")


###################################################
### code chunk number 35: sessionInfo
###################################################
  toLatex(sessionInfo())


