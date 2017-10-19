# Simple Run-Time Tests for mcrPlot
# 
# Author: Tobias Verbeke
###############################################################################

require(nlcv)
data(nlcvRF_R)
data(nlcvTT_R)

### layout FALSE

layout(matrix(1:4, ncol = 2), height = c(6, 1, 6, 1))
mcrPlot_RF_R <- mcrPlot(nlcvRF_R, plot = TRUE, optimalDots = TRUE, layout = FALSE, main = 'RF selection')
mcrPlot_TT_R <- mcrPlot(nlcvTT_R, plot = TRUE, optimalDots = TRUE, layout = FALSE, main = 'T selection')
layout(1)

plot(1:10)
plot(1:10)

### layout TRUE

mcrPlot_TT <- mcrPlot(nlcvTT_R, plot = TRUE, optimalDots = TRUE,
    layout = TRUE, main = "t-test selection")

plot(1:10)
plot(1:10)
