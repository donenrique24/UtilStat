########################################################
# Tests for UtilStat
# Author: Mathieu Fortin, Canadian Forest Service
# Date: May 2025
########################################################

context("Testing linear model assessment functions")

require(UtilStat)

fit <- lm(y ~ lnDbh + lnDbh2, UtilStat::dataHeight)
plot <- UtilStat::getResidPlotAgainst(fit, UtilStat::dataHeight, 1, printout = T, cutoff = 10)

plotData <- plot$plot_env$tmp
test_that("Testing underlying data.frame object", {
  expect_equal(nrow(plotData), 27)
  expect_equal(min(plotData$clVar), 3)
  expect_equal(max(plotData$clVar), 31)
  expect_equal(plotData$resid[1], 0.204873036, tolerance = 1E-8)
})

plot <- UtilStat::getResidPlotAgainst(fit, UtilStat::dataHeight, fieldname = "BAL", 2, printout = T)
plotData <- plot$plot_env$tmp
test_that("Testing underlying data.frame object", {
  expect_equal(nrow(plotData), 40)
  expect_equal(min(plotData$clVar), 0)
  expect_equal(max(plotData$clVar), 112)
  expect_equal(plotData$resid[1], -0.517593961, tolerance = 1E-8)
})

