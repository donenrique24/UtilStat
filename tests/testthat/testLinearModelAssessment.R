########################################################
# Tests for UtilStat
# Author: Mathieu Fortin, Canadian Forest Service
# Date: May 2025
########################################################

context("Testing linear model assessment functions")

require(UtilStat)

heightData <- UtilStat::dataHeight
fit <- lm(y ~ lnDbh + lnDbh2, heightData)
plot <- UtilStat::getResidPlotAgainst(fit, UtilStat::dataHeight, "pred", 1, printout = T, cutoff = 10)

plotData <- plot$plot_env$tmp
test_that("Testing underlying data.frame object", {
  expect_equal(nrow(plotData), 26)
  expect_equal(min(plotData$clVar), 3)
  expect_equal(max(plotData$clVar), 31)
  expect_equal(plotData$res[1], -0.0814085694, tolerance = 1E-8)
})

plot <- UtilStat::getResidPlotAgainst(fit, UtilStat::dataHeight, "BAL", 2, printout = T)
plotData <- plot$plot_env$tmp
test_that("Testing underlying data.frame object", {
  expect_equal(nrow(plotData), 40)
  expect_equal(min(plotData$clVar), 0)
  expect_equal(max(plotData$clVar), 112)
  expect_equal(plotData$res[1], -0.51731305, tolerance = 1E-8)
})

out <- tryCatch(
  {
    UtilStat::getResidPlotAgainst(fit, UtilStat::dataHeight, "USA_CD", 1)
    return("FAILED - Should have thrown an exception!")
  },
  error = function(cond) {
    return("SUCCESS - Threw an exception as expected!")
  },
  warning = function(cond) {
    return("FAILED - Should have thrown an exception!")
  }
)

test_that("Testing if exception was rightfully thrown", {
  expect_equal(startsWith(out, "SUCCESS"), T)
})


plot <- UtilStat::getResidPlotAgainst(fit, UtilStat::dataHeight, "USA_CD", printout = T)
plotData <- plot$plot_env$tmp
test_that("Testing underlying data.frame object", {
  expect_equal(nrow(plotData), 2)
  expect_equal(plotData$clVar[1], "HALES")
  expect_equal(plotData$clVar[2], "LADE2")
  expect_equal(plotData$res[1], 0.3730116, tolerance = 1E-6)
})
