########################################################
# Home made tests for J4R
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: January 2019
########################################################


context("Testing variable description")

example <- UtilStat::exampleRecruitment

a <- summarizeThisVariable(example, "G_TOT", "speciesGr")
b <- summarizeThisVariable(example, "G_TOT", "speciesGr", format = T, nbDigits = 1)

test_that("Testing results of variable description", {
  expect_equal(a$n.G_TOT, 26248, tolerance = 1E-5)
  expect_equal(a$mean.G_TOT, 16.87469, tolerance = 1E-5)
  expect_equal(a$min.G_TOT, 0)
  expect_equal(a$max.G_TOT, 64.13471, tolerance = 1E-5)
  expect_equal(b$formatted.G_TOT, "16.9 (0.0,64.1)")
})

context("Testing predicted probabilities against observed proportions")

example <- UtilStat::exampleRecruitment

predProb <- UtilStat::getPredictedProbsAndObservedProps(example, example$pred, "G_TOT", 2, "occurred")

test_that("Testing results of Hosmer-Lemeshow test", {
  expect_equal(predProb[10,"roundVar"], 18, tolerance = 1E-5)
  expect_equal(predProb[10,"n"], 1800, tolerance = 1E-5)
  expect_equal(predProb[10,"pred"], 0.0355889, tolerance = 1E-5)
  expect_equal(predProb[10,"obs"], 0.03166667, tolerance = 1E-5)
})

context("Testing Hosmer-Lemeshow Implementation")

example <- UtilStat::exampleRecruitment

hTest <- hosmerLemeshowTest(example$occurred, example$pred)

test_that("Testing results of Hosmer-Lemeshow test", {
  expect_equal(hTest$chi2, 28.48646, tolerance = 1E-5)
  expect_equal(hTest$pValue, 0.0003900657, tolerance = 1E-5)
})

context("Testing Gauss-Hermite quadrature")

# exampleFitGLMER <- UtilStat::fitGLMER
#
# margProb <- glmmPA(exampleFitGLMER)
# test_that("Testing marginal probabilities from Gauss-Hermite", {
#   expect_equal(margProb[1,"meanPA"], 0.24252764771389884, tolerance = 1E-8)
# })


meanPA <- getGaussHermiteQuadApproximation(function(u) {return(exp(u))}, 1)
expected <- exp(0.5)
test_that("Testing Gauss-Hermite approximation with exp(u)", {
  expect_equal(expected, meanPA, tolerance = 1E-4)
})

meanPA <- getGaussHermiteQuadApproximation(function(u) {return(exp(u))}, 2)
expected <- exp(1)
test_that("Testing Gauss-Hermite approximation with exp(u)", {
  expect_equal(expected, meanPA, tolerance = 2E-3)
})

meanPA <- getGaussHermiteQuadApproximation(function(u) {return(u^2)}, 1)
expected <- 1
test_that("Testing Gauss-Hermite approximation with u^2", {
  expect_equal(expected, meanPA, tolerance = 1E-6)
})

meanPA <- getGaussHermiteQuadApproximation(function(u) {return(u^2)}, 2)
expected <- 2
test_that("Testing Gauss-Hermite approximation with u^2", {
  expect_equal(expected, meanPA, tolerance = 1E-6)
})

meanPA <- getGaussHermiteQuadApproximation(function(u, a) {return(exp(a + u))}, 1, 2)
expected <- exp(2 + 0.5)
test_that("Testing Gauss-Hermite approximation with exp(a + u)", {
  expect_equal(expected, meanPA, tolerance = 1E-3)
})

meanPA <- getGaussHermiteQuadApproximation(function(u, a) {return((a + u)^2)}, 1, 2)
expected <- 2^2 + 1
test_that("Testing Gauss-Hermite approximation with (a + u)^2", {
  expect_equal(expected, meanPA, tolerance = 1E-6)
})


