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



context("Testing Hosmer-Lemeshow Implementation")

example <- UtilStat::exampleRecruitment

hTest <- hosmerLemeshowTest(example$occurred, example$pred)

test_that("Testing results of Hosmer-Lemeshow test", {
  expect_equal(hTest$chi2, 28.48646, tolerance = 1E-5)
  expect_equal(hTest$pValue, 0.0003900657, tolerance = 1E-5)
})

context("Testing Gauss-Hermite quadrature")

exampleFitGLMER <- UtilStat::fitGLMER

margProb <- glmmPA(exampleFitGLMER)
test_that("Testing marginal probabilities from Gauss-Hermite", {
  expect_equal(margProb[1,"meanPA"], 0.24252764771389884, tolerance = 1E-8)
})


