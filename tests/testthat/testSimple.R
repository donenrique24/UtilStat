########################################################
# Home made tests for J4R
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: January 2019
########################################################

context("Testing Hosmer-Lemeshow Implementation")

example <- UtilStat::exampleRecruitment

hTest <- hosmerLemeshowTest(example$occurred, example$pred)

test_that("Testing results of Hosmer-Lemeshow test", {
  expect_equal(hTest$chi2, 28.48646, tolerance = 1E-5)
  expect_equal(hTest$pValue, 0.0003900657, tolerance = 1E-5)
})

