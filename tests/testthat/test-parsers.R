library(testthat)

context("getters")

test_that("getMethod works", {
  path <- "edgeR/y_KCs/y_KCs_assay-normcounts_comparison-ZT8 Gab vs. ZT8 20h SD_grouping-Condition_diff_table.csv"
  expect_equal(parseMethod(path), "edgeR")
})


test_that("getComparison works", {
   path <- "edgeR/y_KCs/y_KCs_assay-normcounts_comparison-ZT8 Gab vs. ZT8 20h SD_grouping-Condition_diff_table.csv"
   expect_equal(parseComparison(path), "ZT8 Gab vs. ZT8 20h SD_grouping-Condition_diff_table")
})

test_that("getCellType works", {
  path <- "edgeR/y_KCs/y_KCs_assay-normcounts_comparison-ZT8 Gab vs. ZT8 20h SD_grouping-Condition_diff_table.csv"
  expect_equal(parseCellType(path), "y_KCs")
})
