library(testthat)

context("path_parsers")

test_that("getComparison works", {
   path <- "edgeR/y_KCs/y_KCs_assay-normcounts_comparison-ZT8 Gab vs. ZT8 20h SD_grouping-Condition_diff_table.csv"
   expect_equal(getComparison(path), "ZT8 Gab vs. ZT8 20h SD_grouping-Condition_diff_table")
})
