library(testthat)

context("comparisons")

test_that("readComparisonFile works", {
  
  comparison_file <- "../comparisons/ZT20 sleep + ZT20 midline cross vs. ZT20 sleep deprivation + ZT8 wake + ZT8 wake stimulation + ZT2 14h SD + ZT8 20h SD.csv"
  comparison <- readComparisonFile(comparison_file)
  expect_s3_class(comparison, "data.frame")
  
})