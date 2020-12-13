context("loom")
library(testthat)
# TODO Need to put the long paths into package
test_that("filter_sce works", {
  
  sce <- readRDS(system.file(package = "sleepapp", "sce.rds"))
  barcodes_file <- "/1TB/Cloud/Lab/Projects/SleepSignature/workflow/results/20201212/select_cells/y_KCs/y_KCs_barcodes.txt"
  filtered_sce <- filter_sce(sce, barcodes_file)
  expect_true(all(filtered_sce$CellID %in% read.table(barcodes_file)[, 1]))
})

test_that("loom2sce works", {
  # TODO
  sce <- loom2sce("/1TB/Cloud/Lab/Projects/SleepSignature/data/20201116/20201116_a_b_KCs_No_ZT2_Wake.loom")
})