library(testthat)

context("plotHist")

test_that("plotHist works", {
  ROOT_DIR <-  "/1TB/Cloud/Lab/Projects/SleepSignature/workflow/"
  WORKING_DIR <-  "results/20201126/"
  library(scuttle)
  
  grouping <- "Condition"
  path <- build_sce_rds_path(cell_type = "y_KCs", grouping = grouping, normalized = TRUE)
  sce <- readRDS(path)
  comparison_file <- "ZT20 sleep + ZT20 midline cross vs. ZT20 sleep deprivation"
  absolute_comparison_file <- file.path(ROOT_DIR, "comparisons", paste0(comparison_file, ".csv"))
  
  plotHistogram(sce = sce , gene = "Ddc", comparison_file = absolute_comparison_file, by_exprs_values = "logcounts", geom = "density")
  plotHistogram(sce = sce , gene = c("Ddc", "Act5C", "noe"), comparison_file = absolute_comparison_file, by_exprs_values = "logcounts", geom = "density")
  plotHistogram(sce = sce , gene = c("Ddc", "Act5C", "noe"), comparison_file = absolute_comparison_file, by_exprs_values = "normcounts", geom = "histogram")
})
