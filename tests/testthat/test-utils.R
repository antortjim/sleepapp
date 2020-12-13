context("utils")

test_that("annotate_mitochondrial_genes works", {
  sce <- readRDS(system.file(package = "sleepapp", "sce.rds"))
  sce <- annotate_mitochondrial_genes(sce)
})