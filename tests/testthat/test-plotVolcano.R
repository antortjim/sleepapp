library(testthat)

context("plotVolcano")

test_that("plotVolcano works", {
  
    options(shiny.debug=F)
    ROOT_DIR <-  "/1TB/Cloud/Lab/Projects/SleepSignature/workflow"
    WORKING_DIR <- "results/20201126/"
    comparison_file <- "ZT20 sleep + ZT20 midline cross vs. ZT20 sleep deprivation + ZT8 wake + ZT8 wake stimulation"
  
    readMethods <- list(
      MAST = readMAST,
      edgeR = readEdgeR,
      DESeq = readDESeq,
      Wilcoxon = readWilcoxon,
      scvi = readSCVI_vanilla
    )
    
    
    diff_table_id <- list(
      cell_type = "a_b_KCs", method = "edgeR", assay = "normcounts",
      comparison = comparison_file, grouping = "Condition"
    )
    
    path <- rlang::exec(.fn = build_diff_table_path, !!!diff_table_id)
    diff_table <- readMethods[[diff_table_id$method]](path)
    gg <- rlang::exec(plotVolcano, diff_table, labels = 1, genes=c("Act5C"), !!!diff_table_id)
    
    for (n in names(readMethods)) {
      diff_table_id$method <- n
      print(n)
      if (n %in% c("MAST", "Wilcoxon", "scvi")) diff_table_id$assay <- "counts"
      else if (n %in% c("edgeR", "DESeq")) diff_table_id$assay <- "normcounts"
      path <- rlang::exec(.fn = build_diff_table_path, !!!diff_table_id)
      diff_table <- readMethods[[diff_table_id$method]](path)
      
      gg <- rlang::exec(plotVolcano, diff_table, labels = 1, genes=c("Act5C"), !!!diff_table_id)
      print(gg)
      askYesNo(msg = "OK?")
    }
})
