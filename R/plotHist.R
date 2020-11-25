ROOT_DIR <- "/1TB/Cloud/Lab/Projects/SleepSignature/workflow/"

path <- file.path(ROOT_DIR, "results/20201118/scran/Condition/y_KCs/y_KCs_norm.rds")

source(file.path(ROOT_DIR, "notebooks/04-pseudoBulk.R"))
sce <- readRDS(path)

library(SingleCellExperiment)
library(tidyr)

readComparisonFile <- function(comparison_file) {
  cmd <- paste0('grep -v "#" "', file.path(ROOT_DIR, "comparisons", comparison_file), ".csv", '"')
  print(cmd)
  comparison <- data.table::fread(cmd = cmd)
  print(comparison)
  stopifnot(nrow(comparison) != 0)
  
  return(comparison)
  
}

comparison_file <- "ZT20 sleep + ZT20 midline cross vs. ZT20 sleep deprivation"
# by_exprs_values = "counts"
plotHist <- function(sce, comparison_file, by_exprs_values="counts", gene="Ddc") {
  
  comparison <- readComparisonFile(comparison_file)
  subsce <- label_cells(sce, comparison, comparison_file = comparison_file, remove = TRUE)
  
  stopifnot(gene %in% rowData(sce)$Gene)
            
  counts_matrix <- assay(sce[rowData(sce)$Gene == gene,], by_exprs_values)
  
  counts_df <- as.data.frame(counts_matrix)
  colnames(counts_df) <- "counts"
  counts_df$CellID <- rownames(counts_df)
  rownames(counts_df) <- NULL
  counts_df$Gene <- gene
  head(counts_df)
  

  
  # counts_df_long <- counts_df_long[counts_df_long$value != 0,]
  
  cell_metadata <- as.data.frame(colData(subsce))
  # df <- head(counts_df_long, 1e3)
  df <- counts_df
  cell_metadata <- cell_metadata[, c("CellID", "experiment_")]
  
  df <- dplyr::left_join(df, cell_metadata, by = "CellID")
  df <- df[complete.cases(df),]
  
  gg <- ggplot(data = df, aes(x = counts,
                              col = experiment_,
                              fill = experiment_
                              )) +
    geom_density(alpha = 0.2)
  
  gg <- gg + ggtitle(
    label = glue::glue(
    "{gene}"
    ),
    subtitle = glue::glue(
      "{comparison_file}"
    ))
  gg <- gg + labs(x = by_exprs_values)
  gg
  
}

# comparison_file
# sce
# unique(sce$Condition)

# plotHist(sce, comparison_file, by_exprs_values = "logcounts", gene = "Act5C")
# plotHist(sce, comparison_file, by_exprs_values = "normcounts", gene = "Ddc")
# plotHist(sce, comparison_file, by_exprs_values = "logcounts", gene = "Ddc")
# plotHist(sce, comparison_file, by_exprs_values = "logcounts", gene = "mei-P26")
# plotHist(sce, comparison_file, by_exprs_values = "logcounts", gene = "Dop2R")

