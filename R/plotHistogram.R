#' Plot the histogram of counts of a gene across cells in a pairwise comparison
#' 
#' @details
#' * The SingleCellObject must have a colData column called experiment_
#' encoding which group the corresponding cell belongs to
#' * The colData should contain a CellID column unique to each cell
#' * The SingleCellExperiment must have the assay specified by by_exprs_values i.e. at least counts
#' @param sce SingleCellExperiment
#' @param gene A gene contained in the Gene variable in the rowData of sce
#' @param comparison_file Path to a comparison file
#' @param by_exprs_values Name of a counts assay in the sce
#' @importFrom glue glue
#' @importFrom dplyr left_join
#' @import SingleCellExperiment
#' @import scuttle
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @seealso `readComparisonFile`
#' @export
plotHistogram <- function(sce, gene, comparison_file, by_exprs_values="counts", geom="histogram") {
  
  if (getOption("shiny.debug")) browser()
  
  subsce <- label_cells(sce, comparison_file = comparison_file, remove = TRUE)
  stopifnot(all(gene %in% rowData(subsce)$Gene))
            
  counts_matrix <- assay(subsce[rowData(subsce)$Gene %in% gene,], by_exprs_values)
  
  counts_df <- as.data.frame(t(counts_matrix))
  colnames(counts_df) <- rowData(subsce)[rowData(subsce)$Gene %in% gene, "Gene"]
  counts_df$CellID <- rownames(counts_df)
  rownames(counts_df) <- NULL
  
  counts_df <- tidyr::pivot_longer(counts_df, names_to="Gene", cols = gene, values_to="counts")
  

  cell_metadata <- as.data.frame(colData(subsce))
  cell_metadata <- cell_metadata[, c("CellID", "experiment_")]
  
  counts_df <- dplyr::left_join(counts_df, cell_metadata, by = "CellID")
  counts_df <- counts_df[complete.cases(counts_df),]
  
  gg <- ggplot(data = counts_df, aes(x = counts,
                              col = experiment_,
                              fill = experiment_
                              )) + facet_wrap("Gene")
  
    
  gg <- gg + geoms[[geom]](alpha = 0.2)
  
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

geoms <- list(
  "histogram" = function(...) {
    geom_histogram(..., position = "dodge")
  },
  "density" = function(...) {
    geom_density(...)
  }
)
