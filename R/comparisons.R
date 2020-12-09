#' Read a comparison file into R
#' 
#' @param path Path to a comparison file
#' 
#' @details A comparison file is a csv file with a header whose column names
#' match column names in SingleCellExperiment objects
#' Each row specifies the set of cells matching all column values
#' If all possible values of a metavariable should be included in the group
#' i.e. the metavariable is to be ignored when building the group, supply * to match everything
#' A comparison file should also contain a column named experiment_ with values either Minus or Plus
#' Only pairwise comparisons are supported for now
#' @importFrom data.table fread
#' @importFrom glue glue
readComparisonFile <- function(path) {
  # the path is wrapped with double quotes so it works
  # even if there are spaces in it
  if (!file.exists(path)) stop(glue::glue("Provided comparison_file {path} does not exist")) 
  cmd <- paste0('grep -v "#" "', path, '"')
  comparison <- data.table::fread(cmd = cmd)
  if (nrow(comparison) == 0) stop(glue::glue("Cant read comparison_file {path}"))
  return(comparison)
}


#' Return index of cells in SingleCellExperiment that match group
#' 
#' @param sce SingleCellExperiment
#' @param group named list matching column names in the sce colData
#' @return Logical vector where ith value is TRUE if the ith cell belongs to group
#' @seealso `label_cells`
#' @import SingleCellExperiment
match_group <- function(sce, group) {
  
  checked_columns <- setdiff(colnames(group), "experiment_")
  individual_columns <- lapply(checked_columns, function(col) {
    if (group[[col]] != '*')
      colData(sce)[[col]] == group[[col]]
    else
      rep(TRUE, length = nrow(colData(sce)))
  })
  
  cells_passed <- Reduce(`&`, individual_columns)
  
  return(cells_passed)
}


#' Label groups of cells given a comparison
#' 
#' Given a pairwise-comparison specifying cells,
#' an experiment_ column is added that specifies one of
#' c("Minus", "Plus", "")
#' 
#' @param sce A SingleCellExperiment object
#' @param comparison_file Path to comparison file
#' @param remove If True, cells not included in the comparison
#' are removed from the SingleCellExperiment obhect
#' @return Labeled (and optionally reduced) SingleCellExperiment object
#' @import SingleCellExperiment
#' @export
label_cells <- function(sce, comparison_file, remove=TRUE) {

  comparison <- readComparisonFile(comparison_file)
  
  cells_passed <- lapply(1:nrow(comparison), function(i) {
    match_group(sce, comparison[i, ])  
  })
  names(cells_passed) <- comparison$experiment_
  cell_indices <- Reduce(`|`, cells_passed)
  
  pass_matrix <- do.call(rbind, cells_passed)
  stopifnot(all(colSums(pass_matrix) < 2))
  colData(sce)$experiment_ <- NA_character_
  
  for (i in 1:length(cells_passed)) {
    index <- cells_passed[[i]]
    if (!any(index)) stop(glue::glue(
      "The {i}th group of file {comparison_file} has 0 matches. Is there a typo?"
    ))
    colData(sce)[index, "experiment_"] <- names(cells_passed)[i]  
  }
  
  if (isTRUE(remove)) sce <- sce[, cell_indices]
  
  return(sce)
}


#' Filter sce so only genes are available
#' Sort the sce so genes are alphabetically ordered
#' @import SingleCellExperiment
#' @export
keep_genes <- function(sce, genes) {
  sce <- sce[rownames(rowData(sce)) %in% genes,]
  genes_sorted <- sort(genes)
  genes_sce <- rownames(rowData(sce))
  sce[match(shared_genes, genes_sce),]
}

#' A wrapper around scuttle::aggregateAcrossCells that also:
#' * computes normcounts (divide by library size) and logcounts (normcounts and take log with pseudocount 1)
#' * adds QC metrics
#' * marks mito genes
#' * formats sample names
#' @import scuttle
#' @import SingleCellExperiment
#' @export
aggregate_cells <- function(sce, assay=c("normcounts", "counts"), pca_assay=c("normcounts", "logcounts")) {
  
  summed <- scuttle::aggregateAcrossCells(
    sce, 
    id=colData(sce)[,bulk_column_criteria],    
    use.assay.type = assay
  )
  
  # rename whatever assay used as the counts assay of the bulk ex-single-cell object
  assay(summed, "counts") <- assay(summed, assay)
  
  # compute the library size factor
  summed$sizeFactor <- scuttle::librarySizeFactors(summed)
  
  # Normalize
  summed <- scuttle::logNormCounts(summed, log = FALSE, pseudo.count=0,  center.size.factors=TRUE)
  
  # Normalize and take log
  summed <- scuttle::logNormCounts(summed, log = TRUE, pseudo.count=1, center.size.factors=TRUE)
  
  # Add metrics and be mitoch gene aware
  is.mito <- grep(pattern = "mt:", x = rownames(rowData(summed)))
  summed <- scuttle::addPerCellQC(summed, assay.type=assay, subsets=list(Mito=is.mito))
  
  # give a nice sample name
  summed$SampleName <- colData(summed)[,bulk_column_criteria]  %>%
    apply(., 1, function(x) {paste(x, collapse = "_")})
  
  # drop single cell (useless) columns
  keep_cols <- colData(summed) %>%
    apply(., 2, function(x) !all(is.na(x)))                                  
  colData(summed) <- colData(summed)[,keep_cols]
  
  return(summed)
}

