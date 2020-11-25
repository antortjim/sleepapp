#' Label groups of cells given a comparison
#' 
#' Given a pairwise-comparison specifying cells,
#' an experiment_ column is added that specifies
#' Minus/Plus/""
#' 
#' @param sce A SingleCellExperiment object
#' @param comparison_file Path to comparison file
#' @param remove If True, cells not included in the comparison
#' are removed from the SingleCellExperiment obhect
#' @return Labeled (and optionally reduced) SingleCellExperiment object
label_cells <- function(sce, comparison_file="", remove=TRUE) {
  stopifnot(nrow(comparison) != 0)
  
  cmd <- paste0('grep -v "#" "', comparison_file, '"')
  comparison <- data.table::fread(cmd)
  
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
    if(!any(index)) stop(glue::glue(
      "The {i}th group of file {comparison_file} has 0 matches. Is there a typo?"
    ))
    colData(sce)[index, "experiment_"] <- names(cells_passed)[i]  
  }
  
  if(isTRUE(remove)) sce <- sce[, cell_indices]
  
  return(sce)
}