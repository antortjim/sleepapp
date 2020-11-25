#' Given a path, parse the method it belongs to
#'  @import magrittr
getMethod <- function(path) {
  c("MAST", "DESeq", "edgeR", "Wilcoxon") %>% sapply(function(p) grep(pattern = p, x = path, value = TRUE)) %>% unlist %>% names
}

#' Given a path, parse the comparison it belongs to
#' @importFrom stringr str_match
getComparison <- function(path) {
  res <- stringr::str_match(string = path, pattern = "comparison-(.*vs.*)_(Condition)?_diff_table.csv")[,2]
  if (is.na(res)) res <- stringr::str_match(string = path, pattern = "comparison-(.*vs.*)_diff_table.csv")[,2]
  return(res)
}

getCellType <- function(path) {
  filename <- basename(path)
  res <- stringr::str_match(string = filename, pattern = "(.*)_(assay-.*)_comparison-.*_diff_table.csv")[,2]
  if (is.na(res)) res <- stringr::str_match(string = filename, pattern = "(.*)_comparison-.*_diff_table.csv")[,2]
  return(res)
}