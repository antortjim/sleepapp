#' Functions to parse metadata from the path of a diff_table
#' 
#' Given a path, parse the method it belongs to
#'  @import magrittr
parseMethod <- function(path) {
  
  names(read_methods) %>% sapply(function(p) grep(pattern = p, x = path, value = TRUE)) %>% unlist %>% names
}

#' Given a path, parse the comparison it belongs to
#' @importFrom stringr str_match
parseComparison <- function(path) {
  res <- stringr::str_match(string = path, pattern = "comparison-(.*vs.*)_(grouping-Condition)?_diff_table.csv")[,2]
  if (is.na(res)) res <- stringr::str_match(string = path, pattern = "comparison-(.*vs.*)_diff_table.csv")[,2]
  return(res)
}

#' Given a path, parse the celltype/cluster it belongs to
#' @importFrom stringr str_match
parseCellType <- function(path) {
  filename <- basename(path)
  res <- stringr::str_match(string = filename, pattern = "(.*)_(assay-.*)_comparison-.*_diff_table.csv")[,2]
  if (is.na(res)) res <- stringr::str_match(string = filename, pattern = "(.*)_comparison-.*_diff_table.csv")[,2]
  return(res)
}
