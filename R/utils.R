sanitize_path <- function(path) {
  path <- gsub(pattern = "assay-_", x = path, replacement = "")
  path <- gsub(pattern = "grouping-_", x = path, replacement = "")
  path <- gsub(pattern = "comparison-_", x = path, replacement = "")
  path <- gsub(pattern = "__", x = path, replacement = "_")
  return(path)
}

#' @importFrom glue glue
#' @import magrittr
build_diff_table_path <- function(cell_type, method, comparison, grouping="", assay="") {
  
  if (method == "scvi") {
    ending <- "change_diff_table.csv"
    insert_grouping <- "grouping-Condition_"
  } else {
      ending <- "diff_table.csv"
      insert_grouping <- glue::glue("grouping-{grouping}_")
      
  }
  path <- file.path(
    ROOT_DIR, WORKING_DIR, method, cell_type,
    glue::glue("{cell_type}_assay-{assay}_comparison-{comparison}_{insert_grouping}{ending}")
  )
  
  return(path)
}

build_sce_rds_path <- function(cell_type, grouping, normalized=TRUE) {
  if (normalized) {
    folder <- "scran"
    filename <- glue::glue(
      "{cell_type}_grouping-{grouping}_norm.rds"
    )
    
  } else {
    folder <- "preproc_QC"
    filename <- glue::glue(
      "{cell_type}_raw.rds"
    )
  }
  
  path <- file.path(
    ROOT_DIR, WORKING_DIR,
    folder,
    cell_type,
    filename
  )
  return(path)
}

build_comparison_path <- function(comparison_file) {
  file.path(ROOT_DIR, "comparisons", paste0(comparison_file, ".csv"))
}

# Hacky way of loading the stuff in a list to the environment
#' @importFrom glue glue
load_list <- function(foo) {
  for (i in 1:length(foo)) {
    assign(value = foo[[i]], x = names(foo)[i], envir = parent.frame(1))
  }
}
