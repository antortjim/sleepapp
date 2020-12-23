sanitize_path <- function(path) {
  path <- gsub(pattern = "assay-_", x = path, replacement = "")
  path <- gsub(pattern = "grouping-_", x = path, replacement = "")
  path <- gsub(pattern = "comparison-_", x = path, replacement = "")
  path <- gsub(pattern = "__", x = path, replacement = "_")
  return(path)
}


read_single_table <- function(cell_type, method, comparison, grouping="", assay="") {
  sheet_path <- build_diff_table_path(cell_type, method, comparison, grouping=grouping, assay=assay)
  if (!file.exists(sheet_path)) {return(NULL)}
  df <- read_methods[[method]](sheet_path)#[, c("Gene", "effect.size", "significance", "corrected.significance")]
  df <- dplyr::arrange(df, significance)
  return(df)
}

#' @import openxlsx
export_sheet <- function(output_folder) {
  
  header_style <- openxlsx::createStyle(fgFill = "#4F81BD", halign = "CENTER", textDecoration = "Bold",
                     border = "Bottom", fontColour = "white")
  
  
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
    
  conf <- SleepAppConfiguration$new()
  config <- conf$content
  
  cell_types <- config$cell_types
  assays_available <- config$assays_available
  comparisons_available <- conf$comparisons_available()
  groupings_available <- config$groupings_available
  methods_available <- config$methods_available
  
  for (ct in cell_types) {
      for (ca in comparisons_available) {
        wb <- openxlsx::createWorkbook()
        for (ma in methods_available) {
          openxlsx::addWorksheet(wb, ma)
          start_col <- 3
          
          for (aa in assays_available) {
            for (ga in groupings_available) {
              df <- read_single_table(ct, ma, ca, ga, aa)
              # print(nrow(df))
              if (!is.null(df)) {
                openxlsx::writeData(
                  wb = wb, sheet = ma, x = data.table::data.table(Grouping = ga, Assay = aa),
                  startCol = start_col, startRow = 2,
                )
                openxlsx::writeData(
                  wb = wb, sheet = ma, x = data.table::data.table(Cell_type = ct, Comparison = ca, Method = ma),
                  startCol = start_col, startRow = 5,
                )
                openxlsx::writeData(
                  wb = wb, sheet = ma,
                  x = df, headerStyle = header_style,
                  startCol = start_col, startRow = 8
                )
              }
              start_col <- start_col + 6
              # print(start_col)
            }       
          }
        }
        output_workbook <- file.path(output_folder, paste0(ct, "_", ca, ".xlsx"))
        message(output_workbook)
        openxlsx::saveWorkbook(wb, file = output_workbook, overwrite = TRUE)
      }
  }
  return(0)
}

  
#' @importFrom glue glue
#' @import magrittr
#' @export
build_diff_table_path <- function(cell_type, method, comparison, grouping="", assay="") {
  
  if (method == "scvi") {
    ending <- "change_diff_table.csv"
    insert_grouping <- "grouping-Condition_"
  } else {
      ending <- "diff_table.csv"
      insert_grouping <- glue::glue("grouping-{grouping}_")
      
  }
  abs_dir <- SleepAppConfiguration$new()$content$abs_dir
  
  
  path <- file.path(
    abs_dir, method, cell_type,
    glue::glue("{cell_type}_assay-{assay}_comparison-{comparison}_{insert_grouping}{ending}")
  )
  
  return(path)
}

#' @importFrom glue glue
build_sce_rds_path <- function(cell_type, grouping, normalized=TRUE) {
  
  abs_dir <- SleepAppConfiguration$new()$content$abs_dir
  
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
    abs_dir,
    folder,
    cell_type,
    filename
  )
  return(path)
}

build_comparison_path <- function(comparison_file) {
  
  root_dir <- SleepAppConfiguration$new()$content$root_dir
  file.path(root_dir, "comparisons", paste0(comparison_file, ".csv"))
}

# Hacky way of loading the stuff in a list to the environment
#' @importFrom glue glue
load_list <- function(foo) {
  for (i in 1:length(foo)) {
    assign(value = foo[[i]], x = names(foo)[i], envir = parent.frame(1))
  }
}


get_diff_tables <- function() {
  abs_dir <- SleepAppConfiguration$new()$content$abs_dir
  list.files(path = abs_dir, recursive = TRUE, pattern = "diff_table.csv")
}


#' Annotate mitochondrial genes
#'
#' Set the mitochondrial field of the gene metadata to true
#' if `pattern` is found in the gene name
#' @param pattern Character which if contained in a gene name signals it is a mitochondrial gene
#' @details The mitochondria has its own genome and mitochondrial genes are encoded in this genome
#' i.e. genes encoded in the nucleus but whose protein product travels to the mitochondria are NOT mitochondrial genes
#' @import SingleCellExperiment
#' @export
annotate_mitochondrial_genes <- function(sce, pattern="mt:") {
  rowData(sce)$mitochondrial <- FALSE
  rowData(sce)[grep(x = rownames(rowData(sce)), pattern = pattern,), "mitochondrial"] <- TRUE
  return(sce)
}


