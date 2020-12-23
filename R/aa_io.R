#' Load diff_table produced by MAST
#' 
#' Publication: https://genomebiology.biomedcentral.com/articles/10.1186/s13059-015-0844-5
#' Notebook: https://github.com/antortjim/SleepSignature/blob/main/notebooks/08-MAST.R.ipynb
#' 
#' @importFrom data.table fread
#' @importFrom dplyr arrange
#' @export
readMAST <- function(path, signif_threshold=0.05, only_signif=FALSE) {
  
  diff_table <- data.table::fread(file = path)
  diff_table <- diff_table[, c("Gene", "Coefficient", "Probability (Chisq)", "FDR (Adj-pval)")]
  colnames(diff_table) <- c("Gene", "effect.size", "significance", "corrected.significance")
  diff_table$signif <- diff_table$corrected.significance < signif_threshold
  diff_table <- dplyr::arrange(diff_table, corrected.significance)
  
  if (only_signif) {
    diff_table <- diff_table[diff_table$signif,]
  }
  
  return(diff_table)
}


#' @importFrom data.table fread
#' @importFrom dplyr arrange
#' @export
readWilcoxon <- function(path, signif_threshold=0.05, only_signif=FALSE) {
  
  diff_table <- data.table::fread(file = path)
  diff_table <- diff_table[, c("genes", "logFC", "PValue", "PValue_adj")]
  colnames(diff_table) <- c("Gene", "effect.size", "significance", "corrected.significance")
  diff_table$signif <- diff_table$corrected.significance < signif_threshold
  diff_table <- dplyr::arrange(diff_table, corrected.significance)
  
  if (only_signif) {
    diff_table <- diff_table[diff_table$signif,]
  }
  
  return(diff_table) 
}

#' Load diff_table produced by MAST
#'
#' edgeR does not return adjusted PValues (for now)
#' but an integer called direction which is non 0 if the adj-PValue is significant
#' and 0 if it is not significant
#' If it is significant, it will be eithe 1 or -1 depending on the sign of the fold change
#' 
#' @importFrom data.table fread
#' @importFrom dplyr arrange
#' @export
readEdgeR <- function(path, signif_threshold=NULL, only_signif=FALSE, adjust=TRUE) {
  
  diff_table <- data.table::fread(file = path)
  diff_table <- diff_table[, c("gene", "logFC", "PValue", "PValue", "direction")]
  colnames(diff_table) <- c("Gene", "effect.size", "significance", "corrected.significance", "direction")
  
  if (adjust) {
    diff_table$corrected.significance <- p.adjust(p = diff_table$significance, method = "fdr")
    diff_table$signif <- diff_table$corrected.significance <= 0.05
  } else {
    diff_table$signif <- diff_table$direction != 0
  }
  diff_table$direction <- NULL
  diff_table <- dplyr::arrange(diff_table, corrected.significance)
  
  if (only_signif) {
    diff_table <- diff_table[diff_table$signif,]
  }
  
  return(diff_table) 
}

#' @importFrom data.table fread
#' @importFrom dplyr arrange
#' @export
readDESeq <- function(path, signif_threshold=0.05, only_signif=FALSE) {

  diff_table <- data.table::fread(file = path)
  diff_table <- diff_table[, c("gene", "log2FoldChange", "pvalue", "padj")]
  colnames(diff_table) <- c("Gene", "effect.size", "significance", "corrected.significance")
  diff_table$signif <- diff_table$corrected.significance < signif_threshold
  diff_table <- dplyr::arrange(diff_table, corrected.significance)
  
  if (only_signif) {
    diff_table <- diff_table[diff_table$signif,]
  }
  
  return(diff_table) 
}


readSCVI_vanilla <- function(path, signif_threshold=0.05, only_signif=FALSE) {
  diff_table <- data.table::fread(file=path)
  diff_table <- diff_table[, c("Gene", "log_scale_ratio", "abs_bayes_factor", "abs_bayes_factor")]
  colnames(diff_table) <- c("Gene", "effect.size", "significance", "corrected.significance")
  diff_table$signif <- diff_table$corrected.significance < signif_threshold
  diff_table <- dplyr::arrange(diff_table, corrected.significance)
  
  if (only_signif) {
    diff_table <- diff_table[diff_table$signif,]
  }
  
  return(diff_table) 
  
}

#' @importFrom data.table fread
#' @importFrom dplyr arrange
#' @export
readSCVI_change <- function(path, signif_threshold=0.05, only_signif=FALSE) {
  diff_table <- data.table::fread(file=path)
  diff_table <- diff_table[, c("Gene", "lfc_median", "proba_not_de", "proba_not_de")]
  colnames(diff_table) <- c("Gene", "effect.size", "significance", "corrected.significance")
  diff_table$signif <- diff_table$corrected.significance < signif_threshold
  diff_table <- dplyr::arrange(diff_table, corrected.significance)
  
  if (only_signif) {
    diff_table <- diff_table[diff_table$signif,]
  } 
  
  return(diff_table) 
}

read_methods <- list(
  MAST = readMAST,
  edgeR = readEdgeR,
  DESeq = readDESeq,
  Wilcoxon = readWilcoxon,
  scvi = readSCVI_change
  
)