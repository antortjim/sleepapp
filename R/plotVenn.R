#' Get DEGs found in a list of diff_tables
#' 
#' Get Differentially Expressed Genes (DEGs) across diff_table(s)
#' Optionally, customise the effect size and/or significance threshold
#' Optionally keep genes   
#' @importFrom magrittr `%>%`
getDEGs <- function(data_list, sign=0, effect.size_threshold = 0.05, signif_threshold=0.05) {
  
  # read diff table of each method
  genes <- lapply(data_list, function(dl) {
    method <- parseMethod(dl)
    # keep only those with significance threshold -> only_signif = TRUE
    diff_table <- read_methods[[method]](dl, signif_threshold=signif_threshold, only_signif=TRUE)[, c("Gene", "effect.size")]
    
    # keep only those with an effect.size threshold
    diff_table <- diff_table[abs(diff_table$effect.size) > effect.size_threshold,]

    # if sign is not 0, keep DEGs only with that sign
    if (sign != 0) {
      diff_table <- diff_table[diff_table$effect.size * sign > 0,]
    }
    # return the Gene column
    diff_table$Gene
  }) %>%
    unlist %>%
    # count each gene maximum once
    unique
  return(genes)

}



computePartitionCounts <- function(data_list_list, ...) {
  ncircles <- length(data_list_list)
  if (ncircles > 3)
    stop("Only 2 and 3 fold Venn Diagrams are supported")
  
  groups <- names(data_list_list)
  if (is.null(groups))
    stop("Please name entered list i.e. give a name to each element")
  
  gene_list <- lapply(data_list_list, function(data_list) getDEGs(data_list, ...))
  genes <- gene_list %>% unlist %>% unique
  
  gene_presence_matrix <- lapply(gene_list, function(gl) {
    genes %in% gl
  }) %>% do.call(rbind, .) %>% t
  
  rownames(gene_presence_matrix) <- genes
  colnames(gene_presence_matrix) <- groups
  return(gene_presence_matrix)
}

computeGeneEnsembles <- function(gene_presence_matrix, groups) {
  arr_inds <- which(gene_presence_matrix, arr.ind = TRUE)

  partition <- rownames(gene_presence_matrix) %>% sapply(
    function(g) {
      x <- arr_inds[g == rownames(arr_inds), 2]
      paste0(groups[x], collapse="_")
    }
  )
  partition <- unique(partition) %>% sapply(
    function(p) paste0(names(which(partition == p)), collapse = ',')
  )
  return(partition)
}

#' Generate Venn Diagram based on up to 3 lists of genes
#' @importFrom limma vennDiagram vennCounts
#' @importFrom magrittr `%>%`
plotVenn <- function(data_list_list, ...) {
  colors <- c("red", "blue", "green")
  gene_presence_matrix <- computePartitionCounts(data_list_list, ...)
  counts <- limma::vennCounts(gene_presence_matrix)
  limma::vennDiagram(counts, circle.col = colors[1:length(data_list_list)])
}
