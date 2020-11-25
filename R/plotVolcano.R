ROOT_DIR <- "/1TB/Cloud/Lab/Projects/SleepSignature/workflow/"
RESULTS_DIR <- "results/20201118"

sanitize_path <- function(path) {
  path <- gsub(pattern = "assay-_", x = path, replacement = "")
  path <- gsub(pattern = "grouping-_", x = path, replacement = "")
  path <- gsub(pattern = "comparison-_", x = path, replacement = "")
  path <- gsub(pattern = "__", x = path, replacement = "_")
  return(path)
}



#' @import glue
plotVolcano <- function(cell_type, method, comparison, grouping="", assay="", labels=0, colors=c("black", "yellow")) {
    path <- file.path(glue::glue(
      "{ROOT_DIR}/{RESULTS_DIR}/{method}/{grouping}/{cell_type}/{cell_type}_assay-{assay}_comparison-{comparison}_{grouping}_diff_table.csv"
    )) %>% sanitize_path
    
    diff_table <- readMethods[[method]](path)
    

    gg <- ggplot() +
      geom_point(data = diff_table, aes(x = effect.size, y = -log10(significance), col = signif)) +
      ggtitle(label = glue::glue("{cell_type} - {comparison} - {method}"))
    
    if (labels != 0) {
      diff_table_labels <- diff_table[1:labels, ]
      gg <- gg + ggrepel::geom_label_repel(
        data = diff_table_labels,
        mapping = aes(x = effect.size, y = -log10(significance), label = Gene)
      )
    }
    
    if (method != "MAST") {
      gg <- gg + labs(x = "log2FC", y = "-log10(PVal)")
    } else {
      gg <- gg + labs(x = "MAST coefficient", y = "-log10(P-Chisq)")
      
    }
    
    gg <- gg + scale_color_manual(values = colors)

    return(gg)
}

# plotVolcano("y_KCs", "Wilcoxon", "ZT_20_sleep-vs-ZT_20_SD", labels = 3)
  