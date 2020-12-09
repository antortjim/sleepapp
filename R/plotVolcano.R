#' @import glue
#' @import ggplot2
#' @import ggrepel
plotVolcano <- function(diff_table, genes=NULL, labels=0, colors=c("black", "yellow"), ...) {
  
   ellipsis <- list(...)
   load_list(ellipsis)
   selected_labels <- if (labels != 0) seq(from = 1, to = labels, by=1) else NULL
   if (getOption("shiny.debug")) browser()
   
   highlight_index <- unique(
     c(
       # top n labels
       which(1:nrow(diff_table) %in% selected_labels),
       # passed genes
       which(diff_table$Gene %in% genes),
       # signif genes
       which(diff_table$signif)
       )
     )
   nonhl_index <- setdiff(1:nrow(diff_table), highlight_index)
   
   
   diff_table_highlight <- diff_table[-nonhl_index, ]
   diff_table <- diff_table[nonhl_index, ]
   
   # Maximum 20 genes highlighted
   if (nrow(diff_table_highlight) > 20) {
     
     diff_table <- rbind(
         diff_table,
         diff_table_highlight[21:nrow(diff_table_highlight)]
     )
     diff_table_highlight <- diff_table_highlight[1:20, ]
     
   }

    gg <- ggplot() +
      geom_point(data = diff_table, aes(x = effect.size, y = -log10(significance))) +
      ggtitle(label = glue::glue("{cell_type} - {comparison} - {method}"))
    
    if (nrow(diff_table_highlight) != 0) {
      gg <- gg + geom_point(
              data = diff_table_highlight,
              col = colors[2],
              mapping = aes(x = effect.size, y = -log10(significance))
              # color = "black", shape = 21, stroke=0.5
            )
      gg <- gg + ggrepel::geom_label_repel(
             data = diff_table_highlight,
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
    