

DE_method <- "*"
cell_type <- "*"
comparison <- "*"
grouping = "Condition"
Sys.glob("../results/20201118/Wilcoxon/*/*_diff_table.csv")
signif_status <- c("TRUE" = 2, "FALSE" = 1, "NA" = 0)
column_var = "method"

plotDEGs <- function(cell_type="*", DE_method = "*", comparison="*", grouping="Condition", names_var="method", values_var="signif_status") {
  diff_table_paths <- Sys.glob(
    file.path(
      ROOT_DIR,
      RESULTS_DIR,
      DE_method,
      grouping,
      cell_type,
      "*_diff_table.csv"
    )
  )
  
  diff_table <- lapply(diff_table_paths, function(path) {
    method <- getMethod(path)
    diff_table <- readMethods[[method]](path)
    diff_table$method <- method
    diff_table$comparison <- getComparison(path)
    diff_table$cell_type <- getCellType(path)
    return(diff_table)
  })
  
  diff_table_df <- do.call(rbind, diff_table)
  
  # diff_table_df[diff_table_df$Gene == "jeb" & diff_table_df$cell_type == "y_KCs" & diff_table_df$comparison == "ZT8 Gab vs. ZT8 20h SD", ]
  
  # diff_table_df$column_var <- diff_table_df[[column_var]]
  # diff_table_df[[column_var]] <- NULL
  # rm(column_var)
  # browser()
  # print(diff_table_df)
  diff_table_df <- diff_table_df[complete.cases(diff_table_df),]

  # diff_table_df[is.na(diff_table_df$signif), "signif_status"] <- 0
  diff_table_df[diff_table_df$signif, "signif_status"] <- 2
  diff_table_df[!diff_table_df$signif, "signif_status"] <- 1
  
  table(diff_table_df$signif_status)

  # rm(column_var)
  
  comparison_value <- comparison
  rm(comparison)
  # comparison_value <- "ZT8 Gab vs. ZT8 20h SD"
  cell_type_value <- cell_type
  rm(cell_type)
  # cell_type_value <- "y_KCs"
  
  diff_table_df_sub <- diff_table_df[diff_table_df$comparison == comparison_value, ]
  diff_table_df_sub <- diff_table_df_sub[diff_table_df_sub$cell_type == cell_type_value, ]
  # diff_table_df_sub[diff_table_df_sub$Gene == "jeb" & diff_table_df_sub$cell_type == "y_KCs" & diff_table_df_sub$comparison == "ZT8 Gab vs. ZT8 20h SD", ]
  
  keep_cols <- unique(c("Gene", values_var, "method", "cell_type", "comparison"))
  keep_cols <- names(which(sapply(keep_cols, function(x) x %in% colnames(diff_table_df_sub))))
  diff_table_df_sub <- diff_table_df_sub[, keep_cols, with = F ]
  # diff_table_df_sub[diff_table_df_sub$Gene == "jeb",]
  
  diff_table_df_wide <- diff_table_df_sub %>% tidyr::pivot_wider(data = ., names_from = names_var, values_from = values_var)
  # diff_table_df_wide[diff_table_df_wide$Gene == "jeb", ]

  diff_table_df_wide$total_DE <- rowSums((diff_table_df_wide[, names(readMethods)] == 2), na.rm = TRUE)
  diff_table_df_wide <- dplyr::arrange(diff_table_df_wide, -total_DE)
  diff_table_df_wide
  
  # getRectData <- function(i, gene) {
  #   data.frame(
  #     xmin = 0:3, xmax=1:4, gene=gene, ymin=i,ymax=i+1, gene=gene,
  #     method = names(readMethods),
  #     fill=unlist(diff_table_df_wide[diff_table_df_wide$Gene == gene, c(names(readMethods))]))
  # }
  # 
  # diff_table_df_wide[ diff_table_df_wide$total_DE == 2,]

  # res <- lapply(1:nrow(diff_table_df_wide), function(i) {getRectData(i, diff_table_df_wide[["Gene"]][i])})
  # res <- do.call(rbind, res)
  # rownames(res) <- NULL
  # res
  # ggplot() + geom_rect(data=res, aes(xmin=xmin, xmax=xmax, ymin=ymin,ymax=ymax,fill=as.factor(fill))) + ggplot2::
  
  
  
}

# df1 <- readMAST(path = "../results/20201118/MAST/Condition/y_KCs/y_KCs_assay-normcounts_comparison-ZT8 Gab vs. ZT8 20h SD_Condition_diff_table.csv")
# df2 <- readWilcoxon(path = "../results/20201118/Wilcoxon/Condition/y_KCs/y_KCs_comparison-ZT8 Gab vs. ZT8 20h SD_diff_table.csv")
# df1[df1$Gene == "jeb",]
# df2[df2$Gene == "jeb",]
