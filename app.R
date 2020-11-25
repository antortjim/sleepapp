library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(SingleCellExperiment)
library(scuttle)

sce <- readRDS("../results/20201118/scran/Condition/y_KCs/y_KCs_norm.rds")

normcounts(sce)
list.files(".") %>% grep(pattern="app.R", invert=TRUE, value=TRUE) %>% lapply(source)

comparison <- "ZT8 Gab vs. ZT8 20h SD"


plotDEGs(cell_type = "y_KCs", DE_method = "*", comparison = comparison, grouping = "Condition") %>% View

res <- plotDEGs(cell_type = "y_KCs", DE_method = "*", comparison = comparison, grouping = "Condition")

signif_genes <- res[res$total_DE == 2, "Gene"] %>% unlist
signif_genes <- strsplit(
  "Ddc Alg-2 mei-P26 eIF-4a nAChRalpha6 Dop2R Pka-C1 Pka-R2 Clip-190 RpS18",
  split = " "
) %>% unlist


signif_genes %>% 
  lapply(function(gene) plotHist(sce = sce, comparison_file = comparison, by_exprs_values = "normcounts", gene=gene))

subsce <- sce[rowData(sce)$Gene %in% signif_genes, !(sce$Condition %in% c( "ZT 8 gab", "ZT 8 20h SD"))]
subsce <- sce[rowData(sce)$Gene %in% signif_genes, (sce$Sleep_Stage %in% c("ZT 14"))]
subsce <- sce[rowData(sce)$Gene %in% signif_genes, (sce$Sleep_Stage %in% c("ZT 20"))]


subsce <- scater::runPCA(subsce, exprs_values="logcounts")
scater::plotPCA(subsce, colour_by="Sleep_Stage")
scater::plotPCA(subsce, colour_by="Condition", ncomponents=5)
scater::plotPCA(subsce, colour_by = "Treatment",
                ncomponents=5)

subsce <- scater::runUMAP(subsce, exprs_values="logcounts")
scater::plotUMAP(subsce, colour_by="Treatment", ncomponents = 2)
subsce <- scater::runTSNE(subsce,  exprs_values="logcounts")
scater::plotTSNE(subsce, colour_by="Percent_mito")
scater::plotTSNE(subsce, colour_by="Genotype")
scater::plotTSNE(subsce, colour_by="Treatment")

