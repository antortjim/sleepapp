#' Load a Scope loomfile into R
#'
#' Open a Scope loomfile and load its data
#' into R as a SingleCellExperiment object
#' @param path Path to a Scope loomfile
#' @import rhdf5
#' @import HDF5Array
#' @import SingleCellExperiment
#' @details A scope loomfile is an HDF5 file with the following structure:
#' \itemize{
#' \item \textbf{matrix}: counts matrix
#' \item \textbf{col_attrs}: cell metatadata. cell identifiers (barcodes) are available under CellID field
#' \item \textbf{row_attrs}: gene metatadata. gene identifiers are available under Gene field
#' \item Extra layers may be available
#' }
#' @export
loom2sce <- function(path) {
    
    message(paste0("Loading loomfile -> ", path))
    sce <- tryCatch({
        # Extracting the count matrix.
        mat <- HDF5Array::HDF5Array(path, "matrix")
        mat <- t(mat)

        # Extracting the row and column metadata.
        col.attrs <- rhdf5::h5read(path, "col_attrs")
        if (length(col.attrs)) { 
            col.df <- data.frame(col.attrs)
            row.names(col.df) <- col.attrs$CellID
        } else {
            col.df <- DataFrame(matrix(0, nrow(mat), 0))
        }

        row.attrs <- rhdf5::h5read(path, "row_attrs")
        if (length(row.attrs)) { 
            row.df <- data.frame(row.attrs)
            row.names(row.df) <- row.attrs$Gene
        } else {
            row.df <- NULL
        }

        # Extracting layers (if there are any)
        optional <- rhdf5::h5ls(path)
        is.layer <- optional$group=="/layer"
        if (any(is.layer)) {
            layer.names <- optional$name[is.layer,]
            other.layers <- vector("list", length(layer.names))
            names(other.layers) <- layer.names

            for (layer in layer.names) {
                current <- HDF5Array::HDF5Array(path, file.path("/layer", layer))
                other.layers[[layer]] <- t(current)
            }
        } else {
            other.layers <- list()
        }

        # Returning SingleCellExperiment object.
        sce <- SingleCellExperiment::SingleCellExperiment(c(counts=mat, other.layers), rowData=row.df, colData=col.df)
    }, error = function(e) {
        Sys.sleep(60)
        message("Loomfile unavailable. Trying again in 50 seconds")
        loom2sce(path)
    })

    return(sce)
}

#' Export a SingleCellExperiment as a loomfile
#'
#' @param sce SingleCellExperiment instance
#' @param output_loomfile Path to newly created loomfile. It must not exist
#'# @param clean If TRUE, all columns containing ClusterMarkers in gene metadata are dropped
#' @import SingleCellExperiment
#' @import loomR
#' @importFrom dplyr select
#' @export
sce2loom <- function(sce, output_loomfile) {
    features <- colnames(rowData(sce))
    features <- features[features != "Gene"]

    if (length(features) == 0) {
        features_metadata <- NULL
    } else {
        features_metadata <- as.list(rowData(sce)[, features, drop=FALSE])
        names(features_metadata) <- features
    }
    

    # Cell metadata
    cell_metadata <- as.list(as.data.frame(colData(sce)) %>% dplyr::select(-CellID))

    if (length(cell_metadata) == 0) cell_metadata <- NULL
    names(features_metadata)

    # Open loom connection and fill it with data
    loomcon <- loomR::create(
        filename = output_loomfile,
        data = counts(sce),
        cell.attrs = cell_metadata,
        feature.attrs = features_metadata,
        transpose = TRUE # because counts(sce) returns features as rows
    )
    # Close the connection!!!!
    loomcon$close_all()
}

#' Keep cells whose barcode is included in the barcodes_file
#'
#' The cell barcode must be available
#' in the CellID column of the colData dataframe in the sce object
#' @param sce A SingleCellExperiment object
#' @param barcodes_file A txt file leading to cell barcodes, one per line
#' @export
filter_sce <- function(sce, barcodes_file, verbose=0) {
    barcodes <- read.table(barcodes_file)[, 1]
    keep_cells <- (colData(sce)[,"CellID"] %in% barcodes)
    if (verbose) table(keep_cells)
    stopifnot(nrow(colData(sce)) != 0)
    return(sce[,keep_cells])
}

#' @importFrom data.table fwrite
#' @import SingleCellExperiment
#' @export
sce2csv <- function(sce, output, assay) {
    if (is.null(names(assays(sce))))
        sce_counts <- assays(sce)
    else
        sce_counts <- assays(sce)[[assay]]

    data.table::fwrite(x = as.data.frame(as.matrix(sce_counts)), file = output[1], row.names = F, col.names = F)
    data.table::fwrite(x = as.data.frame(colData(sce)), file = output[2])
    data.table::fwrite(x = as.data.frame(rowData(sce)), file = output[3])
}
