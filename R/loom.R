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
    return(sce)
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
