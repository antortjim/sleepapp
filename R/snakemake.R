Snakemake <- methods::setClass(
  "Snakemake",
  slots = c(
    input = "list",
    output = "list",
    params = "list",
    wildcards = "list",
    threads = "numeric",
    log = "list",
    resources = "list",
    config = "list",
    rule = "character",
    bench_iteration = "numeric",
    scriptdir = "character",
    source = "function"
  )
)


unlockEnvironment <- function (env) {
  return (new.env(parent=env))
}

#' Load a json file and generate a fake Snakemake object
#' for easy debugging of Snakemake pipelines
#' 
#' The snakemake object is bound to the global environment
#' using rlang functionality
#' @importFrom jsonlite fromJSON
#' @import methods
#' @import rlang
#' @export
snakemaker <- function(jsonfile, overwrite=FALSE) {
  
  e <- rlang::global_env()
  if ((!"snakemake" %in% rlang::env_names(e)) | overwrite) {
  
    message("Loading json file")
    json_data <- jsonlite::fromJSON(jsonfile)
    
    snakemake <- Snakemake(
      input = json_data$input,
      output = json_data$output,
      params = json_data$params,
      wildcards = json_data$wildcards
    )
    
    rlang::env_poke(e, "snakemake", snakemake)
  }
}
