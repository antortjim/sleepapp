#' Configuration of SleepApp
#'
#' Load and update the configuration of the program
#'
#' Anywhere in the application `SleepAppConfiguration` can be instantiated so
#' default values for hardware defined parameters can be set dynamically
#' via a config file
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @import magrittr
#' @export
#' @noRd
SleepAppConfiguration <- R6::R6Class(classname = "SleepAppConfiguration", public = list(
  
  content = list(),
  config_file = "",


  initialize = function(config_file = "/etc/sleepapp.conf") {
    
    content <- list("debug" = TRUE, "ncores" = 2, "stop_backups" = TRUE, port = 3838)
    self$config_file <- config_file
    content$content <- content
    content$cell_types <- list(
      "γ KC" = "y_KCs",
      "αβ KC" = "a_b_KCs",
      "α'β' KC" = "a_b_prime_KCs",
      "Astrocytes" = "Astrocytes",
      "Ensheathing Glia" = "Ensheathing_Glia",
      "Surface Glia" = "Surface_Glia"
    )
    content$methods_available = names(read_methods)
    content$assays_available = c("counts", "normcounts", "logcounts")
    content$groupings_available = c("Condition", "quick_clusters")
    content$root_dir = "/1TB/Cloud/Lab/Projects/SleepSignature/workflow/"
    content$working_dir = "results/20201213/"
    content$abs_dir <- file.path(content$root_dir, content$working_dir)
    self$content <- content
    self$load()
  },
  comparisons_available = function() {
     file.path(self$content$root_dir, "comparisons") %>% list.files %>% gsub(x = ., pattern = "\\.csv", replacement = "")
  },
  save = function(config_file = NULL) {
    json <- jsonlite::toJSON(self$content)
    
    if (is.null(config_file))
      config_file <- self$config_file
    
    write(x = json, file = config_file)
  },
  
  load = function() {
    if (!file.exists(self$config_file)) self$save()
    else {
      json <- jsonlite::fromJSON(self$config_file)
      self$content <- modifyList(self$content, json)
    }
    
    return(self$content)
  }
))
