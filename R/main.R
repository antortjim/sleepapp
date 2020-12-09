#' Sleepapp endpoint
#' @export
main <- function(debug=TRUE) {
  
  devtools::load_all()
  
  ROOT_DIR <-  "/1TB/Cloud/Lab/Projects/SleepSignature/workflow/"
  WORKING_DIR <-  "results/20201209/"
  root <- file.path(ROOT_DIR, WORKING_DIR)
  options(shiny.launch.browser = FALSE)
  options(shiny.port = 3838)
  options(shiny.host = "0.0.0.0")
  options(shiny.fullstacktrace = TRUE)
  options(shiny.debug = TRUE)
  
  # Run the app
  shiny::shinyApp(server = server, ui = shinydashboard_ui())
}