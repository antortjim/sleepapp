#' Sleepapp endpoint
#' @export
main <- function(debug=TRUE, port=3838) {
  
  options(shiny.launch.browser = FALSE)
  options(shiny.port = port)
  options(shiny.host = "0.0.0.0")
  options(shiny.fullstacktrace = TRUE)
  options(shiny.debug = FALSE)
  
  # Run the app
  shiny::shinyApp(server = server, ui = shinydashboard_ui())
}
