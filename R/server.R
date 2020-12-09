#' @export
server <- function(input, output, session) {
  
  plotHistogramServer("histogram")
  plotVolcanoServer("volcano")
  
}