plotUI <- function(id, modules="*", helper_text="Help text") {
  
  config <- SleepAppConfiguration$new()$content
  
  cell_types <- config$cell_types
  assays_available <- config$assays_available
  comparisons_available <- config$comparisons_available
  groupings_available <- config$groupings_available
  methods_available <- config$methods_available
  
  ns <- shiny::NS(id)
  
  if (getOption("shiny.debug")) browser()
  inputs <- shiny::tagList(
    cell_type = shiny::selectInput(ns("cell_type"), label = "Cell Type",
                                   choices = cell_types, selected = cell_types[[1]],
                                   multiple = FALSE
    )
    , grouping = shiny::radioButtons(ns("grouping"), label = "Grouping",
                                     choices = groupings_available,
                                     selected = "Condition", inline = TRUE
    )
    , assay = shiny::radioButtons(ns("assay"), label = "Assay",
                                  choices = assays_available,
                                  selected = "counts", inline = TRUE
    )
    , comparison_file = shiny::selectInput(ns("comparison_file"), label = "Comparison",
                                           choices = comparisons_available,
                                           selected = comparisons_available[1]
    )
    , geom = shiny::radioButtons(ns("geom"), label = "Plot",
                                 choices = c("density", "histogram"),
                                 selected = "histogram", inline=TRUE
    )
    , method = shiny::radioButtons(ns("method"), label = "DE Method",
                                   choices = methods_available,
                                   selected = "Wilcoxon", inline=TRUE)
    
    , gene = shiny::textInput(ns("gene"), label = "Gene", value = "", placeholder = "Act5C")
    
    , submit = shiny::actionButton(ns("submit"), label = "Submit")
  )
  
  if (all(modules == "*")) modules <- names(inputs)
  
  
  shiny::tagList(
    fluidRow(
      column(wellPanel(
        tagList(p("Inputs"), inputs[modules])
      ), width = 8),
      column(wellPanel(
        helper_text
      ), width = 4)
    )
    , wellPanel(
      plotOutput(ns("plot"))
    )
  )
}
