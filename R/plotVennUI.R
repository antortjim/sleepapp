plotVennUI <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
      shiny::wellPanel(
      "Inputs"
      , shiny::tagList(
        shiny::selectInput(ns("group1"), label = "Group 1",
                           choices = get_diff_tables(),
                           multiple = TRUE)
        , shiny::textInput(ns("name1"), label = "Group 1", value = "group_1")
        , shiny::selectInput(ns("group2"), label = "Group 2",
                             choices = get_diff_tables(),
                             multiple = TRUE)
        , shiny::textInput(ns("name2"), label = "Group 2", value = "group_2")
        , shiny::selectInput(ns("group3"), label = "Group 3",
                             choices = get_diff_tables(),
                             multiple = TRUE)
        , shiny::textInput(ns("name3"), label = "Group 3", value = "group_3")
        
        , shiny::radioButtons(ns("sign"), label = "Sign", selected = 0, choices = -1:1, inline = TRUE)
        , shiny::sliderInput(ns("sig.thresh"), label = "Significance", min = 0, max = 1, value = 0.05, step = 0.01)
        , shiny::sliderInput(ns("es.thresh"), label = "Effect size", min = 0, max = 100, value = 0, step = 0.1)
        , shiny::actionButton(ns("submit"), label = "Submit")
      )
    )
    , wellPanel(
      plotOutput(ns("plot"))
      , htmlOutput(ns("text")))
  )
}