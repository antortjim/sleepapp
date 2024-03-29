helper_text <- list(
  "histogram" = div(
    tags$h2("Distribution of detected counts of a list of gene(s) across two groups of cells given by a comparison"),
    
    "Visualize in a histogram or a density plot what's the counts distribution for the same set of genes across two groups of cells.
    A Differentially Expressed Gene will display two distributions with either different shape or different heights or both",
    "Gene names should be separated by , without spaces between them",
    "Genes that are not typed exactly as used in database are dropped without warning."
  )
  ,
  "volcano" = div(
    tags$h2("Estimated effect size and significance of change for all assayed genes across two groups of cells given by a comparison"),
    tags$b("Known issues"),
    "Make sure the labels box has a number on it and it's not just blank",
    "scvi only has counts assay",
    "You may have to press submit twice to get your volcano plot. But it should take a couple of seconds to run!"
  )
)


#' A UI for fslretho
#'
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody
#' @importFrom shinydashboard sidebarMenu menuItem tabItems tabItem
#' @import shinydashboardPlus
#' @import shiny
#' @importFrom magrittr `%>%`
#' @importFrom shinybusy add_busy_bar
#' 
#' @export
shinydashboard_ui <- function() {
  
  ui <- shinydashboardPlus::dashboardPagePlus(skin = "black",
      shinydashboardPlus::dashboardHeaderPlus(
        title = "Sleepapp"
      ),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Welcome", tabName = 'welcome', icon = shiny::icon('info'))
          , shinydashboard::menuItem("Plots", tabName = 'plots', icon = shiny::icon('upload'))
          , shinydashboard::menuItem("Venn", tabName = 'venn')
        )
      ),
      # TODO Place somewhere the UI for scoreData
      shinydashboard::dashboardBody(
        shinybusy::add_busy_bar(color = "#FF0000")
        , shiny::tags$link(rel = "stylesheet", type = "text/css", href = "fslretho/css/styles.css")
        , shiny::tags$script(src = "fslretho/js/my_javascript.js")
        , shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = 'welcome', welcomePageUI("welcome"))
          , shinydashboard::tabItem(tabName = 'plots', shiny::tagList(
            plotUI("histogram", modules = c("cell_type", "grouping", "assay", "comparison_file", "geom", "gene", "submit"),
                   helper_text = helper_text[["histogram"]])
            , plotUI("volcano", modules = c("*"),
                     helper_text = helper_text[["volcano"]])
            , numericInput("volcano-labels", label = "# Labels", min = 0, max=Inf, step = 1, value=0))
          ),
          shinydashboard::tabItem(tabName = "venn", plotVennUI("venn"))
        )
      )
  )
  
  return(ui)
}


ui <- shinydashboard_ui

