plotUI <- function(id, modules="*", helper_text="Help text") {
  
  cell_types <- list(
    "γ KC" = "y_KCs",
    "αβ KC" = "a_b_KCs",
    "α'β' KC" = "a_b_prime_KCs",
    "Astrocytes" = "Astrocytes",
    "Ensheathing Glia" = "Ensheathing glia",
    "Surface Glia" = "Surface glia"
  )
  
  
  comparisons <- gsub(x = list.files("../comparisons/"), pattern = "\\.csv", replacement = "")
  ns <- shiny::NS(id)
  
  if (getOption("shiny.debug")) browser()
  inputs <- shiny::tagList(
    cell_type = shiny::selectInput(ns("cell_type"), label = "Cell Type",
                                   choices = cell_types, selected = cell_types[[1]],
                                   multiple = FALSE
    )
    , grouping = shiny::radioButtons(ns("grouping"), label = "Grouping",
                                     choices = c("Condition", "quick_clusters"),
                                     selected = "Condition", inline = TRUE
    )
    , assay = shiny::radioButtons(ns("assay"), label = "Assay",
                                  choices = c("counts", "normcounts", "logcounts"),
                                  selected = "counts", inline = TRUE
    )
    , comparison_file = shiny::selectInput(ns("comparison_file"), label = "Comparison",
                                           choices = comparisons,
                                           selected = comparisons[1]
    )
    , geom = shiny::radioButtons(ns("geom"), label = "Plot",
                                 choices = c("density", "histogram"),
                                 selected = "histogram", inline=TRUE
    )
    , method = shiny::radioButtons(ns("method"), label = "DE Method",
                                   choices = c("edgeR", "DESeq", "Wilcoxon", "MAST", "scvi"),
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
