#' @import SingleCellExperiment
plotHistogramServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      rds_path <- reactive({
        if (getOption("shiny.debug")) browser()
        
        path <- build_sce_rds_path(input$cell_type, input$grouping, normalized = TRUE)
        validate(need(file.exists(path), "Please provide a combination that leads to an existing cached SingleCellExperiment"))
        message(path)
        path
      })
      
      sce <- eventReactive(input$submit, {
        require("SingleCellExperiment")
        require("scuttle")
        if (getOption("shiny.debug")) browser()
        
        sce <- readRDS(rds_path())
        validate(need("SingleCellExperiment" %in% class(sce), "Error loading SingleCellExperiment from cache"))
        sce
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      genes_char <- reactive({
        genes <- unlist(strsplit(input$gene, split = ","))
        message("#############################")
        message(glue::glue("Genes: {genes}"))
        message("#############################")
        validate(need(length(genes) != 0 & genes != "", "Please enter a gene name"))
        genes
      })
      
      genes <- reactive({
        aval_genes <- genes_char()[genes_char() %in% SingleCellExperiment::rowData(sce())$Gene]
        nonaval_genes <- genes_char()[!genes_char() %in% SingleCellExperiment::rowData(sce())$Gene]
        if (length(nonaval_genes) == 0)  message(glue::glue("Genes : {nonaval_genes} not available in the dataset"))
        aval_genes
      })
      
      output$plot <- renderPlot({
        input$submit
          # validate(need(input$submit != 0, ""))
        if (getOption("shiny.debug")) browser()
        
          isolate({
            plotHistogram(
              sce(), gene = genes(), comparison_file = build_comparison_path(input$comparison_file),
              by_exprs_values = input$assay, geom = input$geom
            )
          })
      })
    }
  )
}

#'
plotVennServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      data_list_list <- eventReactive(input$submit, {
        abs_dir <- SleepAppConfiguration$new()$content$abs_dir
        
        groups <- paste0("group", 1:3) %>% lapply(., function(g) {
          input[[g]] %>% sapply(., function(x) file.path(abs_dir, x))
          
        })
        names(groups) <- c(input$name1, input$name2, input$name3)
        groups
      })
      
      observeEvent(input$submit, {
        data_list_list()
      })
      
      output$text <- renderText({
        ensembles <- computeGeneEnsembles(computePartitionCounts(data_list_list()), names(data_list_list()))
        text <- lapply(
          1:length(ensembles), function(i) {
            paste0("<b>", names(ensembles)[i], "</b>", ":", ensembles[i])
          }
        ) %>% unlist %>% paste(., collapse = "<br>")
        
        text
      })
      
      output$plot <- renderPlot({
        plotVenn(data_list_list(), sign=input$sign, effect.size_threshold = input$es.thresh, signif_threshold=input$sig.thresh)
      })
    }
  )
}

#' @import rlang
plotVolcanoServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      diff_table_id <- reactive({
        list(
          cell_type = input$cell_type, method = input$method, assay = input$assay,
          comparison = input$comparison_file, grouping = input$grouping
        )
      })
      
      
      diff_table_path <- eventReactive(input$submit, {
        if (getOption("shiny.debug")) browser()
        
        path <- rlang::exec(build_diff_table_path, !!!diff_table_id())
        validate(need(file.exists(path), "Passed inputs dont match any file in the DE database"))
        path
      })
      
      diff_table <- reactive({
        if (getOption("shiny.debug")) browser()
        read_methods[[input$method]](diff_table_path())
      })
      
      genes_char <- reactive({
        if (getOption("shiny.debug")) browser()
        genes <- unlist(strsplit(input$gene, split = ","))
        if (length(genes) == 0) genes <- NULL
        genes
      })
      
      genes_validated <- reactive({
        validate(need(all(genes_char() %in% diff_table()$Gene) | is.null(genes_char()), "Please enter a gene available in the selected dataset. Change the gene name or change the dataset"))
        genes_char()
      })
      
      
      output$plot <- renderPlot({
        input$submit
        if (getOption("shiny.debug")) browser()
        rlang::exec(plotVolcano, diff_table(), labels=input$labels, genes=genes_validated(), !!!diff_table_id())
      })
      
      
    }
  )
}
