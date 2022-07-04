#' @title Ui part of module for Scatter Plot
#'
#' @param id character, shiny id
#'
#' @import shinydashboard
#' @import shiny
#' @import shinyjs
#' @import shinyWidgets
#'
#' @export
mainModuleUI_scatterPlot <- function(id){

  ns <- NS(id)

  # Application title
  titlePanel("Scatter plot")

  # Sidebar with a select input for name of assay
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = ns("assay_name"),
        label = "Choose assay name",
        choices = "",
      ),
      uiOutput(outputId = ns("filters"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotly::plotlyOutput(outputId = ns("scatter_plot"))
    )
  )
}



#' @title Server part of module for Scatter plot
#'
#' @description Module for prepare scatter plot.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
#' @export
mainModule_scatterPlot <- function(input, output, session){

  attachNamespace("MultiAssayExperiment")
  data("miniACC")

  #add assays to choose
  observeEvent(TRUE, {
    assays_to_select <- SummarizedExperiment::assays(miniACC)

    # filter out not supported assays
    # valid_assays <- assays[!"miRNASeqGene" %in% assays]
    valid_assays <- assays_to_select[c("RNASeq2GeneNorm", "gistict", "RPPAArray", "Mutations")]

    #update selectInput with supported assays names
    updateSelectInput(session = session,
                      inputId = "assay_name",
                      label = "Choose assay name",
                      choices = c("", names(valid_assays)),
                      selected = ""
    )
  })

  #create reactive expression with data frame from selected assay name
  assays_names_r <- reactive({
    needed_assay <- SummarizedExperiment::assay(miniACC, input$assay_name)
    needed_assay_transpoze <- t(needed_assay)
    needed_assay_df <- as.data.frame(needed_assay_transpoze)
  })

  #add pickerInput for selected gene names needed to create plot
  output$filters <- renderUI({
    req(input$assay_name)

    if (input$assay_name %in% names(SummarizedExperiment::assays(miniACC))) {
      tagList(
        pickerInput(
          inputId = session$ns("gene_name_1"),
          label = "Choose gene name 1:",
          choices = c("", names(assays_names_r())),
          selected = "",
          options = list(
            `live-search` = TRUE)
        ),
        pickerInput(
          inputId = session$ns("gene_name_2"),
          label = "Choose gene name 2:",
          choices = c("", names(assays_names_r())),
          selected = "",
          options = list(
            `live-search` = TRUE)
        )
      )
    }
  })

  #create correlation plot of selected gene names
  output$scatter_plot <- plotly::renderPlotly ({
    validate(
      need(input$assay_name,
           message = "Choose assay name"),
      need(input$gene_name_1,
           message = "Choose gene name 1"),
      need(input$gene_name_2,
           message = "Choose gene name 2 to create a plot")
    )

    x <- as.formula(paste0("~", input$gene_name_1))
    y <- as.formula(paste0("~", input$gene_name_2))

    plotly::plot_ly(data = assays_names_r(), x = x, y = y)
  })
}
