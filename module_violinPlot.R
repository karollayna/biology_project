
library(shiny)
library(shinyjs)
library(shinyWidgets)

#' @title Ui part of module for violin Plot
#'
#' @param id character, shiny id
#'
#' @import shinydashboard
#' @import shiny

#'
#' @export
mainModuleUI_violinPlot <- function(id){

  ns <- NS(id)

  # Application title
  titlePanel("Violin Plot")

  # Sidebar with a select input for name of assay
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = ns("assay_violin"),
        label = "Choose assay name",
        choices = ""
      ),
      uiOutput(outputId = ns("filters_violin"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotly::plotlyOutput(outputId = ns("violin_plot"))
    )
  )
}


#' @title Server part of module for violin plot
#'
#' @description Module for prepare violin plot.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
#' @export
mainModule_violinPlot <- function(input, output, session){

  attachNamespace("MultiAssayExperiment")
  data("miniACC")

  # define suported assays
  assays_to_select <- SummarizedExperiment::assays(miniACC)[1:4]

  observeEvent(TRUE, {
    #update selectInput with supported assays names
    updateSelectInput(
      session = session,
      inputId = "assay_violin",
      choices = names(assays_to_select),
      selected = ""
    )
  })

  #renderUI with two pickerInputs to select gene name and filter
  output$filters_violin <- renderUI({

    req(input$assay_violin)
    supported_assay <- SummarizedExperiment::assays(miniACC)[1:4]

    needed_assay <- SummarizedExperiment::assay(miniACC, input$assay_violin)
    #transpoze matrix because we need data from rows
    needed_assay_transpoze <- t(needed_assay)
    #create data frame with assay choosen by user
    needed_assay_df <- as.data.frame(needed_assay_transpoze)

    #render two pickerInputs to select gene name and filter
    if (input$assay_violin %in% names(supported_assay)) {
      tagList(
        pickerInput(
          inputId = session$ns("gene_violin"),
          label = "Choose gene name:",
          choices = c("", names(needed_assay_df)),
          selected = "",
          options = list(`live-search` = TRUE)
        ),
        pickerInput(
          inputId = session$ns("filters_violin"),
          label = "Choose filter:",
          choices = c("", as.list(colnames(colData(miniACC)))),
          selected = "",
          options = list(`live-search` = TRUE)
        )
      )
    }
  })

  #render violin plot
  output$violin_plot  <- plotly::renderPlotly({
    validate(
      need(input$assay_violin,
           message = "Choose assay name"),
      need(input$gene_violin,
           message = "Choose gene name"),
      need(input$filters_violin,
           message = "Choose additional filter to create a plot")
    )


    selected_assay <- SummarizedExperiment::assay(miniACC, input$assay_violin)
    #transpoze matrix because we need data from rows
    selected_assay_t <- t(selected_assay)
    #create data frame with assay choosen by user
    selected_assay_df <- as.data.frame(selected_assay_t)

    coldata <- as.data.frame(colData(miniACC))

    #add column patientID which will be merge with coldata
    #regex which is based on the name of the rows that contain patientID
    #and additional data after the third '-'
    selected_assay_df$patientID <- sub(sprintf("^((.*?-){%d}.*?)-.*", 2), "\\1",
                                       rownames(selected_assay_df)
    )
    #merge new column with coldata
    merge_df <- merge(selected_assay_df[,c(input$gene_violin, "patientID")], coldata[ , 1:30])

    x <- as.formula(paste0("~", input$filters_violin))
    y <- as.formula(paste0("~", input$gene_violin))

    #create violin plot
    violin_plot <- plotly::plot_ly(merge_df,
                                   x = x,
                                   y = y,
                                   type = "violin",
                                   box = list(visible = T),
                                   meanline = list(visible = T)
    )
    violin_plot
  })
}
