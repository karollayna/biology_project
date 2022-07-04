library(shiny)
library(shinyjs)
library(shinyWidgets)

#' @title Ui part of module for survival plot
#'
#' @param id character, shiny id
#'
#' @import shinydashboard
#' @import shiny

#'
#' @export
mainModuleUI_survivalPlot <- function(id){

  ns <- NS(id)

  # Application title
  useShinyjs()
  titlePanel("Biology project")

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = ns("select"),
                  label = "Select box",
                  choices = ""
                  ),
      shinyWidgets::actionBttn(inputId = ns("add_filters"),
                               label = "Add filters"),
      uiOutput(outputId = ns("filters")),
      uiOutput(outputId = ns("additional_filters"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput(outputId = ns("distPlot"))
    )
  )
}


#' @title Server part of module for survival plot.
#'
#' @description Module for prepare survival plot.
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
#' @export
mainModule_survivalPlot <- function(input, output, session){

  loadNamespace("MultiAssayExperiment")
  data("miniACC")

  observeEvent(TRUE, {
    updateSelectInput(
      session = session,
      inputId = "select",
      label = "Select box",
      choices = as.list(BiocGenerics::colnames(SummarizedExperiment::colData(miniACC))),
      selected = ""
    )
  })

  # reactive dependency on input$add_filters
  input_select <- eventReactive(input$add_filters, {
    input$select
  })

  # add filters after clicking on button
  output$filters <- renderUI({

    #local variable
    .input_select <- req(input_select())

    filters_names <- names(SummarizedExperiment::colData(miniACC))
    filters_names <- filters_names[filters_names != .input_select]

    selectInput(inputId = session$ns("filters"),
                label = "Choose filters names",
                choices = filters_names,
                selected = ""
    )
  })


  #type of filter (checkbox or slider) dependent on the class and number of filters
  type_of_filter <- reactive({
    filter_name <- req(input$filters)

    if (class(SummarizedExperiment::colData(miniACC)[[filter_name]]) == "character") {
      "checkbox"
    } else if ((class(SummarizedExperiment::colData(miniACC)[[filter_name]]) == "integer") &&
               length(unique(SummarizedExperiment::colData(miniACC)[[filter_name]])) == 2) {
      "checkbox"
    } else {
      "slider"
    }
  })

  #render UI dependent on selected filters
  output$additional_filters <- renderUI({
    filter_name <- req(input$filters)

    #create data frame with unique names of coldata of selected assay
    names_of_coldata <- unique(SummarizedExperiment::colData(miniACC)[[filter_name]])
    #sort names of coldata
    sorted_names <- as.character(sort(names_of_coldata))
    max_value <- max(as.numeric(sorted_names))
    min_value <- min(as.numeric(sorted_names))

    #create checkbox or slider dependent on selected filter
    if (type_of_filter() == "checkbox") {

      prettyCheckboxGroup(
        inputId = session$ns("additional_filters01"),
        label = "Filters",
        choices = sorted_names,
        icon = icon("check-square-o"),
        status = "primary",
        outline = TRUE,
        animation = "jelly"
      )
    } else {
      sliderInput(
        inputId = session$ns("additional_filters01"),
        label = "Filters",
        min = min_value,
        max = max_value,
        value = c(min_value, max_value)
      )
    }
  })

  data <- reactive({

    #covert colData to data frame
    coldat <- as.data.frame(SummarizedExperiment::colData(miniACC))
    #create new column for survival analysis
    coldat$y <- survival::Surv(miniACC$days_to_death, miniACC$vital_status)
    colData(miniACC) <- DataFrame(coldat)

    #remove any patients missing overall survival information
    miniACC <- miniACC[, complete.cases(coldat$y),]
    coldat <- as(SummarizedExperiment::colData(miniACC), "data.frame")

    if (type_of_filter() == "checkbox") {
      coldat <- coldat[coldat[[input$filters]] %in% input$additional_filters01, ]
    } else {
      coldat <-
        coldat[min(input$additional_filters01) < coldat[[input$filters]] &&
                 coldat[[input$filters]] < max(input$additional_filters01), ]
    }

    coldat
  })

  #create survival plot
  output$distPlot <- renderPlot({
    req(input$select)
    req(input$filters)
    req(input$additional_filters01)

    coldat <- data()

    shiny::validate(
      need(NROW(coldat) != 0,
           message = "There is no data to display the plot. Please, select a different range of values")
    )

    x <- as.formula(paste0("y ~ ", input$select))
    fit <- survival::survfit(x, data = coldat)
    fit$call$formula <- x

    survminer::ggsurvplot(fit, data = coldat)
  })
}

