library(shiny)
library(shinyjs)
library(shinyWidgets)
library(MultiAssayExperiment)
library(survival)
library(survminer)

data(miniACC)


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  useShinyjs(),
  titlePanel("Biology project"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "select",
                  label = h3("Select box"),
                  choices = as.list(colnames(colData(miniACC))),
                  selected = 1),
      shinyWidgets::actionBttn(inputId = "add_filters",
                               label = "Add filters"),
      uiOutput("filters"),
      uiOutput("additional_filters")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # reactive dependency on input$add_filters
  input_select <- eventReactive(input$add_filters, {
    input$select
  })

# add filters after clicking on button
  output$filters <- renderUI({

    #local variable
    .input_select <- req(input_select())

    filters_names <- names(colData(miniACC))
    filters_names <- filters_names[filters_names != .input_select]

    selectInput(inputId = "filters",
                label = "Choose filters names",
                choices = filters_names,
                selected = ""
                )
  })


  #type of filter (checkbox or slider) dependent on the class and number of filters
  type_of_filter <- reactive({
    filter_name <- req(input$filters)

    if (class(colData(miniACC)[[filter_name]]) == "character") {
      "checkbox"
    } else if ((class(colData(miniACC)[[filter_name]]) == "integer") &&
               length(unique(colData(miniACC)[[filter_name]])) == 2) {
      "checkbox"
    } else {
      "slider"
    }
  })

  #render UI dependent on selected filters
  output$additional_filters <- renderUI({
    filter_name <- req(input$filters)

    #create data frame with unique names of coldata of selected assay
    names_of_coldata <- unique(colData(miniACC)[[filter_name]])
    #sort names of coldata
    sorted_names <- as.character(sort(names_of_coldata))
    max_value <- max(as.numeric(sorted_names))
    min_value <- min(as.numeric(sorted_names))

    #create checkbox or slider dependent on selected filter
    if (type_of_filter() == "checkbox") {

      prettyCheckboxGroup(
        inputId = "additional_filters01",
        label = "Filters",
        choices = sorted_names,
        icon = icon("check-square-o"),
        status = "primary",
        outline = TRUE,
        animation = "jelly"
      )
    } else {
      sliderInput(
        inputId = "additional_filters01",
        label = "Filters",
        min = min_value,
        max = max_value,
        value = c(min_value, max_value)
      )
    }
  })

  data <- reactive({

    #covert colData to data frame
    coldat <- as.data.frame(colData(miniACC))
    #create new column for survival analysis
    coldat$y <- Surv(miniACC$days_to_death, miniACC$vital_status)
    colData(miniACC) <- DataFrame(coldat)

    #remove any patients missing overall survival information
    miniACC <- miniACC[, complete.cases(coldat$y),]
    coldat <- as(colData(miniACC), "data.frame")

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
    fit <- survfit(x, data = coldat)
    fit$call$formula <- x

    ggsurvplot(fit, data = coldat)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
