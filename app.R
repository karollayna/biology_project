
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(MultiAssayExperiment)
library(survival)
library(survminer)


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
            shinyjs::hidden(selectInput(inputId = "filters",
                                        label = "Filters",
                                        choices = "")),
            uiOutput("additional_filters"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    observeEvent(input$add_filters, {
        shinyjs::show("filters")
        input_select <- input$select

        filters_names <- names(colData(miniACC))
        filters_names <- filters_names[filters_names != input_select]

        updateSelectInput(session = session,
                          inputId = "filters",
                          choices = filters_names,
                          selected = ""
                          )
    })

    type_of_filter <- reactive({
        filter_name <- req(input$filters)

        if (length(unique(colData(miniACC)[[filter_name]])) < 8) {
            "checkbox"
        } else {
            "slider"
        }
    })

    output$additional_filters <- renderUI({
        filter_name <- req(input$filters)

        names_of_coldata <- unique(colData(miniACC)[[filter_name]])
        sorted_names <- as.character(sort(names_of_coldata))
        max_value <- max(as.numeric(sorted_names))

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
                min = 0,
                max = max_value,
                value = c(0, max_value)
            )
        }
    })

    data <- reactive({

        coldat <- as.data.frame(colData(miniACC))
        coldat$y <- Surv(miniACC$days_to_death, miniACC$vital_status)
        colData(miniACC) <- DataFrame(coldat)

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
