
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(survival)
library(survminer)
library(MultiAssayExperiment)
library(plotly)


data(miniACC)

# Define UI for application that draws a plot
ui <- fluidPage(

    # Application title
    titlePanel("Scatter plot"),

    # Sidebar with a select input for name of assay
    sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "assay_name",
            label = "Choose assay name",
            choices = "",
          ),
          uiOutput("filters")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("scatter_plot")
        )
    )
)

server <- function(input, output, session) {

#add assays to choose
  observeEvent(TRUE, {
    assays <- assays(miniACC)

    #filter out not supported assays
    # valid_assays <- assays[!"miRNASeqGene" %in% assays]
    valid_assays <- assays[c("RNASeq2GeneNorm", "gistict", "RPPAArray", "Mutations")]

    #update selectInput with supported assays names
    updateSelectInput(session = session,
                      inputId = "assay_name",
                      label = "Choose assay name",
                      choices = names(valid_assays),
                      selected = ""
                      )
  })

  #create reactive expression with data frame from selected assay name
  assays_names_r <- reactive({
      needed_assay <- assay(miniACC, input$assay_name)
      needed_assay_transpoze <- t(needed_assay)
      needed_assay_df <- as.data.frame(needed_assay_transpoze)
  })

  #add pickerInput for selected gene names needed to create plot
  output$filters <- renderUI({
    req(input$assay_name)

    if (input$assay_name %in% names(assays(miniACC))) {
      tagList(
        pickerInput(
          inputId = "gene_name_1",
          label = "Choose gene name 1:",
          choices = c("", names(assays_names_r())),
          selected = "",
          options = list(
            `live-search` = TRUE)
        ),
        pickerInput(
          inputId = "gene_name_2",
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
  output$scatter_plot <- renderPlotly ({
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

# Run the application
shinyApp(ui = ui, server = server)
