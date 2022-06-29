
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
    titlePanel("Violin Plot"),

    # Sidebar with a select input for name of assay
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "assay_violin",
          label = "Choose assay name",
          choices = ""
        ),
        uiOutput("filters_violin")
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("violin_plot")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # define suported assays
  assays <- assays(miniACC)[1:4]

  observeEvent(TRUE, {
  #update selectInput with supported assays names
    updateSelectInput(
      session = session,
      inputId = "assay_violin",
      choices = names(assays),
      selected = ""
    )
  })

  output$filters_violin <- renderUI({
    req(input$assay_violin)
    supported_assay <- assays[c("RNASeq2GeneNorm", "gistict", "RPPAArray", "Mutations")]

    needed_assay <- assay(miniACC, input$assay_violin)
    #transpoze matrix because we need data from rows
    needed_assay_transpoze <- t(needed_assay)
    #create data frame with assay choosen by user
    needed_assay_df <- as.data.frame(needed_assay_transpoze)

    #renderUI with two pickerInputs to select gene name and filter
    if (input$assay_violin %in% names(supported_assay)) {
      tagList(
        pickerInput(
          inputId = "gene_violin",
          label = "Choose gene name:",
          choices = c("", names(needed_assay_df)),
          selected = "",
          options = list(`live-search` = TRUE)
        ),
        pickerInput(
          inputId = "filters_violin",
          label = "Choose filter:",
          choices = c("", as.list(colnames(colData(miniACC)))),
          selected = "",
          options = list(`live-search` = TRUE)
        )
      )
    }
  })

  output$violin_plot  <- renderPlotly({
    validate(
      need(input$assay_violin,
           message = "Choose assay name"),
      need(input$gene_violin,
           message = "Choose gene name"),
      need(input$filters_violin,
           message = "Choose additional filter to create a plot")
    )


    selected_assay <- assay(miniACC, input$assay_violin)
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

# Run the application
shinyApp(ui = ui, server = server)
