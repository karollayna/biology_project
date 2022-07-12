#' Application that displays Biology Project
#' @return a shiny app
#'
#' @export
runSurvivalApp <- function() {

  ui <- survivalPlotUI("survivalPlot")

  server <- function(input, output, session){
    callModule(survivalPlot, "survivalPlot")
  }

  shiny::shinyApp(ui = ui, server = server)
}

