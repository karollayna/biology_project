#' Application that displays scatter plot
#' @return a shiny app
#'
#' @export
runScatterApp <- function() {

  ui <- scatterPlotUI("scatterPlot")

  server <- function(input, output, session){
    callModule(scatterPlot, "scatterPlot")
  }

  shiny::shinyApp(ui = ui, server = server)
}
