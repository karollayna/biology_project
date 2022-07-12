#' Application that displays violin plot
#' @return a shiny app
#'
#' @export
runViolinApp <- function() {

  ui <- violinPlotUI("violinPlot")

  server <- function(input, output, session){
    callModule(violinPlot, "violinPlot")
  }

  shiny::shinyApp(ui = ui, server = server)
}
