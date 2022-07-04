#' Application that displays scatter plot
#' @return a shiny app
#'
#' @export
runFutureApp <- function() {

  ui <- mainModuleUI_scatterPlot("main_module")

  server <- function(input, output, session){
    callModule(mainModule_scatterPlot, "main_module")
  }

  shiny::shinyApp(ui = ui, server = server)
}
