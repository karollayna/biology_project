#' Application that displays violin plot
#' @return a shiny app
#'
#' @export
runFutureApp <- function() {

  ui <- mainModuleUI_violinPlot("main_module")

  server <- function(input, output, session){
    callModule(mainModule_violinPlot, "main_module")
  }

  shiny::shinyApp(ui = ui, server = server)
}
