#' Application that displays Biology Project
#' @return a shiny app
#'
#' @export
runFutureApp <- function() {

  ui <- mainModuleUI_survivalPlot("main_module")

  server <- function(input, output, session){
    callModule(mainModule_survivalPlot, "main_module")
  }

  shiny::shinyApp(ui = ui, server = server)
}

