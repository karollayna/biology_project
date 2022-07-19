## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Biology project",
                  titleWidth = 180),
  dashboardSidebar(
    width = 180,
    sidebarMenu(id = "lol",
      menuItem("Survival plot", tabName = "survival_plot"),
      menuItem("Scatter plot", tabName = "scatter_plot"),
      menuItem("Violin plot", tabName = "violin_plot")
    )
  ),

  dashboardBody(
    tabItems(
      # First tab content
      tabItem("survival_plot", survivalPlotUI("survivalPlot")),
      tabItem("scatter_plot", scatterPlotUI("scatterPlot")),
      tabItem("violin_plot", violinPlotUI("violinPlot"))
    )
  )
)

server <- function(input, output) {

  callModule(survivalPlot, "survivalPlot")

  callModule(scatterPlot, "scatterPlot")

  callModule(violinPlot, "violinPlot")

}

shinyApp(ui, server)
