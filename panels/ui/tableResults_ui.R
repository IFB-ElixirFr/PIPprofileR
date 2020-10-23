tableResults <- fluidPage(
  h1("Score"), 
  withLoader(dataTableOutput(outputId = "pipResults"))
)

