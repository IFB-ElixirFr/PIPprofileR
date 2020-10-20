resume <- fluidPage(
  h1("Summary"), 
  withLoader(dataTableOutput(outputId = "resumeQuery"))
)