resume <- fluidPage(
  h1("Summary"), 
    h2("Reference "), 
    uiOutput("refName_ui"), 
    uiOutput("refSize_ui"),
    h2("Alignment results"), 
    dataTableOutput(outputId = "resumeQuery")
)