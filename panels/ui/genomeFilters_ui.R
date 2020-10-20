genomeFilters <- fluidPage(
  h1("Genome filters"), 
  fluidRow(
    column(3,
           selectizeInput("refPattern", "Select Reference genome", 
                          width = "100%", choices = NULL, 
                          selected = NULL, multiple = FALSE)),
    column(4,
           uiOutput("multiInput_query"),
           actionButton("clear", "Clear"),
           actionButton("all", "All") 
    ), 
    
    column(5,
           dataTableOutput(outputId = "refQueryTable"))
    
  )
)