sequenceFilters <- fluidPage(
  h1("Sequence filters"), 
  h2("Your parameters"), 
  fluidRow(
    column(3,
           h3("1- Select Reference sequence"), 
           helpText("A help text message - Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed non risus. Suspendisse lectus tortor, dignissim sit amet, adipiscing nec, ultricies sed, dolor. "), 
           selectizeInput("refPattern", NULL, 
                          width = "100%", choices = NULL, 
                          selected = NULL, multiple = FALSE)),
    column(6,
           h3("2- Select query sequences"), 
           helpText("A help text message - Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed non risus. Suspendisse lectus tortor, dignissim sit amet, adipiscing nec, ultricies sed, dolor. "), 
           uiOutput("multiInput_query"),
           actionButton("clear", "Clear"),
           actionButton("all", "All") 
    ), 
    
    column(3,
           h3("3- Select a windows size"), 
           helpText("A help text message - Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed non risus. Suspendisse lectus tortor, dignissim sit amet, adipiscing nec, ultricies sed, dolor. "), 
           numericInput("windowSize_param", label = NULL, value = 500, step = 50)
    )
  ), 
  h2("Summary"), 
  fluidRow(style="display: flex; align-items: center;text-align:center;",
           column(6,
                  dataTableOutput(outputId = "refQueryTable")),
           column(6,
                  actionButton("Run", 
                               "Run", 
                               icon = icon("play"),
                               width = 200, 
                               style="height:100px ; font-size: 20px; 
                               font-weight: bold; background-color : #3c8dbc ; 
                               color : white; ")
           )
  )
)