sequenceFilters <- fluidPage(
  h1("Sequence filters"), 
  h2("Your parameters"), 
  fluidRow(
    column(3,
           h3("1- Select Reference sequence"), 
           helpText("Select the reference sequence from all available sequences in the data. "), 
           selectizeInput("refPattern", NULL, 
                          width = "100%", choices = NULL, 
                          selected = NULL, multiple = FALSE)),
    column(6,
           h3("2- Select query sequences"), 
           helpText("Select the sequences to be aligned against the reference sequence."), 
           uiOutput("multiInput_query"),
           actionButton("clear", "Clear"),
           actionButton("all", "All") 
    ), 
    
    column(3,
           h3("3- Pairwise Alignment type"), 
           helpText("Type of alignment for the function pairwiseAlignment (Biostrings)."), 
           selectInput("pairwiseType", label = NULL, 
                       choices = c("global", "local", "overlap", "global-local","local-global"),
                       selected = "global-local", width = '100%'),
           p(tags$i("Documentation :")), 
           div(style="border: 1px solid black; padding: 5px 10px; background-color: #F2F2F2; border-radius: 5px; font-family: Courier New,Courier,Lucida Sans Typewriter,Lucida Typewriter,monospace;", 
               textOutput("helpPairwiseType")),
           h3("4- Select a windows size"), 
           helpText("Size of the sliding window to calculate PIP profiles"), 
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