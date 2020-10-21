tableResults <- fluidPage(
  h1("Score"),
  h2("Distribution"), 
  fluidRow(
    column(6,
           withLoader(plotOutput("scorePlot", height = 700)), 
           div(style="display: inline-block", 
               dropdownButton(
                 tags$h3("Save parameters"),
                 numericInput("ggsave_width_scorePlot", label = "Width", min = 1, value = 29 ),
                 numericInput("ggsave_height_scorePlot", label = "Height", min = 1, value = 21 ),
                 selectInput("ggsave_unit_scorePlot", label = "Unit", choices = c("in", "cm", "mm"),selected = "cm"), 
                 numericInput("ggsave_dpi_scorePlot", label = "DPI", min = 1, value = 300 ),
                 selectInput("ggsave_format_scorePlot", label = "Unit", 
                             choices = c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg" ),
                             selected = "png"), 
                 circle = TRUE, status = "myClass", icon = icon("gear"), width = "300px",
                 tooltip = tooltipOptions(title = "Click to see save parameters !")
               )),
           div(style="display: inline-block", downloadButton('downloadPlot_scorePlot','Download Plot'))
    ), 
    
    column(6,
           withLoader(plotOutput("pidPlot", height = 700)), 
           div(style="display: inline-block", 
               dropdownButton(
                 tags$h3("Save parameters"),
                 numericInput("ggsave_width_pidPlot", label = "Width", min = 1, value = 29 ),
                 numericInput("ggsave_height_pidPlot", label = "Height", min = 1, value = 21 ),
                 selectInput("ggsave_unit_pidPlot", label = "Unit", choices = c("in", "cm", "mm"),selected = "cm"), 
                 numericInput("ggsave_dpi_pidPlot", label = "DPI", min = 1, value = 300 ),
                 selectInput("ggsave_format_pidPlot", label = "Unit", 
                             choices = c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg" )
                             ,selected = "png"), 
                 circle = TRUE, status = "myClass", icon = icon("gear"), width = "300px",
                 tooltip = tooltipOptions(title = "Click to see save parameters !")
               )),
           div(style="display: inline-block", downloadButton('downloadPlot_pidPlot','Download Plot'))
    )
  ),  
  
  h2("Table"), 
  dataTableOutput(outputId = "resultNto1")
)

