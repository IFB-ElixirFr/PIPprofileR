graphic <- fluidPage(
  uiOutput("plotArea_title"),
  uiOutput("plotArea"), 

  div(id = "geneExplore", style="display : none;", 
      h2(tagList(shiny::icon("search"), "Gene exploration")), 
      fluidRow(
        column(4, 
               selectizeInput("geneExplore_selector", "Select gene", 
                              width = "100%", choices = NULL, 
                              selected = NULL, multiple = FALSE)
        ), 
        column(4, 
               uiOutput("resumeGene_general")
        ), 
        column(4, 
               uiOutput("resumeGene_attributes")
        )
      ), 
      actionButton("updateGenes", label = "Search", icon = icon("search"))
  ), 
  
  
  h2(tagList(shiny::icon("gear"), "Settings")), 
  
  tabBox(width =12, 
         # Title can include an icon
         title = NULL,
         tabPanel("General",
                  h4("Reverse plot : "),
                  switchInput(inputId = "reversePlot",NULL,  value = TRUE), 
                  h4("Plot dynamic: "),
                  switchInput(inputId = "dynamicPlot",NULL,  value = FALSE),
                  h4("Window size:"),
                  numericInput("windowSize", label = NULL, value = 500, step = 50), 
                  actionButton("updateGeneral", label = "Update", icon = icon("sync-alt"))
         ),
         tabPanel("Limits",
                  sliderInput("ylimRange", "Y limits",
                              min = 0, max = 100,
                              value = c(0,100), 
                              step = 1
                  ),
                  sliderInput("xlimRange", 'X limits', 
                              min = 0, max = 100,
                              value = c(0,100), 
                              step = 1), 
                  actionButton("updateLimits", label = "Update", icon = icon("sync-alt"))
         ),
         tabPanel("Labels",
                  fluidRow(
                    column(6,textInput("titleInput_main", "Title"), 
                           textInput("titleInput_legende", "Legend title") ), 
                    column(6, textInput("titleInput_x", "X title"),  
                           textInput("titleInput_y", "Y title"))
                  ), 
                  actionButton("updateTitle", label = "Update", icon = icon("sync-alt"))
         ),
         tabPanel("Colors", 
                  selectizeInput("speciesColor_name", "Select species", 
                                 width = "100%", choices = NULL, 
                                 selected = NULL, multiple = FALSE), 
                  colourInput("speciesColor_picker", "Select color"), 
                  actionButton("updateColor", label = "Update", icon = icon("sync-alt")) 
         ),
         tabPanel("Grid",
                  fluidRow(
                    column(4, h4("Grid color"),
                           colourInput("colMinorH", "Horizontal minor", "#DDDDDD"), 
                           colourInput("colMajorH", "Horizontal major", "#888888"),
                           colourInput("colMinorV", "Vertical minor", "#DDDDDD"),
                           colourInput("colMajorV", "Vertical major", "#888888")), 
                    column(4, h4("Grid size"),
                           numericInput("sizeMinorH", "Horizontal minor", value = 0.5, min = 0, max = 5, step = 0.5), 
                           numericInput("sizeMajorH", "Horizontal major", value = 0.5, min = 0, max = 5, step = 0.5),
                           numericInput("sizeMinorV", "Vertical minor", value = 0.5, min = 0, max = 5, step = 0.),
                           numericInput("sizeMajorV", "Vertical major", value = 0.5, min = 0, max = 5, step = 0.5) 
                    ), 
                    column(4, h4("Space between"),
                           numericInput("spaceMinorH", "Horizontal minor", value = 1000, min = 0, max =4000000 , step = 1000), 
                           numericInput("spaceMajorH", "Horizontal major", value = 5000, min = 0, max = 4000000, step = 1000),
                           numericInput("spaceMinorV", "Vertical minor", value = 10, min = 0, max = 100, step = 10),
                           numericInput("spaceMajorV", "Vertical major", value = 5, min = 0, max = 100, step = 5) 
                    )
                  ), 
                  actionButton("updateGrid", label = "Update", icon = icon("sync-alt")) 
         )
  ), 
  
  h2(tagList(shiny::icon("save"), "Save plot")), 
  fluidRow(column(3,numericInput("ggsave_width", label = "Width", min = 1, value = 29 )),
           column(3,numericInput("ggsave_height", label = "Height", min = 1, value = 21 )),
           column(3,selectInput("ggsave_unit", label = "Unit", choices = c("in", "cm", "mm"),selected = "cm")), 
           column(3,numericInput("ggsave_dpi", label = "DPI", min = 1, value = 300 )), 
           selectInput("ggsave_format", label = "Format", 
                       choices = c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg" ),
                       selected = "png")),
  downloadButton('downloadPlot','Download Plot')
  
  
)


