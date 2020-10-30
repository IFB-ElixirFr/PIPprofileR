graphic <- fluidPage(
  uiOutput("plotArea_title"),
  uiOutput("plotArea"), 

  h2(tagList(shiny::icon("info-circle"), "Exploration")), 
  fluidRow(
    column(width = 4,
           h3("Position values"),
           helpText("Hover the plot to get the PIP values"),
           uiOutput("hover_info")
    ), 
    column(width = 4,
           h3("Annotation information"),
           helpText("Hover over the plot to get the annotation information"),
           uiOutput("hover_info_annot")
    ), 
    column(width = 4,
           h3("Mean area"),
           helpText("Select an area.  The average of the PIPs for each strain in the zone will be calculated."), 
           uiOutput("brush_info")
    )
  ),
  
  div(id = "geneExplore", style="display : none;", 
      h2(tagList(shiny::icon("search"), "Feature exploration")), 
      fluidRow(
        column(4, 
               selectizeInput("geneExplore_selector", "Select feature", 
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
      actionButton("updateGenes", label = "Focus", icon = icon("search"))
  ), 
  
  h2(tagList(shiny::icon("gear"), "Settings")), 
  
  tabBox(width =12, 
         title = NULL,
         tabPanel("General",
                  fluidRow(
                    column(6,
                           h4("Closest sequence on top"),
                           switchInput(inputId = "reversePlot",NULL,  
                                       value = TRUE,
                                       size = "mini"),
                           h4("Dynamic plot "),
                           switchInput(inputId = "dynamicPlot",NULL,  
                                       value = FALSE,
                                       size = "mini"),
                           h4("Size of the sliding window"),
                           numericInput("windowSize", label = NULL, value = 500, step = 50), 
                           actionButton("updateGeneral", label = "Update", icon = icon("sync-alt"))
                    ), 
                    column(6,
                           h4("Limits"),
                           sliderInput("ylimRange", "Y limits",
                                       min = 0, max = 100,
                                       value = c(0,100), 
                                       step = 1, width = '100%'
                           ),
                           sliderInput("xlimRange", 'X limits', 
                                       min = 0, max = 100,
                                       value = c(0,100), 
                                       step = 1, width = '100%'), 
                           actionButton("updateLimits", label = "Update", icon = icon("sync-alt"))
                    )
                  )
         ),
         tabPanel("Labels",
                  fluidRow(
                    column(6,textInput("titleInput_main", "Title", width = '100%'), 
                           textInput("titleInput_legende", "Legend title", width = '100%') ), 
                    column(6, textInput("titleInput_x", "X title", width = '100%'),  
                           textInput("titleInput_y", "Y title", width = '100%'))
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
                           colourInput("colMajorX", "X major", "#888888"),
                           colourInput("colMinorX", "X minor", "#DDDDDD"), 
                           colourInput("colMajorY", "Y major", "#888888"), 
                           colourInput("colMinorY", "Y minor", "#DDDDDD")
                           ),

                    column(4, h4("Grid size"),
                           numericInput("sizeMajorX", "X major", value = 0.5, min = 0, max = 5, step = 0.5, width = '100%'),
                           numericInput("sizeMinorX", "X minor", value = 0.5, min = 0, max = 5, step = 0.5, width = '100%'), 
                           numericInput("sizeMajorY", "Y major", value = 0.5, min = 0, max = 5, step = 0.5, width = '100%'),
                           numericInput("sizeMinorY", "Y minor", value = 0.5, min = 0, max = 5, step = 0.5, width = '100%')
                           
                    ), 
                    column(4, h4("Space between"),
                           numericInput("spaceMajorX", "X major", value = 5000, min = 0, max = 4000000, step = 1000, width = '100%'),
                           numericInput("spaceMinorX", "X minor", value = 1000, min = 0, max = 4000000 , step = 1000, width = '100%'), 
                           
                           numericInput("spaceMajorY", "Y major", value = 10, min = 0, max = 100, step = 10, width = '100%'), 
                           numericInput("spaceMinorY", "Y minor", value = 5, min = 0, max = 100, step = 5, width = '100%')
                           
                    )
                  ), 
                  actionButton("updateGrid", label = "Update", icon = icon("sync-alt")) 
         ), 
         tabPanel("Save", 
                  fluidRow(
                    column(4,numericInput("ggsave_width", label = "Width", min = 1, value = 29, width = '100%' )),
                    column(4,numericInput("ggsave_height", label = "Height", min = 1, value = 21, width = '100%' )),
                    column(4,selectInput("ggsave_unit", label = "Unit", choices = c("in", "cm", "mm"),selected = "cm", width = '100%'))),
                  
                  fluidRow(style="display: flex; align-items: center;",
                           column(4,numericInput("ggsave_dpi", label = "DPI", min = 1, value = 300, width = '100%' )),
                           column(4, selectInput("ggsave_format", label = "Format", 
                                                 choices = c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg" ),
                                                 selected = "png", width = '100%')),
                           column(4, style="text-align:center;", downloadButton('downloadPlot','Download Plot')))
         )
  )
)


