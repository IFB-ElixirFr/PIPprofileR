graphic <- fluidPage(
  uiOutput("plotArea_title"),
  uiOutput("plotArea"),


  fluidRow(

    column(6,
           fluidRow(
             h2(tagList(shiny::icon("info-circle"), "Exploration PIP")),
             column(width = 6,
                    h3("Position values"),
                    helpText("Hover the plot to get the PIP values"),
                    uiOutput("hover_info")
             ),
             column(width = 6,
                    h3("Mean of selected region"),
                    helpText("Select an area.  The average of the PIPs for each strain in the zone will be calculated."),
                    uiOutput("brush_info")

             )
           )

    ),
    column(6,
           h2(tagList(shiny::icon("pen"), "Annotation")),
           helpText("Hover over the plot to get the annotation information"),
           uiOutput("hover_info_annot")
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
                    column(4,numericInput("ggsave_width", label = "Width", min = 1, value = 20, width = '100%' )),
                    column(4,numericInput("ggsave_height", label = "Height", min = 1, value = 5, width = '100%' )),
                    column(4,selectInput("ggsave_unit", label = "Unit", choices = c("in", "cm", "mm"),selected = "in", width = '100%'))),

                  fluidRow(style="display: flex; align-items: center;",
                           column(4,numericInput("ggsave_dpi", label = "DPI", min = 1, value = 300, width = '100%' )),
                           column(4, selectInput("ggsave_format", label = "Format",
                                                 choices = c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg" ),
                                                 selected = "png", width = '100%')),
                           column(4, style="text-align:center;", downloadButton('downloadPlot','Download Plot')))
         )
  )
)


