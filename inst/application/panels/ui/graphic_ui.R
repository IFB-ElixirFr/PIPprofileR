graphic <- fluidPage(
  uiOutput("plotArea_title"),
  uiOutput("plotArea"),
  fluidRow(
    column(8,
           h2(tagList(shiny::icon("info-circle"), "PIP exploration")),
           # column(width = 6,
           #        h3("Position values"),
           #        helpText("Hover the plot to get the PIP values"),
           #        uiOutput("hover_info")
           # ),
           # column(width = 6,
           #        h3("Mean of selected region"),
           #        helpText("Select an area.  The average of the PIPs for each strain in the zone will be calculated."),
           #        uiOutput("brush_info")
           #
           # )
           helpText("The table below is dynamic. Hover the graph or select an area"),
           DTOutput(outputId = "pipExplo",height = '600px'),
           tags$br(),
           helpText("Show the strains / species displayed in the table below"),
           actionButton("selectCB", "Update")
    ),
    column(4,
           h2(tagList(shiny::icon("pen"), "Annotation")),
           helpText("Hover over the plot to get the annotation information. The blue arrows are strand + and the orange arrows strand -."),
           uiOutput("hover_info_annot")
    )
  ),

  h2(tagList(shiny::icon("table"), "Information")),

  fluidRow(tabBox(width =12,
                  title = NULL,
                  tabPanel("Resume",
                           uiOutput("refName_ui"),
                           uiOutput("refSize_ui"),
                           uiOutput("refType_ui"),
                           uiOutput("nbQuerry"),
                           uiOutput("querrySizeMean_ui"),
                           uiOutput("refTypeAlignemnt_ui"),
                  ),
                  tabPanel("Sequence information",
                           withLoader(dataTableOutput(outputId = "resumeQuery"))
                  ),
                  tabPanel("PIP statistics",
                           withLoader(dataTableOutput(outputId = "pipStat"))
                  ),
                  tabPanel("Alignments",
                           withLoader(dataTableOutput(outputId = "resultNto1")),
                           h3("Distribution"),
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
                                                      selected = "pdf"),
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
                                                      ,selected = "pdf"),
                                          circle = TRUE, status = "myClass", icon = icon("gear"), width = "300px",
                                          tooltip = tooltipOptions(title = "Click to see save parameters !")
                                        )),
                                    div(style="display: inline-block", downloadButton('downloadPlot_pidPlot','Download Plot'))
                             )
                           )
                  )
  )),

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
                  fluidRow(column(6,
                  selectizeInput("speciesColor_name", "Select sequence",
                                 width = "100%", choices = NULL,
                                 selected = NULL, multiple = FALSE),
                  colourInput("speciesColor_picker", "Select color"),
                  actionButton("updateColor", label = "Update", icon = icon("sync-alt"))),
                  column(6,
                         h4("Import color file"),
                         helpText("Available in the zip folder when you save an exploration."),
                         fileInput("file_color",label = NULL,
                                   buttonLabel = "Browse...",
                                   placeholder = "No file selected"),
                         ),
                        actionButton("updateColor_file_update", label = "Update with file", icon = icon("sync-alt"))
                  )
         ),
         tabPanel("Sequences",
                  h3("Displayed sequences"),
                  helpText("Select the sequences to be displayed in plot."),
                  uiOutput("multiInput_display"),
                  actionButton("clear_display", "Clear"),
                  actionButton("all_display", "All"),
                  actionButton("update_display", "Update")
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
                                                 selected = "pdf", width = '100%')),
                           column(4, style="text-align:center;", downloadButton('downloadPlot','Download Plot')))
         )
  )
)


