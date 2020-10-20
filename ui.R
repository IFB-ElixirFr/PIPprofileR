################################################################################
# Title : PIPprofileR - UI
# Organism : All 
# Omics area : Omics
# Users : Thomas Denecker
# Email : thomas.denecker@gmail.com
# Date : Oct, 2020
# GitHub : 
# DockerHub : 
################################################################################

################################################################################
###                                Library                                   ###
################################################################################

#===============================================================================
# Application
#===============================================================================
suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(shinyjs)))
suppressMessages(suppressWarnings(library(shinyalert)))
suppressMessages(suppressWarnings(library(shinydashboard)))
suppressMessages(suppressWarnings(library(shinydashboardPlus)))
suppressMessages(suppressWarnings(library(shinyWidgets)))
suppressMessages(suppressWarnings(library(shinycssloaders)))
suppressMessages(suppressWarnings(library(shinyhelper)))
suppressMessages(suppressWarnings(library(colourpicker)))
suppressMessages(suppressWarnings(library(shinycustomloader)))

#===============================================================================
# Read FASTA files 
#===============================================================================
suppressMessages(suppressWarnings(library(Biostrings)))

#===============================================================================
# Read annotation files 
#===============================================================================
suppressMessages(suppressWarnings(library(ape)))

#===============================================================================
# Visualization
#===============================================================================
suppressMessages(suppressWarnings(library(DT)))
suppressMessages(suppressWarnings(library(plotly)))

#===============================================================================
# Data treatment 
#===============================================================================
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(reshape2)))
suppressMessages(suppressWarnings(library(knitr)))

#===============================================================================
# Write report
#===============================================================================
suppressMessages(suppressWarnings(library(xlsx)))
suppressMessages(suppressWarnings(library(rlist)))

################################################################################
###                           FUNCTIONS                                      ###
################################################################################

source("./R_scripts/align_n_to_one.R")

################################################################################
###                             PANELS                                       ###
################################################################################

source("panels/ui/home_ui.R", local = TRUE)
source("panels/ui/genomeFilters_ui.R", local = TRUE)
source("panels/ui/graphic_ui.R", local = TRUE)
source("panels/ui/about_ui.R", local = TRUE)
source("panels/ui/tableResults_ui.R", local = TRUE)
source("panels/ui/resume_ui.R", local = TRUE)
source("panels/ui/annotationTabItem_ui.R", local = TRUE)

################################################################################
###                              MAIN                                        ###
################################################################################

shinyUI(
    
    dashboardPagePlus(
        title = "PIPprofileR",
        header = dashboardHeaderPlus(title = "PIPprofileR",
                                     left_menu = tagList(
                                         actionButton("dataButton", 
                                                      "Select your data", 
                                                      icon = icon("upload"),
                                                      width = 200
                                         ),
                                         actionButton("annotButton", 
                                                      "Import annotation", 
                                                      icon = icon("upload"),
                                                      width = 200
                                         ),
                                         
                                         uiOutput('RunButton'), 

                                         downloadButton("downloadData", label = "Download", style = "width:200px;margin: 0px; padding : 6px 12px !important; display: block; color: #444 !important;")
                                     ),
                                     dropdownMenu(icon = icon("question-circle"),badgeStatus =NULL,headerText = "Global information",
                                                  messageItem(
                                                      from = "Find our project?",
                                                      message = "Visit our Github!",
                                                      icon = icon("github", class = "fa"),
                                                      href = "https://github.com/IFB-ElixirFr/PIPprofileR/"
                                                  ),
                                                  messageItem(
                                                      from = "New User?",
                                                      message = "Read the docs!",
                                                      icon = icon("book"),
                                                      href = "https://github.com/IFB-ElixirFr/PIPprofileR/wiki"
                                                  ),
                                                  messageItem(
                                                      from = "A bug with app?",
                                                      message = "Declare an issue!",
                                                      icon = icon("exclamation-triangle"),
                                                      href = "hhttps://github.com/IFB-ElixirFr/PIPprofileR/issues"
                                                  ))
                                     
        ),
        sidebar = dashboardSidebar( uiOutput('sidebar') ),
        body = dashboardBody(
            tags$head(tags$link(href = "img/logo.png",
                                rel ="icon", type="image/png")),
            tags$head(HTML('<link rel="stylesheet" type="text/css"
                   href="css/style.css" />')), 
            useShinyjs(),
            useShinyalert(),
            tabItems(
                tabItem("home", home),
                tabItem("genomeFilters", genomeFilters),
                tabItem("resume",resume), 
                tabItem("tableResults",tableResults),
                tabItem("graphic",graphic), 
                # tabItem("tree",treeTabItem), 
                # tabItem("heatmap",heatmapTabItem), 
                tabItem("annotationTabItem", annotationTabItem), 
                tabItem("about", about)
            )
        )
    )
)
