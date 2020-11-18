observeEvent({
  rvEnvent$load
  rvEnvent$loadAnnot
  } , {
  if(rvEnvent$load | rvEnvent$loadAnnot){
    output$sidebar <- renderUI({
      sidebarMenu( id = "tabs",
                   menuItem("Home", tabName = "home", icon = icon("home")),
                   p(style = "text-align: center;margin: 0px; font-weight: bold; background-color: #3c8dbc; padding: 5px;", 'Imports'),
                   actionButton("dataButton",
                                "Import sequences",
                                icon = icon("upload"),
                                width = 200
                   ),

                   actionButton("annotButton",
                                "Import annotation",
                                icon = icon("upload"),
                                width = 200
                   ),

                   if(!is.null(rvAnnotation$annotation) | rvEnvent$load && (input$dataset =="input" | (input$dataset == "demo" & input$demoType == "fasta" ))){
                     p(style = "text-align: center;margin: 0px; font-weight: bold; background-color: #3c8dbc;padding: 5px;", 'Settings')
                   },

                   if( !is.null(rvAnnotation$annotation)){
                     menuItem("Annotations", tabName ="annotationTabItem", icon = icon("pen"))
                   },

                   if( rvEnvent$load && (input$dataset =="input" | (input$dataset == "demo" & input$demoType == "fasta" ))){
                     menuItem("Filters",tabName = "sequenceFilters", icon = icon("filter"))
                   },

                   if(rvEnvent$load){
                     tagList(p(style = "text-align: center;margin: 0px; font-weight: bold; background-color: #3c8dbc;padding: 5px;", 'Results'),
                     menuItem("Sequences information", tabName = "resume", icon = icon("file")),
                     menuItem("PIP profile", tabName = "graphic", icon = icon("chart-area")),
                     menuItem("Alignment", tabName = "alignmentTabItem", icon = icon("equals")),
                     downloadButton("downloadData", label = "Download", style = "width:200px;color: #444 !important; margin : 15px"))
                   },

                   p(style = "text-align: center;margin: 0px; font-weight: bold; background-color: #3c8dbc;padding: 5px;", 'More'),
                   menuItem("Session", tabName = "about", icon = icon("cubes")),
                   menuItem("Help", tabName = "helpPage", icon = icon("question-circle"))
      )
    })

  } else {
    output$sidebar <- renderUI({
      sidebarMenu(id = "tabs",
                  menuItem("Home", tabName = "home", icon = icon("home")),
                  p(style = "text-align: center;margin: 0px; font-weight: bold; background-color: #3c8dbc;padding: 5px;", 'Imports'),
                  actionButton("dataButton",
                               "Import sequences",
                               icon = icon("upload"),
                               width = 200
                  ),
                  actionButton("annotButton",
                               "Import annotation",
                               icon = icon("upload"),
                               width = 200
                  ),
                  p(style = "text-align: center;margin: 0px; font-weight: bold; background-color: #3c8dbc;padding: 5px;", 'More'),
                  menuItem("About", tabName = "about", icon = icon("cubes")),
                  menuItem("Help", tabName = "helpPage", icon = icon("question-circle"))
      )
    })
    updateTabItems(session, "tabs", selected = "home")
    shinyjs::runjs("window.scrollTo(0, 0)")
  }

})
