observeEvent(rvEnvent$load, {
  if(rvEnvent$load){
    output$sidebar <- renderUI({
      sidebarMenu( id = "tabs",
                   menuItem("Import", tabName = "home", icon = icon("file-import")),
                   if( input$dataset =="input"){
                     menuItem("Filters",tabName = "sequenceFilters", icon = icon("filter"))
                     },
                   menuItem("Summary", tabName = "resume", icon = icon("file")),
                   menuItem("PIP profile", tabName = "graphic", icon = icon("chart-area")),
                   menuItem("Alignment", tabName = "alignmentTabItem", icon = icon("equals")),
                   if( !is.null(rvAnnotation$annotation)){
                     menuItem("Annotations", tabName ="annotationTabItem", icon = icon("pen"))
                   },
                   downloadButton("downloadData", label = "Download", style = "width:200px;color: #444 !important; margin : 15px"),
                   tags$hr(style=" margin : 0px 15px"), 
                   menuItem("Session", tabName = "about", icon = icon("cubes")), 
                   menuItem("Help", tabName = "helpPage", icon = icon("question-circle")) 
      )
    })
    
  } else {
    output$sidebar <- renderUI({
      sidebarMenu(id = "tabs",
                  menuItem("Import", tabName = "home", icon = icon("file-import")),
                  menuItem("About", tabName = "about", icon = icon("cubes")), 
                  menuItem("Help", tabName = "helpPage", icon = icon("question-circle"))
      )
    })
    updateTabItems(session, "tabs", selected = "home")
    shinyjs::runjs("window.scrollTo(0, 0)")
  }
  
})