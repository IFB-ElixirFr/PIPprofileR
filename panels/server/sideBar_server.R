observeEvent(rvEnvent$load, {
  if(rvEnvent$load){
    output$sidebar <- renderUI({
      sidebarMenu( id = "tabs",
                   menuItem("Home", tabName = "home", icon = icon("file-import")),
                   if( input$dataset =="input"){
                     menuItem("Filters",tabName = "sequenceFilters", icon = icon("filter"))
                     },
                   menuItem("Summary", tabName = "resume", icon = icon("file")),
                   menuItem("PIP profile", tabName = "graphic", icon = icon("chart-area")),
                   menuItem("Score", tabName = "tableResults", icon = icon("table")),
                   menuItem("Alignment", tabName = "alignmentTabItem", icon = icon("equals")),
                   if( !is.null(rvAnnotation$annotation)){
                     menuItem("Annotations", tabName ="annotationTabItem", icon = icon("pen"))
                   },
                   menuItem("About", tabName = "about", icon = icon("cubes"))
      )
    })
    
  } else {
    output$sidebar <- renderUI({
      sidebarMenu(id = "tabs",
                  menuItem("Home", tabName = "home", icon = icon("file-import")),
                  menuItem("About", tabName = "about", icon = icon("cubes"))
      )
    })
    updateTabItems(session, "tabs", selected = "home")
    shinyjs::runjs("window.scrollTo(0, 0)")
  }
  
})