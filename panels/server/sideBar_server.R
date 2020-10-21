observeEvent(rvEnvent$load, {
  if(rvEnvent$load){
    output$sidebar <- renderUI({
      sidebarMenu( id = "tabs",
                   menuItem("Home", tabName = "home", icon = icon("file-import")),
                   if( input$dataset !="rdata"){
                     menuItem("Filters",icon = icon("filter"), 
                              menuSubItem("Genomes", tabName = "genomeFilters"),
                              menuSubItem("Genes", tabName = "annotFilters"))
                   },
                   menuItem("Summary", tabName = "resume", icon = icon("file")),
                   menuItem("Graphic", tabName = "graphic", icon = icon("chart-area")),
                   menuItem("Score", tabName = "tableResults", icon = icon("table")),
                   menuItem("Tree", tabName = "tree", icon = icon("tree")),
                   menuItem("Heatmap", tabName = "heatmap", icon = icon("chess-board")),
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