output$RunButton <- renderUI({
  if( !is.null(input$dataset) && input$dataset !="rdata"){
    actionButton("Run", 
                 "Run", 
                 icon = icon("play"),
                 width = 200
    ) 
  } else {
    NULL
  }
})