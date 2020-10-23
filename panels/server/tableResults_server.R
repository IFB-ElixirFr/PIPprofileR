output$pipResults <-  renderDataTable({
  if(!is.null(rvAnnotation$annotation)){
    
  } else {
    NULL
  }
}, selection = 'none', filter ="top", options = list(scrollX = TRUE))