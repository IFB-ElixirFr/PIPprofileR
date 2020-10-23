output$resumeQuery <-  renderDataTable({
  if(!is.null(genomes$genomesNto1)){
    
    do.call("rbind", lapply(genomes$genomesNto1$sequences, function(s){
      c(Size = nchar(s), (table(unlist(strsplit(as.character(s),"")))/nchar(s)) * 100)
    }))
    
  } else {
    NULL
  }
}, selection = 'none', options = list(scrollX = TRUE))


output$refName_ui <- renderUI(
  HTML(paste0("<p><b>Name </b>: ",names(genomes$genomesNto1$reference),"</p>"))
)

output$refSize_ui <- renderUI(
  HTML(paste0("<p><b>Size </b>: ",nchar(genomes$genomesNto1$reference),"</p>"))
)



