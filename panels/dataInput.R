dataInput <- function(failed = FALSE) {
  modalDialog(
    title = "Select your data",
    
    if (failed)
    {
      div(
        "Invalide dataset. \n Please try again \n", 
        style = "font-weight: bold; color: red; text-align: center; white-space: pre-line"
      )
    }, 
    
    helpText("Firstly, you should select a demo dataset or upload a FASTA file.
    Make sure that the phyloseq object in the RData file is called `data`."),
    
    radioButtons(
      inputId = "dataset",
      label = "Select dataset : ",
      inline = TRUE,
      choices = list(
        "Demo" = "demo",
        "Input FASTA" = "input",
        "RData" = "rdata"
      ),
      selected = "demo"
    ),
    
    wellPanel(uiOutput("dataUI")),
    
    footer = tagList(modalButton("Cancel"),
                     actionButton(inputId = "okData", label = "OK"))
  )
}

output$dataUI <- renderUI({
  if (is.null(input$dataset))
    return()
  
  switch(
    input$dataset,
    "demo" = selectInput(
      inputId = "demo",
      label = "Select a demo dataset",
      choices = c("Sallar et al., 2020" = "Sallar")
    ),
    "input" = tags$div(
      
      h4("Select file"),
      fileInput("file",label = NULL,
                buttonLabel = "Browse...",
                placeholder = "No file selected",
                accept = c(".fasta", ".fa")),
      
      h4("Summary"),
      DTOutput('previewFasta')

    ),
    "rdata" = fileInput(
      inputId = "fileRData",
      label = "RData where 'Nto1_list' is a PIPprofileR object : ",
      placeholder = "data.RData", 
      accept = ".Rdata"
    )
  )
})

output$previewFasta = renderDataTable({
  if(!is.null(genomes$Sequences)){
    inter <- cbind.data.frame(
      genomes$oldNames, 
      genomes$genomeNames, 
      genomes$genomeSizes
    )
    colnames(inter) <- c("Original names", "Automatic names", "Sequence width")
    inter
  } else {
    NULL
  }
}
, 
selection = 'none', escape = FALSE,
options = list(pageLength = 5, scrollX = TRUE)
)


observeEvent(input$okData, {
  switch(
    input$dataset,
    "demo" =
      {
        message <- as.character(input$demo)
        load("dataExample/article.Rdata")
        genomes$genomesNto1 <- Nto1_list
        rm(Nto1_list)
        updateTabItems(session, "tabs", selected = "resume")
      },
    "input" =
      {
        message <- as.character(input$fileBiom$name)
        updateTabItems(session, "tabs", selected = "genomeFilters")
      },
    "rdata" =
      {
        message <- as.character(input$fileRData$name)
        if (!is.null(input$fileRData)){
          load(input$fileRData$datapath)
          genomes$genomesNto1 <- Nto1_list
          plotlyRV = Nto1_list$plot
          rm(Nto1_list)
          updateTabItems(session, "tabs", selected = "resume")
        }
      }
  )
  
  removeModal()
  rvEnvent$load <- T
  message(paste("[PIPprofileR] Correct upload with", input$dataset, "mode :", message))
  
})