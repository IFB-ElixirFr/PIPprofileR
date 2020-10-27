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
    Make sure that the object in the RData file is called `Nto1_list`."),
    
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
      choices = c("Spike proteins around SARS-CoV-2" = "spike_CoV-2", 
                  "Genomes around SARS-CoV-2" = "genome_CoV-2")
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
        if( input$demo == "spike_CoV-2"  ){
          load("dataExample/spike_proteins_around-CoV-2.Rdata")
        } else if (input$demo == "genome_CoV-2"){
          load("dataExample/genomes_around-CoV-2.Rdata")
        }
        
        genomes$genomesNto1 <- Nto1_list
        
        plotlyRV$p = Nto1_list$plot$p
        plotlyRV$plotGG = Nto1_list$plot$plotGG
        plotlyRV$colors = Nto1_list$plot$colors
        
        plotlyRV$title_main = Nto1_list$plot$title_main
        plotlyRV$title_x = Nto1_list$plot$title_x
        plotlyRV$title_y = Nto1_list$plot$title_y
        plotlyRV$title_legende = Nto1_list$plot$title_legende
        
        plotlyRV$xlim =  Nto1_list$plot$xlim
        plotlyRV$ylim =  Nto1_list$plot$ylim
        
        plotlyRV$colMinorH = Nto1_list$plot$colMinorH 
        plotlyRV$sizeMinorH = Nto1_list$plot$sizeMinorH 
        plotlyRV$colMajorH = Nto1_list$plot$colMajorH 
        plotlyRV$sizeMajorH = Nto1_list$plot$sizeMajorH 
        plotlyRV$colMinorV = Nto1_list$plot$colMinorV 
        plotlyRV$sizeMinorV = Nto1_list$plot$sizeMinorV 
        plotlyRV$colMajorV = Nto1_list$plot$colMajorV 
        plotlyRV$sizeMajorV = Nto1_list$plot$sizeMajorV
        
        plotlyRV$spaceMajorH = Nto1_list$plot$spaceMajorH 
        plotlyRV$spaceMinorH = Nto1_list$plot$spaceMinorH 
        plotlyRV$spaceMajorV = Nto1_list$plot$spaceMajorV 
        plotlyRV$spaceMinorV = Nto1_list$plot$spaceMinorV
        
        rm(Nto1_list)
        updateTabItems(session, "tabs", selected = "resume")
      },
    "input" =
      {
        message <- as.character(input$fileBiom$name)
        updateTabItems(session, "tabs", selected = "sequenceFilters")
      },
    "rdata" =
      {
        message <- as.character(input$fileRData$name)
        if (!is.null(input$fileRData)){
          load(input$fileRData$datapath)
          genomes$genomesNto1 <- Nto1_list
          
          plotlyRV$p = Nto1_list$plot$p
          plotlyRV$plotGG = Nto1_list$plot$plotGG
          plotlyRV$colors = Nto1_list$plot$colors
          
          plotlyRV$title_main = Nto1_list$plot$title_main
          plotlyRV$title_x = Nto1_list$plot$title_x
          plotlyRV$title_y = Nto1_list$plot$title_y
          plotlyRV$title_legende = Nto1_list$plot$title_legende
          
          plotlyRV$xlim =  Nto1_list$plot$xlim
          plotlyRV$ylim =  Nto1_list$plot$ylim
          
          plotlyRV$colMinorH = Nto1_list$plot$colMinorH 
          plotlyRV$sizeMinorH = Nto1_list$plot$sizeMinorH 
          plotlyRV$colMajorH = Nto1_list$plot$colMajorH 
          plotlyRV$sizeMajorH = Nto1_list$plot$sizeMajorH 
          plotlyRV$colMinorV = Nto1_list$plot$colMinorV 
          plotlyRV$sizeMinorV = Nto1_list$plot$sizeMinorV 
          plotlyRV$colMajorV = Nto1_list$plot$colMajorV 
          plotlyRV$sizeMajorV = Nto1_list$plot$sizeMajorV
          
          plotlyRV$spaceMajorH = Nto1_list$plot$spaceMajorH 
          plotlyRV$spaceMinorH = Nto1_list$plot$spaceMinorH 
          plotlyRV$spaceMajorV = Nto1_list$plot$spaceMajorV 
          plotlyRV$spaceMinorV = Nto1_list$plot$spaceMinorV
          
          rm(Nto1_list)
          updateTabItems(session, "tabs", selected = "resume")
        }
      }
  )
  
  removeModal()
  rvEnvent$load <- T
  message(paste("[PIPprofileR] Correct upload with", input$dataset, "mode :", message))
  
})