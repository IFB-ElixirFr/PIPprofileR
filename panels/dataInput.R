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


################################################################################
# Render UI
################################################################################
output$dataUI <- renderUI({
  if (is.null(input$dataset))
    return()
  
  switch(
    input$dataset,
    "demo" = enable("okData"),
    "input" = disable("okData"),
    "rdata" = disable("okData")
  )
  
  switch(
    input$dataset,
    "demo" = tags$div(
      selectInput(
        inputId = "demo",
        label = "Select a demo dataset",
        choices = c("Spike proteins around SARS-CoV-2" = "spike_CoV-2", 
                    "Genomes around SARS-CoV-2" = "genome_CoV-2")
      ), 
      selectInput(
        inputId = "demoType",
        label = "From",
        choices = c("Rdata file" = "rdata", 
                    "Fasta file" = "fasta")
      )
    ),
    "input" = tags$div(
      
      h4("Select file"),
      fileInput("file",label = NULL,
                buttonLabel = "Browse...",
                placeholder = "No file selected",
                accept = c(".fasta", ".fa")),
      
      h4("The nature of the sequence"),
      radioButtons("seqType", NULL,inline = T, 
                   c("DNA" = "DNA",
                     "Protein" = "AA"),
                   selected = "DNA"), 
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

################################################################################
# READ FASTA
################################################################################

observeEvent({
  input$file
  input$seqType
},{
  
  if(!is.null(input$file)){
    if(input$seqType == "DNA"){
      genomes$Sequences <- readDNAStringSet(filepath = input$file$datapath, 
                                            format = "fasta")
    } else {
      genomes$Sequences <- readAAStringSet(filepath = input$file$datapath, 
                                           format = "fasta")
    }
    
    genomes$oldNames <- as.data.frame(genomes$Sequences@ranges)$names
    names(genomes$Sequences) <- sub(pattern = " .*", 
                                    replacement = "",
                                    x = names(genomes$Sequences), 
                                    perl = TRUE)
    genomes$genomeNames <- names(genomes$Sequences)
    genomes$nbGenomes <- length(genomes$genomeNames)
    genomes$genomeSizes <- as.data.frame(genomes$Sequences@ranges)$width
    message("Loaded ", genomes$nbGenomes, " genomes from file ", 
            input$file$name)
    enable("okData")
  }
  
})

observeEvent(genomes$genomeNames, {
  updateSelectizeInput(session, "refPattern", 
                       choices =  setNames(genomes$genomeNames, 
                                           genomes$genomeNames),
                       selected = genomes$genomeNames[1])
})

observeEvent({
  input$fileRData
},{
  
  if(!is.null(input$fileRData)){
    enable("okData")
  }
  
})

################################################################################
# READ sequence
################################################################################

observeEvent(input$okData, {
  switch(
    input$dataset,
    "demo" =
      {
        message <- as.character(input$demo)
        
        if(input$demoType == "fasta"){
          #=======================================================================
          # Demo FASTA
          #=======================================================================

          if(input$demo == "genome_CoV-2"){
            genomes$Sequences <- readDNAStringSet(filepath = "dataExample/genomes_around-CoV-2.fasta", 
                                                  format = "fasta")
            updateRadioButtons(session, "seqType",selected = "DNA")
            genomes$genomesNto1$seqType = "DNA"
          } else if(input$demo == "spike_CoV-2" ) {
            genomes$Sequences <- readAAStringSet(filepath = "dataExample/spike_proteins_around-CoV-2.fasta", 
                                                 format = "fasta")
            updateRadioButtons(session, "seqType",selected = "AA")
            genomes$genomesNto1$seqType = "AA"
          }
          
          genomes$oldNames <- as.data.frame(genomes$Sequences@ranges)$names
          names(genomes$Sequences) <- sub(pattern = " .*", 
                                          replacement = "",
                                          x = names(genomes$Sequences), 
                                          perl = TRUE)
          genomes$genomeNames <- names(genomes$Sequences)
          genomes$nbGenomes <- length(genomes$genomeNames)
          genomes$genomeSizes <- as.data.frame(genomes$Sequences@ranges)$width
          message("Loaded ", genomes$nbGenomes, " genomes from file ", 
                  input$file$name)
          
          updateTabItems(session, "tabs", selected = "sequenceFilters")
          
        } else {
          #=====================================================================
          # Demo Rdata
          #=====================================================================
          if( input$demo == "spike_CoV-2" ){
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
          
          plotlyRV$refPositions = Nto1_list$plot$refPositions
          plotlyRV$xlim =  Nto1_list$plot$xlim
          plotlyRV$ylim =  Nto1_list$plot$ylim
          
          plotlyRV$colMinorX = Nto1_list$plot$colMinorX
          plotlyRV$sizeMinorX = Nto1_list$plot$sizeMinorX 
          plotlyRV$colMajorX = Nto1_list$plot$colMajorX
          plotlyRV$sizeMajorX = Nto1_list$plot$sizeMajorX 
          plotlyRV$colMinorY = Nto1_list$plot$colMinorY
          plotlyRV$sizeMinorY = Nto1_list$plot$sizeMinorY 
          plotlyRV$colMajorY = Nto1_list$plot$colMajorY
          plotlyRV$sizeMajorY = Nto1_list$plot$sizeMajorY
          
          plotlyRV$spaceMajorX = Nto1_list$plot$spaceMajorX 
          plotlyRV$spaceMinorX = Nto1_list$plot$spaceMinorX 
          plotlyRV$spaceMajorY = Nto1_list$plot$spaceMajorY 
          plotlyRV$spaceMinorY = Nto1_list$plot$spaceMinorY
          
          plotlyRV$windowSize = Nto1_list$plot$windowSize
          
          rm(Nto1_list)
          updateTabItems(session, "tabs", selected = "resume")
        }
        
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
          
          plotlyRV$refPositions = Nto1_list$plot$refPositions
          plotlyRV$xlim =  Nto1_list$plot$xlim
          plotlyRV$ylim =  Nto1_list$plot$ylim
          
          plotlyRV$colMinorX = Nto1_list$plot$colMinorX
          plotlyRV$sizeMinorX = Nto1_list$plot$sizeMinorX 
          plotlyRV$colMajorX = Nto1_list$plot$colMajorX
          plotlyRV$sizeMajorX = Nto1_list$plot$sizeMajorX 
          plotlyRV$colMinorY = Nto1_list$plot$colMinorY
          plotlyRV$sizeMinorY = Nto1_list$plot$sizeMinorY 
          plotlyRV$colMajorY = Nto1_list$plot$colMajorY
          plotlyRV$sizeMajorY = Nto1_list$plot$sizeMajorY
          
          plotlyRV$spaceMajorX = Nto1_list$plot$spaceMajorX 
          plotlyRV$spaceMinorX = Nto1_list$plot$spaceMinorX 
          plotlyRV$spaceMajorY = Nto1_list$plot$spaceMajorY 
          plotlyRV$spaceMinorY = Nto1_list$plot$spaceMinorY
          
          plotlyRV$windowSize = Nto1_list$plot$windowSize
          
          rm(Nto1_list)
          updateTabItems(session, "tabs", selected = "resume")
        }
      }
  )
  
  removeModal()
  rvEnvent$load <- T
  message(paste("[PIPprofileR] Correct upload with", input$dataset, "mode :", message))
  
})