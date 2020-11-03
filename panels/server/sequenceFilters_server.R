################################################################################
# 1- Select reference sequence 
################################################################################

observeEvent(input$refPattern, {
  if(! is.null(input$refPattern) & input$refPattern != ""){
    genomes$refGenomeName <- input$refPattern
    genomes$queryGenomeNamesAll <- genomes$genomeNames[- which(genomes$genomeNames == genomes$refGenomeName)]
    genomes$queryGenomeNames <- genomes$genomeNames[- which(genomes$genomeNames == genomes$refGenomeName)]
    genomes$nbQueryGenomes <- length(genomes$queryGenomeNames)
    
    updateMultiInput(
      session = session,
      inputId = "multiQuery",
      choices = genomes$queryGenomeNamesAll, 
      selected = genomes$queryGenomeNamesAll
    )
    
    
    # Proposal of a window size according to the size of the sequences 
    # 
    # Purpose: a function that returns ~150 when the sequences are 3kb, and ~600 when they are 30kb. 
    # Proposal : a linear function. 
    # With w, the width of the window and L, the length of the sequences :
    #   
    #   w = a * L + b
    # the slope a = (600 - 150) / (30000 - 3000) = 1 / 60
    # the intercept b = 150 - a * 3000 = 150 - 50 = 100
    # w = 1/60 * L + 100
    
    calculatedWindowSize = nchar(genomes$Sequences[[genomes$refGenomeName]]) / 60 + 100
    
    updateNumericInput(session,
                       "windowSize_param", 
                       value = round(calculatedWindowSize)
    )
    
    updateNumericInput(session,
                       "windowSize", 
                       value = round(calculatedWindowSize)
    )
  }
})


################################################################################
# 2- Select query sequences
################################################################################

output$multiInput_query <- renderUI({
  if(! is.null(input$refPattern) & input$refPattern != ""){
    multiInput(
      inputId = "multiQuery",
      label = NULL,
      choices = genomes$queryGenomeNamesAll,
      selected = genomes$queryGenomeNamesAll,
      width = "100%"
    )
  }
  
})

observeEvent(input$clear, {
  updateMultiInput(
    session = session,
    inputId = "multiQuery",
    choices = genomes$queryGenomeNamesAll, 
    selected = NULL
  )
})

observeEvent(input$all, {
  updateMultiInput(
    session = session,
    inputId = "multiQuery",
    choices = genomes$queryGenomeNamesAll, 
    selected = genomes$genomeNames[- which(genomes$genomeNames == genomes$refGenomeName)]
  )
})

observeEvent(input$multiQuery, {
  
  genomes$queryGenomeNames <- input$multiQuery
  genomes$nbQueryGenomes <- length(genomes$queryGenomeNames)
  genomes$nbGenomes <- length(genomes$queryGenomeNames) + 1 # +1 for ref
  
  genomes$genomeStat <- data.frame(
    row.names = c(genomes$refGenomeName, genomes$queryGenomeNames),
    status = c("Reference", rep("Query", length.out = length(genomes$queryGenomeNames)))
  )
  
  g <- 1
  for (g in c(genomes$refGenomeName, genomes$queryGenomeNames)) {
    genomes$genomeStat[g, "length"] <- length(genomes$Sequences[[g]])
  }
  
  genomes$genomeStat[, "n"] <- 1:as.numeric(genomes$nbGenomes)
  genomes$genomeStat[, "species"] <- rep( "Other", as.numeric(genomes$nbGenomes))
  
  for (prefix in names(species$Prefix)) {
    genomes$genomeStat[grep(pattern = paste0("^", prefix), x = row.names(genomes$genomeStat), perl = TRUE), "species"] <- species$Prefix[prefix]
  }
  
  ## Assign a color to each species
  genomes$genomeStat$color <- species$Palette[as.vector(genomes$genomeStat$species)]
  
})

################################################################################
# Summary
################################################################################

output$refQueryTable <-  renderDataTable({
  if(!is.null(genomes$genomeStat)){
    write.table(apply(genomes$genomeStat,2,as.character), file.path(tmpFolder, 'genomeStat.tsv'), sep = "\t", quote = F)
    genomes$genomeStat
  } else {
    NULL
  }
}, selection = 'none', options = list(scrollX = TRUE))

################################################################################
# RUN
################################################################################

