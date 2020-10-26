################################################################################
# Title : PIPprofileR - SERVER
# Organism : All 
# Omics area : Omics
# Users : Thomas Denecker
# Email : thomas.denecker@gmail.com
# Date : Oct, 2020
# GitHub : 
# DockerHub : 
################################################################################

################################################################################
###                                Library                                   ###
################################################################################

library(shiny)

################################################################################
###                              MAIN                                        ###
################################################################################

shinyServer(function(input, output, session) {
    
    options(shiny.maxRequestSize=100*1024^2)
    
    #===========================================================================
    # PANELS & FUNCTIONS                                 
    #===========================================================================
    
    source("panels/dataInput.R", local = TRUE)
    source("panels/annotInput.R", local = TRUE)
    source("panels/server/sideBar_server.R", local = TRUE)
    source("panels/server/sequenceFilters_server.R", local = TRUE)
    source("panels/server/graphic_server.R", local = TRUE)
    source("panels/server/tableResults_server.R", local = TRUE)
    source("panels/server/about_server.R", local = TRUE)
    source("panels/server/home_server.R", local = TRUE)
    source("panels/server/resume_server.R", local = TRUE)
    source("panels/server/annotationTabItem_server.R", local = TRUE)
    source("panels/server/alignmentTabItem_server.R", local = TRUE)
    
    #===========================================================================
    # Session
    #===========================================================================
    
    si <- sessionInfo()

    observe_helpers(session, help_dir = "helpfiles/")
    
    #===========================================================================
    # Reactive Values
    #===========================================================================
    
    genomes <-reactiveValues()
    rvAnnotation <- reactiveValues()
    rvEnvent <- reactiveValues()
    features <- reactiveValues()
    species <- reactiveValues()
    plotlyRV <- reactiveValues()
    
    rvEnvent$load = F
    
    tmpFolderRV <- reactiveValues()
    
    #===========================================================================
    # Dir creation
    #===========================================================================
    
    if(! dir.exists("www/tmp")){
        dir.create("www/tmp")
    }
    
    nameTmpFolder = format(Sys.time(), "%Y%m%d_%H%M%S")
    
    tmpFolder = paste0("www/tmp/",nameTmpFolder)
    tmpFolderWithoutWWW = paste0("tmp/",nameTmpFolder)
    
    dir.create(paste0("www/tmp/", nameTmpFolder))
    dir.create(paste0("www/tmp/", nameTmpFolder, "/BrowseSeqs"))
    
    #===========================================================================
    # Modal part - Import data & annotation
    #===========================================================================
    
    showModal(dataInput())
    
    observeEvent(input$dataButton, {
        showModal(dataInput())
    })
    
    observeEvent(input$annotButton, {
        showModal(annotInput())
    })
    
    #===========================================================================
    # Color
    #===========================================================================
    ## Color palette per species
    species$Palette <- list(
        Human = "#880000",
        Bat = "#888888",
        Pangolin = "#448800",
        Camel = "#BB8800",
        Pig = "#FFBBBB",
        Civet = "#00BBFF",
        Other = "grey"
    )
    
    ## Species prefix in the tip labels
    species$Prefix <- c("Hu" = "Human",
                        "Bt" = "Bat",
                        "Pn" = "Pangolin",
                        "Cm" = "Camel",
                        "Pi" = "Pig",
                        "Cv" = "Civet")
    
    #===========================================================================
    # READ FASTA
    #===========================================================================
    
    observeEvent(input$file,{
        genomes$Sequences <- readDNAStringSet(filepath = input$file$datapath, 
                                              format = "fasta")
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
    })
    
    observeEvent(genomes$genomeNames, {
        updateSelectizeInput(session, "refPattern", 
                             choices =  setNames(genomes$genomeNames, 
                                                 genomes$genomeNames),
                             selected = genomes$genomeNames[1])
    })
    
    #===========================================================================
    # Run analysis
    #===========================================================================
    
    observeEvent(input$Run, {
        
        switch(
            input$dataset,
            "demo" =
                {
                    message <- as.character(input$demo)
                },
            "input" =
                {
                    genomes$strainColors <- (unlist(genomes$genomeStat$color))
                    names(genomes$strainColors) <- row.names(genomes$genomeStat)
                    
                    ## Define output file for genome alignments
                    dir.create(paste0(tmpFolder, "/Nto1_alignments"))
                    
                    OF <- file.path(
                        paste0(tmpFolder, "/Nto1_alignments"), 
                        paste0("genome_alignments_ref_", genomes$refGenomeName))
                    
                    ## Get sequences for reference and query genomes
                    refGenome <- genomes$Sequences[genomes$refGenomeName ]
                    queryGenomes <- genomes$Sequences[genomes$queryGenomeNames]
                    
                    message("queryGenomes :", length(queryGenomes))
                    
                    genomes$genomesNto1 <- alignNtoOne(
                        refSequence = refGenome, 
                        querySequences = queryGenomes,
                        outfile = OF)
                    plotlyRV <- NULL
                },
            "rdata" =
                {
                    message <- as.character(input$fileRData$name)
                    # if (!is.null(input$fileRData)){
                    #     load(input$fileRData$datapath)
                    #     genomes$genomesNto1 <- Nto1_list
                    #     rm(Nto1_list)
                    # }
                }
        )
        
        updateTabItems(session, "tabs", selected = "graphic")
    })
    
    #===========================================================================
    # Download
    #===========================================================================
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0(nameTmpFolder, ".zip")
        },
        content = function(fname) {
            
            #===================================================================
            # Data preparation 
            #===================================================================
            Nto1_list <- genomes$genomesNto1
            Nto1_list$plot <- plotlyRV
            save(Nto1_list, file = file.path(tmpFolder,'genomesNto1.Rdata'))
            rm(Nto1_list)
            
            tmpdir <- tmpFolder
            oldDir <- getwd()
            setwd(tmpFolder)
            fs <- list.files()
            zip(zipfile= fname, files=fs)
            setwd(oldDir)
        },
        contentType = "application/zip"
    )

    #===========================================================================
    # Clean temp
    #===========================================================================
    
    session$onSessionEnded(function(userID = users_data$USERS) {
        unlink(tmpFolder, recursive = T)
    })

})
