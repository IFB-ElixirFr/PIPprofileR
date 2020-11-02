################################################################################
# Function
################################################################################

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

################################################################################
# Render UI
################################################################################

output$plotArea <- renderUI(
    withLoader(plotOutput("plotGGPLOT", height = "500px",
                          hover = hoverOpts(id ="plot_hover", delay=100),
                          click = clickOpts(id ="plot_click"),
                          brush = brushOpts(id ="plot_brush", delay=100)))
)

output$plotArea_title <- renderUI(
  if(!is.null(genomes$refGenomeName)){
    div(
      h1("PIP profile"),
      h2(paste0("Ref: ", genomes$refGenomeName))
    )
  } else {
    h1("PIP profile")
  }
  
)

output$resumeGene_general <- renderUI({ 
  if(input$geneExplore_selector != "All"){
    
    inter = rvAnnotation$feature  %>% filter(featureName == input$geneExplore_selector)
    para = NULL
    for(i in 1:nrow(inter)){
      para = c(para, as.character(p(tags$b(tags$u(paste("Occurence ", i ))),tags$br(),
                                    tags$b("Sequence ID: "), inter$seqid[i],tags$br(),
                                    tags$b("Source: "),inter$source[i],tags$br(),
                                    tags$b("Start: "),inter$start[i],tags$br(),
                                    tags$b("End: "),inter$end[i],tags$br(),
                                    tags$b("Score: "),inter$end[i],tags$br(),
                                    tags$b("Strand: "),inter$strand[i],tags$br(),
                                    tags$b("phase: "),inter$phase[i])))
      
    }
    para = paste0(para, collapse = "")
    
    div(
      h4("General information"), 
      HTML(para)
    )
  } else {
    div(
      h4("General information")
    )
  }
  
})

output$resumeGene_attributes <- renderUI({
  
  if(input$geneExplore_selector != "All"){
    
    inter = rvAnnotation$feature  %>% filter(featureName == input$geneExplore_selector) %>% pull(attrtibuteHTML)
    para = NULL
    for(i in 1:length(inter)){
      para = c(para, as.character(p(tags$b(tags$u(paste("Occurence ", i ))),tags$br(),
                                    HTML(inter[i]))))
      
    }
    para = paste0(para, collapse = "")
    
    div(
      h4("Attributes"), 
      HTML(para)
    )
  } else {
    div(
      h4("Attributes")
    )
  }
  
})

################################################################################
# Generate plot
################################################################################

observe({
  if(!is.null(plotlyRV$p)){
    plotlyRV$plotGG <- ggplot(plotlyRV$p, aes(x = x, y = y, colour = name)) + 
      geom_line() + 
      ggtitle(plotlyRV$title_main) + 
      scale_color_manual(values= plotlyRV$colors) + 
      theme(panel.background = element_rect(fill = "white", colour = "white"),
            panel.grid.minor.y = element_line(colour=plotlyRV$colMinorY, size=plotlyRV$sizeMinorY) , 
            panel.grid.major.y = element_line(colour=plotlyRV$colMajorY, size=plotlyRV$sizeMajorY), 
            panel.grid.minor.x = element_line(colour=plotlyRV$colMinorX, size=plotlyRV$sizeMinorX) , 
            panel.grid.major.x = element_line(colour=plotlyRV$colMajorX, size=plotlyRV$sizeMajorX),
            legend.key=element_blank(), 
            plot.title = element_text(size = 16, face = "bold"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"), 
            legend.title = element_text(size=14,face="bold"),
            legend.text = element_text(size=12)) +
      scale_x_continuous( name= plotlyRV$title_x , limits=plotlyRV$xlim ,
                          breaks = seq(0, length(plotlyRV$refPositions), plotlyRV$spaceMajorX), 
                          minor_breaks = seq(0, length(plotlyRV$refPositions), plotlyRV$spaceMinorX))  +
      scale_y_continuous( name=plotlyRV$title_y, 
                          limits=plotlyRV$ylim , 
                          breaks = seq(0, 100, plotlyRV$spaceMajorY),
                          minor_breaks = seq(0, 100, plotlyRV$spaceMinorY)) + 
      labs(color=plotlyRV$title_legende) +
      guides(color = guide_legend(override.aes = list(size = 2) ) )
  }
})


output$plotGGPLOT <- renderPlot({
  if(!is.null(genomes$genomesNto1$alignments)) {

    if(is.null(rvAnnotation$annotation)){
      plotlyRV$plotGG
      
    } else {
      
      annotationTable <- subset(rvAnnotation$annotation, type == input$typeSelector)
      annotationTable$attributes = unlist(lapply(annotationTable$attributes, function(x){
        texteinter = unlist(strsplit(unlist(strsplit(x, ";")), '='))
        texte = texteinter[seq(2,length(texteinter), 2)]
        names(texte) = firstup(sub("_", " ", texteinter[seq(1,length(texteinter), 2)]))
        texte = paste(paste("<b>",names(texte), "</b>:",texte), collapse = "<br>")
      }))

      levelAnnot <- vector(mode = "list", length = 5)
      names(levelAnnot) <- 0:4
      annotationTable <- annotationTable %>% arrange(start)
      for(i in 1:nrow(annotationTable)){
        
        if(is.null(levelAnnot[[1]]) | all(annotationTable$start[i] > annotationTable$end[levelAnnot[[1]]])){
          levelAnnot[[1]] = c(levelAnnot[[1]], i)
        } else if(is.null(levelAnnot[[2]]) | all(annotationTable$start[i] > annotationTable$end[levelAnnot[[2]]])) {
          levelAnnot[[2]] = c(levelAnnot[[2]], i)
        }else if(is.null(levelAnnot[[3]]) | all(annotationTable$start[i] > annotationTable$end[levelAnnot[[3]]])) {
          levelAnnot[[3]] = c(levelAnnot[[3]], i)
        }else if(is.null(levelAnnot[[4]]) | all(annotationTable$start[i] > annotationTable$end[levelAnnot[[4]]])) {
          levelAnnot[[4]] = c(levelAnnot[[4]], i)
        }else if(is.null(levelAnnot[[5]]) | all(annotationTable$start[i] > annotationTable$end[levelAnnot[[5]]])) {
          levelAnnot[[5]] = c(levelAnnot[[5]], i)
        } else {
          levelAnnot[[1]] = c(levelAnnot[[1]], i)
        }
      }
      
      annotationTable$y = rep(names(levelAnnot), lengths(levelAnnot))
      annotationTable = annotationTable %>%
        mutate(arrowEnd = case_when(strand == "+" ~ "last", 
                                    strand == "-" ~ "first"), 
               color = case_when(strand == "+" ~ "blue", 
                                 strand == "-" ~ "orange" 
                                 
               ))
      
      plotlyRV$annotationTable = annotationTable
      
      plotlyRV$plotGG +
      geom_segment(data=annotationTable, mapping=aes(x=start, y=(as.numeric(y)*(3)),
                                                     xend=end,
                                                     yend=(as.numeric(y)*(3)),
                                                     tooltip = attributes),
                   arrow=grid::arrow(length = grid::unit(0.01, "npc"), 
                                     type = "closed", ends = as.character(annotationTable$arrowEnd)),
                   size=2, color=as.character(annotationTable$color))
      
    }
  } else {
    NULL
  }
})

################################################################################
# Explore 
################################################################################

output$hover_info <- renderUI({
  
  emptyTemp = HTML(paste("<table style='width: 100%;'><thead><tr><th>Species</th><th>PIP</th></tr></thead><tbody>", 
                         paste(plotlyRV$p %>%
                                 select(name) %>%
                                 distinct(name) %>%
                                 mutate(color = plotlyRV$colors, 
                                        printText = paste0("<tr><td><div style='float: left; margin: 3px 5px; width: 20px; height:12px;background-color:",color,"'></div>",name,"</td><td></td><tr>")
                                 ) %>% 
                                 pull (printText), collapse = ""), 
                         "</tbody></table>", collapse=""))
  
  if(!is.null(input$plot_hover)){
    hover=input$plot_hover
    
    if(round(hover$x) >= min(as.numeric(plotlyRV$p$x)) & round(hover$x) <= max(as.numeric(plotlyRV$p$x))){
      HTML(paste("<table style='width: 100%;'><thead><tr><th>Species</th><th style='text-align:center'>PIP</th></tr></thead><tbody>", 
                 paste(plotlyRV$p %>%
                         filter(as.numeric(x) == round(hover$x)) %>%
                         mutate(color = plotlyRV$colors, 
                                printText = paste0("<tr><td><div style='float: left; margin: 3px 5px; width: 20px; height:12px;background-color:",color,"'></div>",name,"</td><td style='text-align:center'>",round(as.numeric(y), 1),"</td><tr>")
                         ) %>% 
                         pull (printText), collapse = ""), 
                 "</tbody></table>", collapse=""))
    } else {
      emptyTemp
    }
    
  } else {
    
    emptyTemp
  }
})

output$hover_info_annot <- renderUI({
  if(!is.null(input$plot_hover) & !is.null(plotlyRV$annotationTable)){
    hover=input$plot_hover
    HTML(paste(plotlyRV$annotationTable %>%
                 filter(as.numeric(plotlyRV$annotationTable$start) <= round(hover$x) & as.numeric(plotlyRV$annotationTable$end) >= round(hover$x)) %>%
                 pull(attributes), collapse = "<br>"))
  }
})


output$brush_info <- renderUI({
  if(!is.null(input$plot_brush)){
    brush=input$plot_brush
    if(nrow(plotlyRV$p %>%
       filter(plotlyRV$p$x >= brush$xmin, 
              plotlyRV$p$x <= brush$xmax) %>% 
       group_by(name) ) != 0){
      HTML(paste("<table style='width: 100%;'><thead><tr><th>Species</th><th style='text-align:center'>Mean PIP</th></tr></thead><tbody>", 
                 paste(plotlyRV$p %>%
                         filter(plotlyRV$p$x >= brush$xmin, 
                                plotlyRV$p$x <= brush$xmax) %>% 
                         group_by(name) %>%
                         summarize(mean_size = mean(as.numeric(y), na.rm = TRUE)) %>%
                         mutate(color = plotlyRV$colors[name], 
                                printText = paste0("<tr><td><div style='float: left; margin: 3px 5px; width: 20px; height:12px;background-color:",color,"'></div>",name,"</td><td style='text-align:center'>",round(mean_size, 1),"</td><tr>")
                         ) %>% 
                         pull (printText), collapse = ""), 
                 "</tbody></table>", collapse=""))
    } else {
      NULL
    }
    
  } else {
    NULL
  }
})

################################################################################
# Update 
################################################################################


observeEvent(input$speciesColor_name, {
  updateColourInput(session, "speciesColor_picker", 
                    value = as.character(plotlyRV$colors[input$speciesColor_name]))
})

observeEvent(rvAnnotation$annotation, {
  if(!is.null(rvAnnotation$annotation)){
    updateSelectInput(session, "geneExplore_selector", 
                      choices = c(All = "All", setNames(rvAnnotation$feature$featureName, 
                                                        rvAnnotation$feature$featureName)),
                      selected = "All"
    )
  }
})


################################################################################
# Update - Buttons 
################################################################################

observeEvent(input$updateTitle, {
  plotlyRV$title_main = input$titleInput_main
  plotlyRV$title_x = input$titleInput_x
  plotlyRV$title_y = input$titleInput_y
  plotlyRV$title_legende = input$titleInput_legende
  
})

observeEvent(input$updateLimits, {
  
  plotlyRV$xlim = c(input$xlimRange[1],input$xlimRange[2])
  plotlyRV$ylim = c(input$ylimRange[1],input$ylimRange[2])
  
})


observeEvent(input$updateGenes, {
  
  if(input$geneExplore_selector == "All"){
    plotlyRV$xlim = c(0, 
                      length(plotlyRV$refPositions))
  } else {
    plotlyRV$xlim = sort(c(min(as.numeric(rvAnnotation$feature %>% filter(featureName == input$geneExplore_selector) %>% pull(start))),
                           max(as.numeric(rvAnnotation$feature %>% filter(featureName == input$geneExplore_selector) %>% pull(end)))))
  }
})

observeEvent(input$updateColor, {
  plotlyRV$colors[input$speciesColor_name] = input$speciesColor_picker
})

observeEvent(input$updateGrid, {
  plotlyRV$colMinorX = input$colMinorX
  plotlyRV$sizeMinorX = input$sizeMinorX 
  plotlyRV$colMajorX = input$colMajorX 
  plotlyRV$sizeMajorX = input$sizeMajorX 
  plotlyRV$colMinorY = input$colMinorY 
  plotlyRV$sizeMinorY = input$sizeMinorY 
  plotlyRV$colMajorY = input$colMajorY 
  plotlyRV$sizeMajorY = input$sizeMajorY
  
  plotlyRV$spaceMajorX = input$spaceMajorX 
  plotlyRV$spaceMinorX = input$spaceMinorX 
  plotlyRV$spaceMajorY = input$spaceMajorY 
  plotlyRV$spaceMinorY = input$spaceMinorY
})



################################################################################
# Generate plot - PIP profile
################################################################################

observeEvent(genomes$genomesNto1$alignments, {
  
  if(!is.null(genomes$genomesNto1$alignments)) {
    if(input$dataset == "rdata" | (input$dataset ==  "demo" & input$demoType == "rdata")){
      
      updateSliderInput(session, "xlimRange",
                        min = 0, max = length(plotlyRV$refPositions), 
                        value = c(plotlyRV$xlim[1], plotlyRV$xlim[2])
      )
      
      
      updateSliderInput(session, "ylimRange",
                        min = 0, max = 100, 
                        value = c(plotlyRV$ylim[1], plotlyRV$ylim[2])
      )
      updateTextInput(session, "titleInput_main", value = plotlyRV$title_main)
      updateTextInput(session, "titleInput_x", value =  plotlyRV$title_x )
      updateTextInput(session, "titleInput_y", value = plotlyRV$title_y)
      updateTextInput(session, "titleInput_legende", value = plotlyRV$title_legende)
      
      updateSelectizeInput(session, "speciesColor_name", 
                           choices =  setNames(names(plotlyRV$colors), 
                                               names(plotlyRV$colors)),
                           selected = names(plotlyRV$colors)[1])
      
      updateColourInput(session, "speciesColor_picker", 
                        value = as.character(plotlyRV$colors[names(plotlyRV$colors)[1]]))
      
      updateColourInput(session, "colMinorX",value = plotlyRV$colMinorX)
      updateColourInput(session, "colMajorX",value = plotlyRV$colMajorX)
      updateColourInput(session, "colMinorY",value = plotlyRV$colMinorY)
      updateColourInput(session, "colMajorY",value = plotlyRV$colMajorY )
      
      updateNumericInput(session, "sizeMinorX", value = plotlyRV$sizeMinorX)
      updateNumericInput(session, "sizeMajorX", value = plotlyRV$sizeMajorX)
      updateNumericInput(session, "sizeMinorY", value = plotlyRV$sizeMinorY)
      updateNumericInput(session, "sizeMajorY", value = plotlyRV$sizeMajorY)
      
      updateNumericInput(session, "spaceMajorX", value = plotlyRV$spaceMajorX)
      updateNumericInput(session, "spaceMinorX", value = plotlyRV$spaceMinorX)
      updateNumericInput(session, "spaceMajorY", value = plotlyRV$spaceMajorY)
      updateNumericInput(session, "spaceMinorY", value = plotlyRV$spaceMinorY)
      
      updateNumericInput(session, "windowSize", value = plotlyRV$windowSize)
      
      
    } else {
      createPIPprofile()
    }
  }
})


observeEvent({
  input$updateGeneral
  input$reversePlot
}, {
  if(!is.null(genomes$genomesNto1$alignments)) {
    createPIPprofile()
  }
})

createPIPprofile <- function(){
  #=========================================================================
  # Variables
  #=========================================================================
  
  reversePlot = input$reversePlot
  plotlyRV$windowSize = input$windowSize
  
  refID = NULL
  leftLimit = NULL
  rightLimit = NULL
  
  colors = genomes$strainColors
  
  #===========================================================================
  # Plot preparation
  #===========================================================================
  
  alignments = genomes$genomesNto1$alignments
  
  p <- NULL
  
  ## Initialise variables
  meanPIPs <- vector() ## Mean PIP per sequence
  i <- 1 #
  nbAlignments <- length(alignments)
  
  ## Color palette
  if (is.null(colors)) {
    colors <- rainbow(n = length(alignments))
    names(colors) <- names(alignments)
  }
  
  ggplotTable = NULL
  seqColors = NULL
  
  ## Plot PIP profiles
  i <- 1
  for (i in 1:length(alignments)) {
    if (reversePlot) {
      j <- length(alignments) - i + 1
    } else {
      j <- i
    }
    
    subject <- names(alignments)[j]
    alignment <- alignments[[j]]
    
    ## Get the aligned reference and query sequences
    refSeq <- unlist(strsplit(as.character(pattern(alignment)), split = ""))
    subjectSeq <- unlist(strsplit(as.character(subject(alignment)), split = ""))
    
    # ## Only keep the positions corresponding to the reference sequence (no gap in ref)
    plotlyRV$refPositions <- refSeq != "-"
    
    ## Compute identity per position
    identityProfile <- refSeq[plotlyRV$refPositions] == subjectSeq[plotlyRV$refPositions]
    
    
    ## Compute PIP profile
    pipProfile <- 100 * stats::filter(identityProfile, filter = rep(1/plotlyRV$windowSize, plotlyRV$windowSize))
    
    ## Compute PIP limits
    if (is.null(leftLimit)) {
      pipStart <- 1
    } else {
      pipStart <- leftLimit
    }
    if (is.null(rightLimit)) {
      pipEnd <- length(identityProfile)
    } else {
      pipEnd <- rightLimit
    }
    
    ## Compute mean PIP over the specified limits (pipStart, pipEnd)
    meanPIP <- round(digits = 3, 100 * sum(identityProfile[pipStart:pipEnd],na.rm = TRUE) / length(identityProfile))
    meanPIPs <- c(meanPIPs, meanPIP)
    
    message("\t", i, "/", nbAlignments,
            "\tpid = ", round(digits = 2, pid(alignment)),
            "\t", subject,
            "\tfrom ", pipStart,
            "\tto ", pipEnd,
            "\tmeanPIP = ", meanPIP
    )
    
    ## Sequence color
    if (is.null(colors[subject])) {
      seqColor <- colors[j]
    } else {
      seqColor <- colors[subject]
    }
    
    seqColors = c(seqColors, seqColor)
    
    ggplotTable = rbind.data.frame(
      ggplotTable, 
      cbind.data.frame(x = pipStart:pipEnd,
                       y = pipProfile[pipStart:pipEnd],
                       name = paste0(subject, " (", round(digits = 1, meanPIP), "%)"))
    )
  }
  
  #===========================================================================
  # Reactive values
  #===========================================================================
  
  plotlyRV$p = ggplotTable
  plotlyRV$colors = setNames(seqColors, 
                             unique(plotlyRV$p$name))
  rm(ggplotTable)
  
  plotlyRV$title_main = paste0("Full PIP profile - Ref: ", genomes$refGenomeName)
  plotlyRV$title_x = "Position "
  plotlyRV$title_y = paste0("PIP (", plotlyRV$windowSize," bp-averaged)")
  plotlyRV$title_legende = 'Species'
  
  plotlyRV$xlim = c(0,length(plotlyRV$refPositions))
  plotlyRV$ylim = c(0,100)
  
  plotlyRV$colMinorX = input$colMinorX 
  plotlyRV$sizeMinorX = input$sizeMinorX 
  plotlyRV$colMajorX = input$colMajorX 
  plotlyRV$sizeMajorX = input$sizeMajorX 
  plotlyRV$colMinorY = input$colMinorY 
  plotlyRV$sizeMinorY = input$sizeMinorY 
  plotlyRV$colMajorY = input$colMajorY
  plotlyRV$sizeMajorY = input$sizeMajorY
  
  if(length(plotlyRV$refPositions) < 5000) {
    plotlyRV$spaceMajorX = 500
    plotlyRV$spaceMinorX = 100
  } else {
    plotlyRV$spaceMajorX = 5000 
    plotlyRV$spaceMinorX = 1000
  }
  
  plotlyRV$spaceMajorY = input$spaceMajorY 
  plotlyRV$spaceMinorY = input$spaceMinorY
  
  #===========================================================================
  # Update
  #===========================================================================
  updateSliderInput(session, "xlimRange",
                    min = 0, max = length(plotlyRV$refPositions), 
                    value = c(0, length(plotlyRV$refPositions))
  )
  
  updateTextInput(session, "titleInput_main", value = "Full PIP profile")
  updateTextInput(session, "titleInput_x", value = "Position")
  updateTextInput(session, "titleInput_y", value = paste0("PIP (", plotlyRV$windowSize," bp-averaged)"))
  updateTextInput(session, "titleInput_legende", value = 'Species')
  
  updateNumericInput(session, "spaceMajorX", value = plotlyRV$spaceMajorX)
  updateNumericInput(session, "spaceMinorX", value = plotlyRV$spaceMinorX)
  updateNumericInput(session, "spaceMajorY", value = plotlyRV$spaceMajorY)
  updateNumericInput(session, "spaceMinorY", value = plotlyRV$spaceMinorY)
  
  updateSelectizeInput(session, "speciesColor_name", 
                       choices =  setNames(names(plotlyRV$colors), 
                                           names(plotlyRV$colors)),
                       selected = names(plotlyRV$colors)[1])
}

################################################################################
# Download plot 
################################################################################

output$downloadPlot <- downloadHandler(
  filename = function(){paste0("FullPIP",'.', input$ggsave_format)},
  content = function(file){
    ggsave(file, plot=plotlyRV$plotGG, 
           width = input$ggsave_width,
           height = input$ggsave_height,
           units = input$ggsave_unit,  
           dpi = input$ggsave_dpi)
  }
)