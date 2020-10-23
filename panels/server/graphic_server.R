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
  if(input$dynamicPlot){
    withLoader(plotlyOutput("plotPLOTLY", height = "500px"))
  } else if( is.null(rvAnnotation$annotation )) {
    withLoader(plotOutput("plotGGPLOT", height = "500px"))
  } else {
    withLoader(girafeOutput("plotGGPLOT_ggiraph",  height = "100%"))
  }
  
)

output$plotArea_title <- renderUI(
  if(!is.null(genomes$refGenomeName)){
    div(
      h1("Full genome PIP profile"),
      h2(paste0("Ref: ", genomes$refGenomeName))
    )
  } else {
    h1("Full genome PIP profile")
  }
  
)

output$resumeGene_general <- renderUI({ 
  div(
    h4("General information"), 
    p(tags$b("Sequence ID: "), rvAnnotation$Gene  %>% filter(geneName == input$geneExplore_selector) %>% pull(seqid),tags$br(),
      tags$b("Source: "),rvAnnotation$Gene  %>% filter(geneName == input$geneExplore_selector) %>% pull(source),tags$br(),
      tags$b("Start: "),rvAnnotation$Gene  %>% filter(geneName == input$geneExplore_selector) %>% pull(start),tags$br(),
      tags$b("End: "),rvAnnotation$Gene  %>% filter(geneName == input$geneExplore_selector) %>% pull(end),tags$br(),
      tags$b("Score: "),rvAnnotation$Gene  %>% filter(geneName == input$geneExplore_selector) %>% pull(end),tags$br(),
      tags$b("Strand: "),rvAnnotation$Gene  %>% filter(geneName == input$geneExplore_selector) %>% pull(strand),tags$br(),
      tags$b("phase: "),rvAnnotation$Gene  %>% filter(geneName == input$geneExplore_selector) %>% pull(phase)),tags$br()
  )
  
})

output$resumeGene_attributes <- renderUI({
  div(
    h4("Attributes"), 
    p(HTML(rvAnnotation$Gene  %>% filter(geneName == input$geneExplore_selector) %>% pull(attrtibuteHTML)))
  )
})

################################################################################
# Generate plotly
################################################################################

observe({
  if(!is.null(plotlyRV$p)){
    plotlyRV$plotGG <- ggplot(plotlyRV$p, aes(x = x, y = y, colour = name)) + 
      geom_line() + 
      ggtitle(plotlyRV$title_main) + 
      scale_color_manual(values= plotlyRV$colors) + 
      theme(panel.background = element_rect(fill = "white", colour = "white"),
            panel.grid.minor.y = element_line(colour=plotlyRV$colMinorH, size=plotlyRV$sizeMinorH) , 
            panel.grid.major.y = element_line(colour=plotlyRV$colMajorH, size=plotlyRV$sizeMajorH), 
            panel.grid.minor.x = element_line(colour=plotlyRV$colMinorV, size=plotlyRV$sizeMinorV) , 
            panel.grid.major.x = element_line(colour=plotlyRV$colMajorV, size=plotlyRV$sizeMajorV)) +
      scale_x_continuous( name= plotlyRV$title_x , limits=plotlyRV$xlim ,
                          breaks = seq(0, length(plotlyRV$refPositions), plotlyRV$spaceMajorH), 
                          minor_breaks = seq(0, length(plotlyRV$refPositions), plotlyRV$spaceMinorH))  +
      scale_y_continuous( name=plotlyRV$title_y, 
                          limits=plotlyRV$ylim , 
                          breaks = seq(0, 100, plotlyRV$spaceMajorV),
                          minor_breaks = seq(0, 100, plotlyRV$spaceMinorV)) + 
      labs(color=plotlyRV$title_legende) 
  }
})

output$plotPLOTLY <- renderPlotly({
  
  if(!is.null(genomes$genomesNto1$alignments) & input$dynamicPlot) {
    
    if(!is.null(rvAnnotation$annotation )){
      
      subplot(ggplotly(plotlyRV$plotGG)  %>%
                layout(hovermode = "x unified")
              , 
              plotlyRV$p2 %>%
                layout(yaxis = list(title = 'Annotation', 
                                    fixedrange=T,
                                    range = c(-1.5,1.5),
                                    zeroline = F,
                                    showline = F,
                                    showticklabels = F,
                                    showgrid = F),
                       xaxis = list( range = plotlyRV$xlim,
                                     title = "Position",
                                     zeroline = T,
                                     showline = T,
                                     showticklabels = T,
                                     showgrid = F)) , 
              shareX=TRUE,
              nrows = 2, 
              heights = c(0.8, 0.2))
    } else {
      ggplotly(plotlyRV$plotGG )  %>%
        layout(hovermode = "x unified")
    }
  } else {
    NULL
  }
  
})

output$plotGGPLOT <- renderPlot({
  if(!is.null(genomes$genomesNto1$alignments) & !input$dynamicPlot & is.null(rvAnnotation$annotation)) {
      plotlyRV$plotGG
  } else {
    NULL
  }
})


output$plotGGPLOT_ggiraph <- renderGirafe({
  if(!is.null(genomes$genomesNto1$alignments) & !input$dynamicPlot & !is.null(rvAnnotation$annotation )) {

      annotationTable <- subset(rvAnnotation$annotation, type == "gene")
      
      p <- plotlyRV$plotGG +
        geom_segment_interactive(data=annotationTable, mapping=aes(x=start, y=1,
                                                                   xend=end, 
                                                                   yend=1, 
                                                                   tooltip = attributes),
                                 arrow=grid::arrow(length = grid::unit(0.03, "npc")),
                                 size=2, color="blue")
      girafe(ggobj = p, width_svg = 12)
  } else {
    NULL
  }
})

################################################################################
# Update 
################################################################################

observeEvent(plotlyRV$p, {
  updateSelectizeInput(session, "speciesColor_name", 
                       choices =  setNames(names(plotlyRV$colors), 
                                           names(plotlyRV$colors)),
                       selected = names(plotlyRV$colors)[1])
})

observeEvent(input$speciesColor_name, {
  updateColourInput(session, "speciesColor_picker", 
                    value = as.character(plotlyRV$colors[input$speciesColor_name]))
})

observeEvent(rvAnnotation$annotation, {
  if(!is.null(rvAnnotation$annotation)){
    
    updateSelectInput(session, "geneExplore_selector", 
                      choices = c(All = "All", setNames(rvAnnotation$Gene$geneName, 
                                                        rvAnnotation$Gene$geneName)),
                      selected = "All"
    )
  }
  rm(inter)
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
    plotlyRV$xlim = sort(c(as.numeric(rvAnnotation$Gene %>% filter(geneName == input$geneExplore_selector) %>% pull(start)),
                           as.numeric(rvAnnotation$Gene %>% filter(geneName == input$geneExplore_selector) %>% pull(end))))
  }
})

observeEvent(input$updateColor, {
  plotlyRV$colors[input$speciesColor_name] = input$speciesColor_picker
})

observeEvent(input$updateGrid, {
  plotlyRV$colMinorH = input$colMinorH 
  plotlyRV$sizeMinorH = input$sizeMinorH 
  plotlyRV$colMajorH = input$colMajorH 
  plotlyRV$sizeMajorH = input$sizeMajorH 
  plotlyRV$colMinorV = input$colMinorV 
  plotlyRV$sizeMinorV = input$sizeMinorV 
  plotlyRV$colMajorV = input$colMajorV 
  plotlyRV$sizeMajorV = input$sizeMajorV
  
  plotlyRV$spaceMajorH = input$spaceMajorH 
  plotlyRV$spaceMinorH = input$spaceMinorH 
  plotlyRV$spaceMajorV = input$spaceMajorV 
  plotlyRV$spaceMinorV = input$spaceMinorV
})



################################################################################
# Generate plot - PIP profile
################################################################################

observeEvent({ 
  genomes$genomesNto1$alignments
  rvAnnotation$annotation
  input$updateGeneral
  input$reversePlot
} , {
  if(!is.null(genomes$genomesNto1$alignments)) {
    
    #===========================================================================
    # Variables
    #===========================================================================
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
      
      ggplotTable = rbind.data.frame(
        ggplotTable, 
        cbind.data.frame(x = pipStart:pipEnd,
                         y = pipProfile[pipStart:pipEnd],
                         name = paste0(subject, " (", round(digits = 1, meanPIP), "%)"), 
                         color= as.character(seqColor))
      )
    }
    
    #===========================================================================
    # Reactive values
    #===========================================================================
    plotlyRV$p <- ggplotTable
    interNames <- ggplotTable %>% distinct(name, color) 
    plotlyRV$colors <- setNames(interNames$color, 
                                interNames$name)
    rm(ggplotTable)
    
    plotlyRV$title_main = "Full PIP profile"
    plotlyRV$title_x = "Position"
    plotlyRV$title_y = paste0("PIP (", plotlyRV$windowSize," bp-averaged)")
    plotlyRV$title_legende = 'Species'
    
    plotlyRV$xlim = c(0,length(plotlyRV$refPositions))
    plotlyRV$ylim = c(0,100)
    
    plotlyRV$colMinorH = input$colMinorH 
    plotlyRV$sizeMinorH = input$sizeMinorH 
    plotlyRV$colMajorH = input$colMajorH 
    plotlyRV$sizeMajorH = input$sizeMajorH 
    plotlyRV$colMinorV = input$colMinorV 
    plotlyRV$sizeMinorV = input$sizeMinorV 
    plotlyRV$colMajorV = input$colMajorV 
    plotlyRV$sizeMajorV = input$sizeMajorV
    
    plotlyRV$spaceMajorH = input$spaceMajorH 
    plotlyRV$spaceMinorH = input$spaceMinorH 
    plotlyRV$spaceMajorV = input$spaceMajorV 
    plotlyRV$spaceMinorV = input$spaceMinorV
    
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
  }
})

################################################################################
# Generate plot - Annotation 
################################################################################

observeEvent(rvAnnotation$annotation, {
  if(!is.null(rvAnnotation$annotation )){
    p2 <- plot_ly() 
    
    inter <- subset(rvAnnotation$annotation, type=="gene" )
    
    for(i in 1:nrow(inter)){
      
      position_inter = c(as.numeric(as.character(inter$start[i])),
                         as.numeric(as.character(inter$end[i])))
      
      texteinter = unlist(strsplit(unlist(strsplit(inter$attributes[i], ";")), '='))
      texte = texteinter[seq(2,length(texteinter), 2)]
      names(texte) = firstup(sub("_", " ", texteinter[seq(1,length(texteinter), 2)]))
      
      texte = paste(paste("<b>",names(texte), "</b>:",texte), collapse = "<br>")
      
      if(inter$strand[i] == "+"){
        
        p2 <- add_trace(p2,
                        x = position_inter,
                        y = rep(0,2) ,
                        type="scatter",
                        mode = 'lines+markers',
                        symbol = I(15),
                        marker = list(
                          color = '#a37800',
                          size = 20,
                          symbol = c('triangle-right', 'triangle-left')
                        ),
                        line = list(color = 'orange', width = 8),
                        text = texte ,
                        hoverinfo = 'text',
                        showlegend = F
                        
        )
      } else {
        p2 <- add_trace(p2, type="scatter", 
                        x = position_inter,
                        y = rep(-1,2) ,
                        type="scatter",
                        mode = 'lines+markers',
                        symbol = I(15),
                        marker = list(
                          color = '#8800a3',
                          size = 20,  
                          symbol = c('triangle-right', 'triangle-left')
                        ),
                        line = list(color = 'purple', width = 8),
                        text = texte ,
                        hoverinfo = 'text',
                        showlegend = F
        )
      }
      
      plotlyRV$p2 <- p2
    }
    rm(inter)
    
  }
})

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