################################################################################
# Reference
################################################################################

output$refNameAlignemnt_ui <- renderUI(
  HTML(paste0("<p><b>Name </b>: ",names(genomes$genomesNto1$reference),"</p>"))
)

output$refSizeAlignemnt_ui <- renderUI(
  HTML(paste0("<p><b>Size </b>: ",nchar(genomes$genomesNto1$reference),"</p>"))
)

output$refTypeAlignemnt_ui <- renderUI(
  HTML(paste0("<p><b>Type of alignment </b>: ",genomes$genomesNto1$pairwiseType,"</p>"))
)

################################################################################
# Table
################################################################################

output$resultNto1 <-  renderDataTable({
  if(!is.null(genomes$genomesNto1)){

    withProgress(message = 'Preparation of alignment results', value = 0, {

      for(i in 1:length(genomes$genomesNto1$alignments)){

        incProgress(1/length(genomes$genomesNto1$alignments),
                    detail  = names(genomes$genomesNto1$alignments)[[i]])

        if(genomes$seqType == "DNA"){
          seq <- c(aligned(pattern(genomes$genomesNto1$alignments[[i]])), aligned(subject(genomes$genomesNto1$alignments[[i]])))

        } else {
          seq <- c(AAStringSet(pattern(genomes$genomesNto1$alignments[[i]])), AAStringSet(subject(genomes$genomesNto1$alignments[[i]])))
        }
        names(seq) = c(names(genomes$genomesNto1$reference), names(genomes$genomesNto1$alignments)[[i]])
        BrowseSeqs(seq, htmlFile = paste0(tmpFolder, "/BrowseSeqs/BrowseSeqs_",names(genomes$genomesNto1$alignments)[[i]],".html"),
                   openURL = F)
      }
    })

    inter = cbind.data.frame(allNames = names(plotlyRV$colors), color = plotlyRV$colors)  %>%
      mutate(name = unlist(lapply(names(plotlyRV$colors), function(x){
        pos = which(unlist(strsplit(x, "")) == "(")
        pos = pos[length(pos)]
        substr(x,1,pos-2)
      })))

      inter = inter %>% left_join(genomes$genomesNto1$stats  %>%
                            mutate(name = rownames(genomes$genomesNto1$stats)),
                          by="name") %>%  arrange(desc(score))  %>%
      mutate( Explore = paste0("<a href='",paste0(tmpFolderWithoutWWW, "/BrowseSeqs/BrowseSeqs_",name,".html"),"' target='_blank'>View alignment!</a>")) %>%
        mutate(name = allNames) %>%
        select(-allNames) %>%
        arrange(match(name, rev(levels(plotlyRV$p$name))))

      DT::datatable(inter, rownames = FALSE,  colnames = c('', colnames(inter)[-1]),
                    selection = 'none',escape = F, extensions = 'FixedColumns',
                    options = list( dom = 't', scrollX = T,
                                    fixedColumns = list(leftColumns = c(2,3)))
      ) %>%
        formatStyle(
          'color',
          width='20px',
          `font-size` ="1px",
          color = styleEqual(
            inter$color, inter$color
          ),
          backgroundColor = styleEqual(
            inter$color, inter$color
          )
        )

  } else {
    NULL
  }
}, selection = 'none', escape = FALSE, options = list(scrollX = TRUE))

################################################################################
# Plot
################################################################################

output$scorePlot <- renderPlot({
  if(!is.null(genomes$genomesNto1)){

    inter = cbind.data.frame(name = rownames(genomes$genomesNto1$stats),
                             score = round(genomes$genomesNto1$stats$score))
    inter = inter[order(inter$score, decreasing = TRUE), ]
    inter$name <- factor(inter$name, levels = inter$name)

    plotlyRV$plotGG_scorePlot<- ggplot(data=inter, aes(x=name, y=score)) +
      geom_bar(stat="identity", fill="steelblue")+
      geom_text(aes(label=score), vjust=-0.3, size=3.5)+
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      labs(title="Score distribution",
           x ="Species", y = "Score")

    plotlyRV$plotGG_scorePlot
  } else {
    NULL
  }
})

output$pidPlot <- renderPlot({
  if(!is.null(genomes$genomesNto1)){
    inter = cbind.data.frame(name = rownames(genomes$genomesNto1$stats),
                             pid = round(genomes$genomesNto1$stats$pid))
    inter = inter[order(inter$pid, decreasing = TRUE), ]
    inter$name <- factor(inter$name, levels = inter$name)

    plotlyRV$plotGG_pidPlot<- ggplot(data=inter, aes(x=name, y=pid)) +
      geom_bar(stat="identity", fill="steelblue")+
      geom_text(aes(label=pid), vjust=-0.3, size=3.5)+
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      labs(title="PID distribution",
           x ="Species", y = "PID")

    plotlyRV$plotGG_pidPlot

  } else {
    NULL
  }
})


################################################################################
# Download plot
################################################################################

output$downloadPlot_scorePlot <- downloadHandler(
  filename = function(){paste0("scorePlot",'.', input$ggsave_format_scorePlot)},
  content = function(file){
    ggsave(file, plot=plotlyRV$plotGG_scorePlot,
           device = as.character(input$ggsave_format_scorePlot),
           width = as.numeric(input$ggsave_width_scorePlot),
           height = as.numeric(input$ggsave_height_scorePlot),
           units = as.character(input$ggsave_unit_scorePlot),
           dpi = as.numeric(input$ggsave_dpi_scorePlot))
  }
)

output$downloadPlot_pidPlot <- downloadHandler(
  filename = function(){paste0("pidPlot",'.', input$ggsave_format_pidPlot)},
  content = function(file){
    ggsave(file, plot=plotlyRV$plotGG_pidPlot,
           device = as.character(input$ggsave_format_pidPlot),
           width = as.numeric(input$ggsave_width_pidPlot),
           height = as.numeric(input$ggsave_height_pidPlot),
           units = as.character(input$ggsave_unit_pidPlot),
           dpi = as.numeric(input$ggsave_dpi_pidPlot))
  }
)
