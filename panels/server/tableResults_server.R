output$resultNto1 <-  renderDataTable({
  if(!is.null(genomes$genomesNto1)){
    genomes$genomesNto1$stats[order(genomes$genomesNto1$stats$score, decreasing = TRUE), ]
  } else {
    NULL
  }
}, selection = 'none', options = list(scrollX = TRUE))


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
           device = input$ggsave_format_scorePlot, 
           width = input$ggsave_width_scorePlot,
           height = input$ggsave_height_scorePlot,
           units = input$ggsave_unit_scorePlot,  
           dpi = input$ggsave_dpi_scorePlot)
  }
)

output$downloadPlot_pidPlot <- downloadHandler(
  filename = function(){paste0("pidPlot",'.', input$ggsave_format_pidPlot)},
  content = function(file){
    ggsave(file, plot=plotlyRV$plotGG_pidPlot, 
           device = input$ggsave_format_pidPlot, 
           width = input$ggsave_width_pidPlot,
           height = input$ggsave_height_pidPlot,
           units = input$ggsave_unit_pidPlot,  
           dpi = input$ggsave_dpi_pidPlot)
  }
)
