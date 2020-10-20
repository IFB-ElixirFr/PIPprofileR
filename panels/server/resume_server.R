output$resumeQuery <-  renderDataTable({
  if(!is.null(genomes$genomesNto1)){
    genomes$genomesNto1$stats[order(genomes$genomesNto1$stats$score, decreasing = TRUE), ]
  } else {
    NULL
  }
}, selection = 'none', options = list(scrollX = TRUE))