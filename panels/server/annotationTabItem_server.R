output$annotationTable <-  renderDataTable({
  if(!is.null(rvAnnotation$annotation)){
    inter <- rvAnnotation$annotation
    
    inter$attributes <- lapply(inter$attributes, function(x){
      texteinter = unlist(strsplit(unlist(strsplit(x, ";")), '='))
      texte = texteinter[seq(2,length(texteinter), 2)]
      names(texte) = firstup(sub("_", " ", texteinter[seq(1,length(texteinter), 2)]))
      
      pos  = which(names(texte) == 'Dbxref')
      if(length(pos) != 0){
        interRef <-  unlist(strsplit(unlist(strsplit(texte[pos], ":")), ','))
        interRef_ref = interRef[seq(2,length(interRef), 2)]
        names(interRef_ref) = interRef[seq(1,length(interRef), 2)]
        
        interRef_ref <- setNames(
          unlist(lapply(seq_along(interRef_ref), function(id, n, i){
            switch(n[[i]], 
                   'taxon' = paste0("<span style='margin-left:20px'><i>", n[[i]], "</i> : ","<a href='https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=", id[[i]], "' target='_blank'>", id[[i]], "</a></span>"),
                   'Genbank' = paste0("<span style='margin-left:20px'><i>",n[[i]], "</i> : ","<a href='https://www.ncbi.nlm.nih.gov/protein/", id[[i]], "' target='_blank'>",  id[[i]], "</a></span>"),
                   'GeneID' = paste0("<span style='margin-left:20px'><i>",n[[i]], "</i> : ","<a href='https://www.ncbi.nlm.nih.gov/gene/?term=", id[[i]], "' target='_blank'>",  id[[i]], "</a></span>"),
                   {
                     paste0("<i>",n[[i]], "</i> : ",  id[[i]])
                   }
            )
          }, id = interRef_ref, n = names(interRef_ref)
          )), 
          names(interRef_ref)
        )  
        interRef_ref = paste0(interRef_ref, collapse = "<br>")
        interRef_ref = paste0(c("<br>", interRef_ref), collapse = "")
        texte[pos] <- interRef_ref
      }
      
      texte = paste(paste("<b>",names(texte), "</b>:",texte), collapse = "<br>")
    }
    )
    
    inter 
  } else {
    NULL
  }
}, selection = 'none', filter ="top",escape = FALSE,  options = list(scrollX = TRUE))