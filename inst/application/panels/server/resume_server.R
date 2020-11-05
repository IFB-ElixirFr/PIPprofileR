output$resumeQuery <-  renderDataTable({
  if(!is.null(genomes$genomesNto1)){
    if(genomes$seqType == "DNA"){
      genomes$SummarySequence  = do.call("rbind", lapply(genomes$genomesNto1$sequences, function(s){
        c(Size = nchar(s), (table(unlist(strsplit(as.character(s),"")))/nchar(s)) * 100)
      }))
      
    } else {
      suppressMessages(suppressWarnings(library(seqinr)))
      
      resInter <- do.call("rbind", lapply(genomes$genomesNto1$sequences, function(s){

        statInter = AAstat(unlist(strsplit(as.character(s), "")), plot = F)
        vectInter = setNames(rep(0, 31), 
                             c("Pi", "Tiny","Small","Aliphatic" ,"Aromatic","Non.polar", "Polar","Charged","Basic","Acidic",
                               "*","A","C","D","E","F","G","H","I","K","L","M","N","P","Q","R","S","T","V","W","Y"))
        
        vectInter[names(statInter$Compo)] = statInter$Compo
        vectInter[names(unlist(statInter$Prop))] = round(unlist(statInter$Prop), 2)
        vectInter["Pi"] = round(statInter$Pi, 2)
        vectInter = c(Size = nchar(s), vectInter)
      }))
      
      detach(package:seqinr)
      
      genomes$SummarySequence = resInter

    }
    genomes$SummarySequence 
  } else {
    NULL
  }
}, selection = 'none', options = list(scrollX = TRUE))


output$refName_ui <- renderUI(
  HTML(paste0("<p><b>Name </b>: ",names(genomes$genomesNto1$reference),"</p>"))
)

output$refSize_ui <- renderUI(
  HTML(paste0("<p><b>Size </b>: ",nchar(genomes$genomesNto1$reference),"</p>"))
)

output$refType_ui <- renderUI(
  if(!is.null(genomes$seqType)){
    message(genomes$seqType)
    if(genomes$seqType == "DNA"){
      HTML("<p><b>Sequence Type </b>:  Nucleic acid sequence</p>")
    } else {
      HTML("<p><b>Sequence Type </b>: Protein sequence</p>")
    }
  } else {
    HTML("<p><b>Sequence Type </b>: </p>")
  }

)



