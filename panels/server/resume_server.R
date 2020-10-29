output$resumeQuery <-  renderDataTable({
  if(!is.null(genomes$genomesNto1)){
    if(genomes$genomesNto1$seqType == "DNA"){
      do.call("rbind", lapply(genomes$genomesNto1$sequences, function(s){
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
        vectInter[names(unlist(statInter$Prop))] = unlist(statInter$Prop)
        vectInter["Pi"] = statInter$Pi
        vectInter = c(Size = nchar(s), vectInter)
      }))
      
      detach(package:seqinr)
      resInter
    }
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



