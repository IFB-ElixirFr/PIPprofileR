output$resumeQuery <-  renderDataTable({
  if(!is.null(genomes$genomesNto1)){
    if(genomes$seqType == "DNA"){
      resInter  = do.call("rbind", lapply(genomes$genomesNto1$sequences, function(s){
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
      rownames(resInter) = as.character(names(genomes$genomesNto1$sequences))
    }


    if(!is.null(plotlyRV$colors) ){

      resInter <- as.data.frame(resInter) %>%
        mutate(name = unlist(lapply(rownames(resInter), function(x){
          pos = which(unlist(strsplit(x, "")) == "_")
          pos = pos[length(pos)]
          substr(x,1,pos-1)
        })))


      inter = cbind.data.frame(allNames = names(plotlyRV$colors), color = plotlyRV$colors)  %>%
        mutate(name = unlist(lapply(names(plotlyRV$colors), function(x){
          pos = which(unlist(strsplit(x, "")) == "(")
          pos = pos[length(pos)]
          substr(x,1,pos-2)
        })))

      resInter <- inter  %>%
        left_join(resInter, by="name") %>%
        mutate(name = allNames) %>%
        select(-allNames) %>%
        arrange(match(name, rev(levels(plotlyRV$p$name))))
    }

    genomes$SummarySequence = resInter

    DT::datatable(genomes$SummarySequence, rownames = FALSE,  colnames = c('', colnames(genomes$SummarySequence)[-1]),
                  selection = 'none',escape = F, extensions = 'FixedColumns',
                  options = list( dom = 't', scrollX = T,
                                  fixedColumns = list(leftColumns = c(2,3)))
    ) %>%
      formatStyle(
        'color',
        width='20px',
        `font-size` ="1px",
        color = styleEqual(
          genomes$SummarySequence$color, genomes$SummarySequence$color
        ),
        backgroundColor = styleEqual(
          genomes$SummarySequence$color, genomes$SummarySequence$color
        )
      )

  } else {
    NULL
  }
})




output$refName_ui <- renderUI(
  HTML(paste0("<p><b>Reference name </b>: ",names(genomes$genomesNto1$reference),"</p>"))
)

output$refSize_ui <- renderUI(
  HTML(paste0("<p><b>Reference size </b>: ",nchar(genomes$genomesNto1$reference),"</p>"))
)

output$querrySizeMean_ui <- renderUI(
  HTML(paste0("<p><b>Mean size of query sequences </b>: ",round(mean(unlist(lapply(genomes$genomesNto1$alignments, nchar)))),"</p>"))
)

output$nbQuerry <- renderUI(
  HTML(paste0("<p><b>Number of query sequences </b>: ",length(genomes$genomesNto1$alignments),"</p>"))
)

output$refType_ui <- renderUI(
  if(!is.null(genomes$seqType)){
    if(genomes$seqType == "DNA"){
      HTML("<p><b>Sequence Type </b>:  Nucleic acid sequence</p>")
    } else {
      HTML("<p><b>Sequence Type </b>: Protein sequence</p>")
    }
  } else {
    HTML("<p><b>Sequence Type </b>: </p>")
  }

)



