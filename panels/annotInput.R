annotInput <- function(){
  
  modalDialog(
    title = "Import an annotation file",
    helpText("You can add an annotation file to enrich the exploration of the PIP profile or refine the search during alignments."),
    fluidRow(class="align-middle", style = "border: solid 3px #7a9eff ; margin: 5px 1px !important;",
             column(2, p(icon("question-circle"), 
                         style = "text-align:center ; font-weight: bold; color: #7a9eff; font-size: 40px; margin : 0px") ),
             column(10,p("Need more information about GFF or GTF format ?", 
                         tags$a(href= "https://www.ensembl.org/info/website/upload/gff.html", "Click here !"),
                         style="margin-top : 5px; margin-bottom : 5px;"),
                    p("Need more information about bed format ? ", 
                      tags$a(href="https://www.ensembl.org/info/website/upload/bed.html", "Click here !"),
                      style="margin-top : 5px; margin-bottom : 5px;")
             )
    ), 
    div(id="annotError", style="display : none;", 
        fluidRow(
          column(2, p(icon("exclamation-triangle"), 
                      style = "text-align:center ; font-weight: bold; color: red; font-size: 40px; margin : 0px") ),
          column(10,p("Invalid annotation file. Please try again", 
                      style="margin-top : 5px; margin-bottom : 5px;font-weight: bold; color: red;"), 
                 actionLink(inputId = "moreErrorAnnot_btn", label = "more"), 
                 div(id="moreErrorAnnot_div",style="display : none; overflow-x: auto;", uiOutput("errorArea"))
          ), style = "border: solid 3px red ; margin: 5px 1px !important;"
        )
    ),
    
    radioButtons(
      inputId = "annotset",
      label = "Select dataset : ",
      inline = TRUE,
      choices = list(
        "GFF3" = "gff3",
        "GFF2 / GTF" = "gtf"
        ,"bed" = "bed"
      ),
      selected = "gff3"
    ),
    
    wellPanel(uiOutput("annotUI")),
    
    h4("File preview (Only five lines)"),
    dataTableOutput(outputId = "contentAnnot"),
    
    footer = tagList(modalButton("Cancel"),
                     actionButton(inputId = "okAnnot", label = "OK"))
  )
}

output$errorArea <- renderUI({
  if (! is.null(rvAnnotation$error)){
    tags$code(rvAnnotation$error)
  } else {
    NULL
  }
})


output$annotUI <- renderUI({
  disable("okAnnot")
  rvAnnotation$annotationTmp  <- NULL
  if (is.null(input$annotset))
    return()
  
  switch(
    input$annotset,
    "gff3" = fileInput("fileAnnot",label = "Select GFF3 file",
                       buttonLabel = "Browse...",
                       placeholder = "No file selected",
                       accept = c(".gff", ".gff3")
    ),
    
    "gtf" = fileInput("fileAnnot",label = "Select GFF2 or GFT file",
                      buttonLabel = "Browse...",
                      placeholder = "No file selected",
                      accept = c(".gtf", ".gff2")
    ),
    "bed" = fileInput(
      inputId = "fileAnnot",
      label = "Select bed file ",
      placeholder = "No file selected", 
      accept = ".bed"
    )
  )
})


observeEvent(input$fileAnnot, {
  tryCatch({
    switch(
      input$annotset,
      "gff3" = {
        rvAnnotation$annotationTmp <- read.gff(input$fileAnnot$datapath, 
                                            GFF3 = TRUE )
      },
      "gtf" = {
        rvAnnotation$annotationTmp <- read.gff(input$fileAnnot$datapath, 
                                            GFF3 = FALSE )
      },
      "bed" = {
        
      }
    )
    
  },
  error=function(cond) {
    rvAnnotation$annotationTmp  <- NULL
    message(paste("Message : ", paste(cond, collapse = "\n")))
    rvAnnotation$error <- paste(cond, collapse = "\n")
    shinyjs::show(id = "annotError")
    disable("okAnnot")
  })
  
})


observeEvent(input$moreErrorAnnot_btn, {
  if(input$moreErrorAnnot_btn %% 2 == 1){
    shinyjs::show(id = "moreErrorAnnot_div")
    moreErrorAnnot_label <- "Hide R error message"
  }else{
    shinyjs::hide(id = "moreErrorAnnot_div")
    moreErrorAnnot_label <- "Show R error message"
  }
  updateActionButton(session, "moreErrorAnnot_btn", label = moreErrorAnnot_label)
})

output$contentAnnot = renderDataTable({
  if(!is.null(rvAnnotation$annotationTmp )){
    shinyjs::hide(id = "annotError")
    enable("okAnnot")
    rvAnnotation$annotationTmp[1:5, ] 
  } else {
    NULL
  }
}
, 
selection = 'none', escape = FALSE,
options = list(pageLength = 5, scrollX = TRUE)
)


observeEvent(input$okAnnot, {
  rvAnnotation$annotation <- rvAnnotation$annotationTmp
  rvAnnotation$annotationTmp <- NULL
  removeModal()
  shinyjs::show(id = "geneExplore")
  
  inter <- rvAnnotation$annotation  %>% filter(type == 'gene')
  geneName <- unlist(lapply(inter$attributes, function(x){
    gsub("ID=", "", grep("ID=", unlist(strsplit(x,";")), value = T))
  }))
  
  
  attrtibuteHTML <- unlist(lapply(inter$attributes, function(x){
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
  ))

  rvAnnotation$Gene <-  cbind.data.frame(inter, 
                                         geneName, 
                                         attrtibuteHTML)
  
  
})