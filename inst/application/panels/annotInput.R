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
        "Demo" = "Demo",
        "GFF3" = "gff3",
        "GFF2 / GTF" = "gtf"
      ),
      selected = "Demo"
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

  rvAnnotation$annotationTmp  <- NULL
  if (is.null(input$annotset))
    return()

  switch(
    input$annotset,
    "gff3" = disable("okAnnot"),
    "gtf" = disable("okAnnot"),
    "Demo" = enable("okAnnot")
  )


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
    "Demo" = selectInput(
      inputId = "demoAnnot",
      label = "Select a demo annotation",
      choices = c("SARS-CoV-2" = "CoV-2")
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
      "Demo" = {
        message("annot demo")

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
  if(input$annotset == "Demo" & input$demoAnnot == "CoV-2" ){
    rvAnnotation$annotationTmp <- read.gff("dataExample/GCF_009858895.2_ASM985889v3_genomic.gff",
                                           GFF3 = TRUE )
  }

  rvAnnotation$annotation <- rvAnnotation$annotationTmp
  rvAnnotation$annotationTmp <- NULL
  removeModal()
  shinyjs::show(id = "geneExplore")
  updateTabItems(session, "tabs", selected = "annotationTabItem")
})
