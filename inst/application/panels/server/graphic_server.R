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
  withLoader(plotOutput("plotGGPLOT", height = "500px",
                        hover = hoverOpts(id ="plot_hover", delay=100),
                        click = clickOpts(id ="plot_click"),
                        brush = brushOpts(id ="plot_brush", delay=100)))
)

output$plotArea_title <- renderUI(
  if(!is.null(genomes$refGenomeName)){
    div(
      h1("PIP profile"),
      h2(paste0("Ref: ", genomes$refGenomeName))
    )
  } else {
    h1("PIP profile")
  }

)

output$typeSequencesPIP <- renderUI(
  if(genomes$seqType == "DNA"){
    p("Nucleic acid sequences")
  } else {
    p("Protein sequences")
  }
)

output$resumeGene_general <- renderUI({
  if(input$geneExplore_selector != "All"){

    inter = rvAnnotation$feature  %>% filter(featureName == input$geneExplore_selector)
    para = NULL
    for(i in 1:nrow(inter)){
      para = c(para, as.character(p(tags$b(tags$u(paste("Occurence ", i ))),tags$br(),
                                    tags$b("Sequence ID: "), inter$seqid[i],tags$br(),
                                    tags$b("Source: "),inter$source[i],tags$br(),
                                    tags$b("Start: "),inter$start[i],tags$br(),
                                    tags$b("End: "),inter$end[i],tags$br(),
                                    tags$b("Score: "),inter$end[i],tags$br(),
                                    tags$b("Strand: "),inter$strand[i],tags$br(),
                                    tags$b("phase: "),inter$phase[i])))

    }
    para = paste0(para, collapse = "")

    div(
      h4("General information"),
      HTML(para)
    )
  } else {
    div(
      h4("General information")
    )
  }

})

output$resumeGene_attributes <- renderUI({

  if(input$geneExplore_selector != "All"){

    inter = rvAnnotation$feature  %>% filter(featureName == input$geneExplore_selector) %>% pull(attrtibuteHTML)
    para = NULL
    for(i in 1:length(inter)){
      para = c(para, as.character(p(tags$b(tags$u(paste("Occurence ", i ))),tags$br(),
                                    HTML(inter[i]))))

    }
    para = paste0(para, collapse = "")

    div(
      h4("Attributes"),
      HTML(para)
    )
  } else {
    div(
      h4("Attributes")
    )
  }

})

################################################################################
# Generate plot
################################################################################

observe({
  if(!is.null(plotlyRV$p)){
    plotlyRV$plotGG <- ggplot(subset(plotlyRV$p, name %in% plotlyRV$selectedRow),
                              aes(x = x, y = y, colour = name)) +
      geom_line() +
      ggtitle(plotlyRV$title_main) +
      scale_color_manual(values= plotlyRV$colors) +
      theme(panel.background = element_rect(fill = "white", colour = "white"),
            panel.grid.minor.y = element_line(colour=plotlyRV$colMinorY, size=plotlyRV$sizeMinorY) ,
            panel.grid.major.y = element_line(colour=plotlyRV$colMajorY, size=plotlyRV$sizeMajorY),
            panel.grid.minor.x = element_line(colour=plotlyRV$colMinorX, size=plotlyRV$sizeMinorX) ,
            panel.grid.major.x = element_line(colour=plotlyRV$colMajorX, size=plotlyRV$sizeMajorX),
            legend.key=element_blank(),
            plot.title = element_text(size = 16, face = "bold"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            legend.title = element_text(size=14,face="bold"),
            legend.text = element_text(size=12)) +
      coord_cartesian(xlim = plotlyRV$xlim,
                      ylim = plotlyRV$ylim) +
      scale_x_continuous( name= plotlyRV$title_x ,
                          # limits=plotlyRV$xlim ,
                          breaks = seq(0, length(plotlyRV$refPositions), plotlyRV$spaceMajorX),
                          minor_breaks = seq(0, length(plotlyRV$refPositions), plotlyRV$spaceMinorX))  +
      scale_y_continuous( name=plotlyRV$title_y,
                          # limits=plotlyRV$ylim ,
                          breaks = seq(0, 100, plotlyRV$spaceMajorY),
                          minor_breaks = seq(0, 100, plotlyRV$spaceMinorY)) +
      labs(color=plotlyRV$title_legende) +
      guides(color = guide_legend(override.aes = list(size = 2), reverse = T))
  }
})

observeEvent(rvAnnotation$annotation, {
  if(!is.null(rvAnnotation$annotation)){
    plotlyRV$ylim = c(input$ylimRange[1],120)
    updateSliderInput(session, "ylimRange",
                      min = 0, max = 120,
                      value = c(plotlyRV$ylim[1], plotlyRV$ylim[2])
    )
  }
})

output$plotGGPLOT <- renderPlot({
  if(!is.null(genomes$genomesNto1$alignments)) {

    if(is.null(rvAnnotation$annotation)){
      plotlyRV$plotGG

    } else {

      annotationTable <- subset(rvAnnotation$annotation, type == input$typeSelector)
      annotationTable$attributes = unlist(lapply(annotationTable$attributes, function(x){
        texteinter = unlist(strsplit(unlist(strsplit(x, ";")), '='))
        texte = texteinter[seq(2,length(texteinter), 2)]
        names(texte) = firstup(sub("_", " ", texteinter[seq(1,length(texteinter), 2)]))
        texte = paste(paste("<b>",names(texte), "</b>:",texte), collapse = "<br>")
      }))

      levelAnnot <- vector(mode = "list", length = 5)
      names(levelAnnot) <- 0:4
      annotationTable <- annotationTable %>% arrange(start)
      position = NULL
      for(i in 1:nrow(annotationTable)){

        if(is.null(levelAnnot[[1]]) | all(annotationTable$start[i] > (annotationTable$end[levelAnnot[[1]]] +100) )){
          levelAnnot[[1]] = c(levelAnnot[[1]], i)
          position = c(position, 0)
        } else if(is.null(levelAnnot[[2]]) | all(annotationTable$start[i] > (annotationTable$end[levelAnnot[[2]]]+100))) {
          levelAnnot[[2]] = c(levelAnnot[[2]], i)
          position = c(position, 1)
        }else if(is.null(levelAnnot[[3]]) | all(annotationTable$start[i] > (annotationTable$end[levelAnnot[[3]]]+100))) {
          levelAnnot[[3]] = c(levelAnnot[[3]], i)
          position = c(position, 2)
        }else if(is.null(levelAnnot[[4]]) | all(annotationTable$start[i] > (annotationTable$end[levelAnnot[[4]]]+100))) {
          levelAnnot[[4]] = c(levelAnnot[[4]], i)
          position = c(position, 3)
        }else if(is.null(levelAnnot[[5]]) | all(annotationTable$start[i] > (annotationTable$end[levelAnnot[[5]]]+100))) {
          levelAnnot[[5]] = c(levelAnnot[[5]], i)
          position = c(position, 4)
        } else {
          levelAnnot[[1]] = c(levelAnnot[[1]], i)
          position = c(position, 0)
        }
      }

      annotationTable$y = position
      annotationTable = annotationTable %>%
        mutate(arrowEnd = case_when(strand == "+" ~ "last",
                                    strand == "-" ~ "first"),
               color = case_when(strand == "+" ~ "blue",
                                 strand == "-" ~ "orange"

               ))

      plotlyRV$annotationTable = annotationTable

      RMD$plot <- plotlyRV$plotGG + geom_rect(inherit.aes = FALSE, data = data.frame(xmin = -Inf,
                                                                                     xmax = Inf,
                                                                                     ymin = 101,
                                                                                     ymax = Inf),
                                              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                              fill = "white") +
        geom_segment(data=annotationTable, mapping=aes(x=start, y=(as.numeric(y)*(3) + 105),
                                                       xend=end,
                                                       yend=(as.numeric(y)*(3) + 105),
                                                       tooltip = attributes),
                     arrow=grid::arrow(length = grid::unit(0.01, "npc"),
                                       type = "closed", ends = as.character(annotationTable$arrowEnd)),
                     size=2, color=as.character(annotationTable$color))

      RMD$plot
    }
  } else {
    NULL
  }
})

################################################################################
# Explore
################################################################################

#===============================================================================
# Datatbale
#===============================================================================

output$pipExplo <-  renderDT({
  # if(!is.null(plotlyRV$p)){
  #
  #   inter <- plotlyRV$p %>%
  #     group_by(name) %>%
  #     summarize(
  #       "Min" = round(min(as.numeric(y), na.rm = TRUE),2),
  #       "1st Quantile" = round(quantile(as.numeric(y), na.rm = TRUE)[2],2),
  #       "Median" = round(median(as.numeric(y), na.rm = TRUE),2),
  #       "Mean" = round(mean(as.numeric(y), na.rm = TRUE),2),
  #       "3rd Quantile" = round(quantile(as.numeric(y), na.rm = TRUE)[4],2),
  #       "Max"  = round(max(as.numeric(y), na.rm = TRUE),2)) %>%
  #     arrange(match(name, rev(levels(plotlyRV$p$name)))) %>%
  #     mutate(Color = plotlyRV$colors[rev(levels(plotlyRV$p$name))]) %>%
  #
  # if(!is.null(inter) & !is.null(input$plot_hover)){
  #   hover=input$plot_hover
  #   if(round(hover$x) >= min(as.numeric(plotlyRV$p$x)) & round(hover$x) <= max(as.numeric(plotlyRV$p$x))){
  #     inter <- inter %>%
  #       left_join(plotlyRV$p %>% filter(as.numeric(x) == round(hover$x)), by = "name")
  #   } else {
  #     inter <- inter %>%
  #       mutate(y = "")
  #   }
  # }
  #   inter <- inter %>%
  #     rename("Strains/Species" = name) %>%
  #     select("Color","Strains/Species", "Color", "Min", "1st Quantile", "Median",
  #            "Mean", "3rd Quantile", "Max", "Hover")
  #
  #   datatable(inter, rownames = FALSE, container = sketch, colnames = c('', colnames(inter)[-1]),
  #             selection = 'none',options = list(scrollX = TRUE)) %>%
  #     formatStyle(
  #       'Color',
  #       width='20px',
  #       `font-size` ="1px",
  #       color = styleEqual(
  #         inter$Color, inter$Color
  #       ),
  #       backgroundColor = styleEqual(
  #         inter$Color, inter$Color
  #       )
  #     )
  #
  # } else {
  #   NULL
  # }

  if(!is.null(plotlyRV$p) & !is.null(plotlyRV$colors)){

    #---------------------------------------------------------------------------
    # Create DT
    #---------------------------------------------------------------------------
    inter = plotlyRV$p %>%
      group_by(name) %>%
      summarize(
        "Min" = min(as.numeric(y), na.rm = TRUE),
        "1st Qu." = quantile(as.numeric(y), na.rm = TRUE)[2],
        "Median" = median(as.numeric(y), na.rm = TRUE),
        "Mean" = mean(as.numeric(y), na.rm = TRUE),
        "3rd Qu." = quantile(as.numeric(y), na.rm = TRUE)[4],
        "Max"  = max(as.numeric(y), na.rm = TRUE),
        options(dplyr.summarise.inform=F) ) %>%
      arrange(match(name, rev(levels(plotlyRV$p$name)))) %>%
      mutate(Color = plotlyRV$colors[rev(levels(plotlyRV$p$name))])

    if(!is.null(inter) & !is.null(input$plot_hover)){
      hover=input$plot_hover
      if(round(hover$x) >= min(as.numeric(plotlyRV$p$x)) & round(hover$x) <= max(as.numeric(plotlyRV$p$x))){
        inter <- inter %>%
          left_join(plotlyRV$p %>% filter(as.numeric(x) == round(hover$x)), by = "name")
      } else {
        inter <- inter %>%
          mutate(y = "")
      }
    } else {
      inter <- inter %>%
        mutate(y = "")
    }

    inter <- inter %>%
      left_join(
        plotlyRV$p %>% filter(plotlyRV$p$x >= plotlyRV$xlim[1],
                              plotlyRV$p$x <= plotlyRV$xlim[2]) %>%
          group_by(name) %>%
          summarize(mean_focus = mean(as.numeric(y), na.rm = TRUE),
                    max_focus = max(as.numeric(y), na.rm = TRUE),
                    min_focus = min(as.numeric(y), na.rm = TRUE),
                    options(dplyr.summarise.inform=F) )%>%
          arrange(match(name, rev(levels(plotlyRV$p$name)))) %>%
          select(name, min_focus, mean_focus, max_focus ),
        by = "name") %>%
      mutate(dif_mean_focus = Mean - mean_focus)

    if(!is.null(input$plot_brush)){

      brush=input$plot_brush

      if(nrow(plotlyRV$p %>%
              filter(plotlyRV$p$x >= brush$xmin,
                     plotlyRV$p$x <= brush$xmax) %>%
              group_by(name) ) != 0){

        inter <- inter %>%
          left_join(
            plotlyRV$p %>% filter(plotlyRV$p$x >= brush$xmin,
                                  plotlyRV$p$x <= brush$xmax) %>%
              group_by(name) %>%
              summarize(mean_brush = mean(as.numeric(y), na.rm = TRUE),
                        max_brush = max(as.numeric(y), na.rm = TRUE),
                        min_brush = min(as.numeric(y), na.rm = TRUE),
                        options(dplyr.summarise.inform=F) )%>%
              arrange(match(name, rev(levels(plotlyRV$p$name)))) %>%
              select(name, min_brush, mean_brush, max_brush ),
            by = "name") %>%
          mutate(dif_mean = Mean - mean_brush)
      } else {
        inter <- inter %>%
          mutate(min_brush= NA, mean_brush= NA, max_brush = NA, dif_mean= NA)
      }
    } else {
      inter <- inter %>%
        mutate(min_brush= NA, mean_brush= NA, max_brush = NA, dif_mean= NA)
    }


    inter = inter  %>%
      rename("Strains/Species" = name,
             "Hover" = y) %>%
      select("Color","Strains/Species",  "Min", "1st Qu.", "Median",
             "Mean", "3rd Qu.", "Max",  "Hover",
             min_focus, mean_focus, max_focus, dif_mean_focus,
             min_brush, mean_brush, max_brush, dif_mean)

    RMD$dt <- inter
    if(is.null(plotlyRV$selectedRow)){
      plotlyRV$selectedRow = as.data.frame(RMD$dt)[,2]
    }

    #---------------------------------------------------------------------------
    # sketch
    #---------------------------------------------------------------------------

    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      textHover = HTML(paste0("Hover <br>(pos. : ", round(hover$x), ")"))
    } else {
      textHover = "Hover"
    }

    if(!is.null(input$plot_brush)){
      brush=input$plot_brush
      textBrush = HTML(paste0("Selected area <br>Start : ",round(brush$xmin)," - End ",round(brush$xmax)," - Length : ", round(brush$xmax - brush$xmin)))
    } else {
      textBrush = "Selected area"
    }

    textFocus = HTML(paste0("Focus area <br>Start : ",round(plotlyRV$xlim[1])," - End ",round(plotlyRV$xlim[2])," - Length : ", round(plotlyRV$xlim[2] - plotlyRV$xlim[1])))

    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, ''),
          th(rowspan = 2, 'Strains/Species'),
          th(colspan = 6, 'Summary'),
          th(rowspan = 2, textHover),
          th(colspan = 4,textFocus),
          th(colspan = 4,textBrush)
        ),
        tr(
          lapply(c("Min", "1st Qu.", "Median",
                   "Mean", "3rd Qu.", "Max",
                   "Min", "Mean", "Max", "Mean diff",
                   "Min", "Mean", "Max", "Mean diff"), th)
        )
      ) ,

      tableFooter(c("", lapply(1:(ncol(inter)-1), function(i, arg2){


        if(i == 1 | all(is.na(as.data.frame(arg2)[, (i+1)])) | all(as.data.frame(arg2)[, (i+1)] == "" )){
          ""
        } else {
          actionButton(
            paste0("btn_",i),
            "plot",
            onclick='Shiny.onInputChange("select_button",  this.id)'
          )
        }

      }, arg2=inter)))
    ))
    RMD$sketch <- sketch

    #---------------------------------------------------------------------------
    # DT
    #---------------------------------------------------------------------------

    DT::datatable(inter,
                  rownames = FALSE,
                  filter = 'top',
                  container = sketch,
                  colnames = c('', colnames(inter)[-1]),
                  escape = F,
                  extensions = 'FixedColumns',
                  selection = "none",
                  options = list(
                    autoWidth = TRUE,
                    dom = 't',
                    scrollY = "400px",
                    columnDefs = list(list(searchable = FALSE, targets = 0),
                                      list(className = 'dt-center', targets = "_all")
                                      ),
                    paging=FALSE,  processing=FALSE,
                    scrollX = "600px",
                    fixedColumns = list(leftColumns= 2)),
    ) %>%
      formatStyle(
        'Color',
        width='20px',
        `font-size` ="1px",
        color = styleEqual(
          inter$Color, inter$Color
        ),
        backgroundColor = styleEqual(
          inter$Color, inter$Color
        )
      ) %>% formatRound(3:ncol(inter), 2)

  } else {
    NULL
  }
})


observeEvent(input$selectCB, {
  plotlyRV$selectedRow = as.data.frame(RMD$dt)[input$pipExplo_rows_all,2]
})

#===============================================================================
# Plot by column
#===============================================================================

observeEvent(input$select_button, {
  selectCol <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
  showModal(modalDialog(
    title = "Data visualization",
    footer = modalButton("Close"),
    size = "l",
    easyClose = T,
    radioButtons("rbModal", label = "Representation type", choices = c("violin","density", "histogram" ), inline = T),
    plotOutput(outputId = "plotModal")
  )
  )

  output$plotModal <- renderPlot({

    if(input$rbModal == "density"){
      ggplot( mapping=aes(x=as.data.frame(RMD$dt)[, (selectCol+1)]))+
        geom_density(fill = "seashell", color = "seashell") +
        stat_density(geom = "line", size = 1) + xlab("Values") +
        theme_bw() + theme(axis.title = element_text(size = 16))
    } else if(input$rbModal == "histogram" ){
      ggplot(mapping=aes(x=as.data.frame(RMD$dt)[, (selectCol+1)])) +
        geom_histogram(bins = 30)  + xlab("Values") +
        theme_bw() + theme(axis.title = element_text(size = 16))
    }  else if(input$rbModal == "violin" ){
      ggplot(mapping=aes(y=as.data.frame(RMD$dt)[, (selectCol+1)], x = "")) +
        geom_violin( fill='white', color="#3c8dbc") + ylab("Values") +
        xlab("") + geom_dotplot(binaxis='y', stackdir='center', dotsize=1, fill="#3c8dbc") +
        theme_bw() + theme(axis.title = element_text(size = 16))
    }else {
      NULL
    }

  })

})

#===============================================================================
# Hover annotation information
#===============================================================================

output$hover_info_annot <- renderUI({
  if(!is.null(input$plot_hover) & !is.null(plotlyRV$annotationTable)){
    hover=input$plot_hover
    HTML(paste(plotlyRV$annotationTable %>%
                 filter(as.numeric(plotlyRV$annotationTable$start) <= round(hover$x) & as.numeric(plotlyRV$annotationTable$end) >= round(hover$x)) %>%
                 pull(attributes), collapse = "<br><br>"))
  }
})


#===============================================================================
# Awaiting validation
#===============================================================================

# output$hover_info <- renderUI({
#   emptyTemp = HTML(paste("<table style='width: 100%;'><thead><tr><th>Species</th><th>PIP</th></tr></thead><tbody>",
#                          paste(plotlyRV$p %>%
#                                  select(name) %>%
#                                  distinct(name) %>%
#                                  arrange(match(name, rev(levels(plotlyRV$p$name)))) %>%
#                                  mutate(color = plotlyRV$colors[rev(levels(plotlyRV$p$name))],
#                                         printText = paste0("<tr><td><div style='float: left; margin: 3px 5px; width: 20px; height:12px;background-color:",color,"'></div>",name,"</td><td></td><tr>")
#                                  ) %>%
#                                  pull (printText), collapse = ""),
#                          "</tbody></table>", collapse=""))
#
#
#
#   if(!is.null(input$plot_hover)){
#     hover=input$plot_hover
#
#     if(round(hover$x) >= min(as.numeric(plotlyRV$p$x)) & round(hover$x) <= max(as.numeric(plotlyRV$p$x))){
#
#       HTML(paste("<table style='width: 100%;'><thead><tr><th>Species</th><th>PIP</th></tr></thead><tbody>",
#                  paste(plotlyRV$p %>%
#                          select(name) %>%
#                          distinct(name) %>%
#                          arrange(match(name, rev(levels(plotlyRV$p$name)))) %>%
#                          mutate(color = plotlyRV$colors[rev(levels(plotlyRV$p$name))]) %>%
#                          left_join(plotlyRV$p %>% filter(as.numeric(x) == round(hover$x)), by = "name") %>%
#                          mutate(printText = paste0("<tr><td><div style='float: left; margin: 3px 5px; width: 20px; height:12px;background-color:",color,"'></div>",name,"</td><td style='text-align:center'>",round(as.numeric(y), 1),"</td><tr>")
#                          ) %>%
#                          pull (printText), collapse = ""),
#                  "</tbody></table>", collapse=""))
#
#     } else {
#       emptyTemp
#     }
#
#   } else {
#
#     emptyTemp
#   }

# or
# if(!is.null(input$plot_hover)){
#   hover=input$plot_hover
#   p(paste0("Hover position : ", round(hover$x)))
# } else {
#   p("Hover position : ")
# }
# })




# output$brush_info <- renderUI({
#   if(!is.null(input$plot_brush)){
#     brush=input$plot_brush
#
#     if(nrow(plotlyRV$p %>%
#             filter(plotlyRV$p$x >= brush$xmin,
#                    plotlyRV$p$x <= brush$xmax) %>%
#             group_by(name) ) != 0){
#       HTML(paste("<b>Start </b>: " , round(brush$xmin), " - <b>End </b>: ", round(brush$xmax)," - Length </b>: ",   round(brush$xmax - brush$xmin),
#                  "<table style='width: 100%;'><thead><tr><th>Species</th><th style='text-align:center'>Mean PIP</th></tr></thead><tbody>",
#                  paste(plotlyRV$p %>%
#                          filter(plotlyRV$p$x >= brush$xmin,
#                                 plotlyRV$p$x <= brush$xmax) %>%
#                          group_by(name) %>%
#                          summarize(mean_size = mean(as.numeric(y), na.rm = TRUE)) %>%
#                          arrange(match(name, rev(levels(plotlyRV$p$name)))) %>%
#                          mutate(color = plotlyRV$colors[rev(levels(plotlyRV$p$name))],
#                                 printText = paste0("<tr><td><div style='float: left; margin: 3px 5px; width: 20px; height:12px;background-color:",color,"'></div>",name,"</td><td style='text-align:center'>",round(mean_size, 1),"</td><tr>")
#                          ) %>%
#                          pull (printText), collapse = ""),
#                  "</tbody></table>", collapse=""))
#     } else {
#       NULL
#     }
#
#   } else {
#     NULL
#   }


# Or
# if(!is.null(input$plot_brush)){
#   brush=input$plot_brush
#   p(paste0("Start area position : ",round(brush$xmin)," - End area position : ",round(brush$xmax)," - Length : ", round(brush$xmax - brush$xmin)))
# } else {
#   tagList()
#   p("Start area position :  - End area position :  - Length : ")
# }
# })

################################################################################
# Update
################################################################################


observeEvent(input$speciesColor_name, {
  updateColourInput(session, "speciesColor_picker",
                    value = as.character(plotlyRV$colors[input$speciesColor_name]))
})

observeEvent(rvAnnotation$annotation, {
  if(!is.null(rvAnnotation$annotation)){
    updateSelectInput(session, "geneExplore_selector",
                      choices = c(All = "All", setNames(rvAnnotation$feature$featureName,
                                                        rvAnnotation$feature$featureName)),
                      selected = "All"
    )
  }
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
    plotlyRV$xlim = sort(c(min(as.numeric(rvAnnotation$feature %>% filter(featureName == input$geneExplore_selector) %>% pull(start))),
                           max(as.numeric(rvAnnotation$feature %>% filter(featureName == input$geneExplore_selector) %>% pull(end)))))
  }

  updateSliderInput(session, "xlimRange",
                    min = 0, max = length(plotlyRV$refPositions),
                    value = c(plotlyRV$xlim[1], plotlyRV$xlim[2])
  )
})

observeEvent(input$updateColor, {
  plotlyRV$colors[input$speciesColor_name] = input$speciesColor_picker
})

observeEvent(input$updateGrid, {
  plotlyRV$colMinorX = input$colMinorX
  plotlyRV$sizeMinorX = input$sizeMinorX
  plotlyRV$colMajorX = input$colMajorX
  plotlyRV$sizeMajorX = input$sizeMajorX
  plotlyRV$colMinorY = input$colMinorY
  plotlyRV$sizeMinorY = input$sizeMinorY
  plotlyRV$colMajorY = input$colMajorY
  plotlyRV$sizeMajorY = input$sizeMajorY

  plotlyRV$spaceMajorX = input$spaceMajorX
  plotlyRV$spaceMinorX = input$spaceMinorX
  plotlyRV$spaceMajorY = input$spaceMajorY
  plotlyRV$spaceMinorY = input$spaceMinorY
})



################################################################################
# Generate plot - PIP profile
################################################################################

observeEvent(genomes$genomesNto1$alignments, {

  if(!is.null(genomes$genomesNto1$alignments)) {
    if(input$dataset == "rdata" | (input$dataset ==  "demo" & input$demoType == "rdata")){

      updateSliderInput(session, "xlimRange",
                        min = 0, max = length(plotlyRV$refPositions),
                        value = c(plotlyRV$xlim[1], plotlyRV$xlim[2])
      )

      updateSliderInput(session, "ylimRange",
                        min = 0, max = 120,
                        value = c(plotlyRV$ylim[1], plotlyRV$ylim[2])
      )
      updateTextInput(session, "titleInput_main", value = plotlyRV$title_main)
      updateTextInput(session, "titleInput_x", value =  plotlyRV$title_x )
      updateTextInput(session, "titleInput_y", value = plotlyRV$title_y)
      updateTextInput(session, "titleInput_legende", value = plotlyRV$title_legende)

      updateSelectizeInput(session, "speciesColor_name",
                           choices =  setNames(names(plotlyRV$colors),
                                               names(plotlyRV$colors)),
                           selected = names(plotlyRV$colors)[1])

      updateColourInput(session, "speciesColor_picker",
                        value = as.character(plotlyRV$colors[names(plotlyRV$colors)[1]]))

      updateColourInput(session, "colMinorX",value = plotlyRV$colMinorX)
      updateColourInput(session, "colMajorX",value = plotlyRV$colMajorX)
      updateColourInput(session, "colMinorY",value = plotlyRV$colMinorY)
      updateColourInput(session, "colMajorY",value = plotlyRV$colMajorY )

      updateNumericInput(session, "sizeMinorX", value = plotlyRV$sizeMinorX)
      updateNumericInput(session, "sizeMajorX", value = plotlyRV$sizeMajorX)
      updateNumericInput(session, "sizeMinorY", value = plotlyRV$sizeMinorY)
      updateNumericInput(session, "sizeMajorY", value = plotlyRV$sizeMajorY)

      updateNumericInput(session, "spaceMajorX", value = plotlyRV$spaceMajorX)
      updateNumericInput(session, "spaceMinorX", value = plotlyRV$spaceMinorX)
      updateNumericInput(session, "spaceMajorY", value = plotlyRV$spaceMajorY)
      updateNumericInput(session, "spaceMinorY", value = plotlyRV$spaceMinorY)

      updateNumericInput(session, "windowSize", value = plotlyRV$windowSize)


    } else {
      createPIPprofile(keepColor = F)
    }
  }
})


observeEvent({
  input$updateGeneral
}, {
  if(!is.null(genomes$genomesNto1$alignments)) {
    createPIPprofile(keepColor = T)
  }
})

createPIPprofile <- function(keepColor){
  #=========================================================================
  # Variables
  #=========================================================================

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
  seqColors = NULL

  ## Plot PIP profiles

  withProgress(message = 'PIP profile generation', value = 0, {

    i <- 1
    for (j in length(alignments):1) {
      incProgress(1/length(genomes$genomesNto1$alignments),
                  detail  = subject)

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
      meanPIPs <- c(meanPIPs, setNames(meanPIP,paste0(subject, " (", round(digits = 1, meanPIP), "%)")))


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

      seqColors = c(seqColors, seqColor)

      ggplotTable = rbind.data.frame(
        ggplotTable,
        cbind.data.frame(x = pipStart:pipEnd,
                         y = pipProfile[pipStart:pipEnd],
                         name = paste0(subject, " (", round(digits = 1, meanPIP), "%)"))
      )
    }
  })

  ggplotTable$name = factor(ggplotTable$name, names(meanPIPs)[order(meanPIPs)])

  #===========================================================================
  # Reactive values
  #===========================================================================

  plotlyRV$p = ggplotTable

  if(keepColor == F){
    plotlyRV$colors = setNames(seqColors,
                               unique(plotlyRV$p$name))
  }

  rm(ggplotTable)

  plotlyRV$title_main = paste0("Full PIP profile - Ref: ", genomes$refGenomeName)
  plotlyRV$title_x = "Position "
  plotlyRV$title_y = paste0("PIP (", plotlyRV$windowSize," bp-averaged)")
  plotlyRV$title_legende = 'Species'

  plotlyRV$xlim = c(0,length(plotlyRV$refPositions))
  plotlyRV$ylim = c(0,100)

  plotlyRV$colMinorX = input$colMinorX
  plotlyRV$sizeMinorX = input$sizeMinorX
  plotlyRV$colMajorX = input$colMajorX
  plotlyRV$sizeMajorX = input$sizeMajorX
  plotlyRV$colMinorY = input$colMinorY
  plotlyRV$sizeMinorY = input$sizeMinorY
  plotlyRV$colMajorY = input$colMajorY
  plotlyRV$sizeMajorY = input$sizeMajorY

  if(length(plotlyRV$refPositions) < 5000) {
    plotlyRV$spaceMajorX = 500
    plotlyRV$spaceMinorX = 100
  } else {
    plotlyRV$spaceMajorX = 5000
    plotlyRV$spaceMinorX = 1000
  }

  plotlyRV$spaceMajorY = input$spaceMajorY
  plotlyRV$spaceMinorY = input$spaceMinorY

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

  updateNumericInput(session, "spaceMajorX", value = plotlyRV$spaceMajorX)
  updateNumericInput(session, "spaceMinorX", value = plotlyRV$spaceMinorX)
  updateNumericInput(session, "spaceMajorY", value = plotlyRV$spaceMajorY)
  updateNumericInput(session, "spaceMinorY", value = plotlyRV$spaceMinorY)

  updateSelectizeInput(session, "speciesColor_name",
                       choices =  setNames(names(plotlyRV$colors),
                                           names(plotlyRV$colors)),
                       selected = names(plotlyRV$colors)[1])
}

################################################################################
# Information - PIP statistics
################################################################################

output$pipStat <-  renderDataTable({
  if(!is.null(plotlyRV$p)){
    inter <- plotlyRV$p %>%
      group_by(name) %>%
      summarize(
        "Min" = round(min(as.numeric(y), na.rm = TRUE),2),
        "1st Quantile" = round(quantile(as.numeric(y), na.rm = TRUE)[2],2),
        "Median" = round(median(as.numeric(y), na.rm = TRUE),2),
        "Mean" = round(mean(as.numeric(y), na.rm = TRUE),2),
        "3rd Quantile" = round(quantile(as.numeric(y), na.rm = TRUE)[4],2),
        "Max"  = round(max(as.numeric(y), na.rm = TRUE),2),
        options(dplyr.summarise.inform=F) ) %>%
      arrange(match(name, rev(levels(plotlyRV$p$name)))) %>%
      mutate(Color = plotlyRV$colors[rev(levels(plotlyRV$p$name))]) %>%
      rename("Strains/Species" = name) %>%
      select("Color","Strains/Species", "Color", "Min", "1st Quantile", "Median",
             "Mean", "3rd Quantile", "Max")

    datatable(inter, rownames = FALSE, colnames = c('', colnames(inter)[-1]),
              selection = 'none',options = list(scrollX = TRUE)) %>%
      formatStyle(
        'Color',
        width='20px',
        `font-size` ="1px",
        color = styleEqual(
          inter$Color, inter$Color
        ),
        backgroundColor = styleEqual(
          inter$Color, inter$Color
        )
      )

  } else {
    NULL
  }
})


################################################################################
# Download plot
################################################################################

output$downloadPlot <- downloadHandler(
  filename = function(){paste0("FullPIP",'.', input$ggsave_format)},
  content = function(file){
    ggsave(file, plot=plotlyRV$plotGG,
           device = , as.character(input$ggsave_format),
           width = as.numeric(input$ggsave_width),
           height = as.numeric(input$ggsave_height),
           units = as.character(input$ggsave_unit),
           dpi = as.numeric(input$ggsave_dpi))
  }
)
