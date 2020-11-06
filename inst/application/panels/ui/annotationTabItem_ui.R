annotationTabItem  <- fluidPage(
  h1("Annotation"),
  h2("Sub-selection of the annotation file for exploration"),

  fluidRow(
    column(3,
           selectizeInput("typeSelector", "Select type",
                          width = "100%", choices = NULL,
                          selected = NULL, multiple = FALSE)
    ),
    column(3,
           selectizeInput("nameAttributesSelector", "Select the attribute name",
                          width = "100%", choices = NULL,
                          selected = NULL, multiple = FALSE)
    ),
    column(6,
           uiOutput("exampleNameExtract")
    )
  ),

  h2("All annotations"),
  withLoader(dataTableOutput(outputId = "annotationTable"))
)
