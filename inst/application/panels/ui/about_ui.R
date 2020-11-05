about <- fluidPage(
  h1("Session Information"), 
  helpText("In this section is gathered all the information concerning 
                       the working environment, the versions of the packages used, ... 
                       to be able to reproduce the analyses. ", style = "text-align: justify;"),
  h2("R session information and parameters"),
  p("The versions of the R software and Bioconductor packages used for this analysis are listed below. 
                It is important to save them if one wants to re-perform the analysis in the same conditions."),
  uiOutput("sessionText")
)