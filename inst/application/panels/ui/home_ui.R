home <- fluidPage(
  HTML('<div class="container">'),
  img(src="img/logo.svg",style= "height:300px; display: block; margin-left: auto; margin-right: auto" ),
  h3("PIPprofileR is a tool to easily generate Profiles of Percent Identical Positions from a fasta file (nucleotide or peptide sequences).
    With the help of an annotation file, it will be possible to simply explore the results."),

  fluidRow( style="display: flex; text-align:center;",
    column(6,style = "border : 2px #3c8dbc solid ; margin : 5px; padding : 0px 0px 20px 0px; ",
           h2("Import your sequences", style = "margin : 0px 0px 15px 0px"),
           helpText("Firstly, you should select a demo dataset, upload a FASTA file or a Rdata file (previous analysis)."),
           actionButton("dataButton",
                        "Select file",
                        icon = icon("upload"),
                        width = 200
           )),
    column(6, style = "border : 2px #3c8dbc solid ; margin : 5px; padding :0px 0px 20px 0px; ",
           h2("Import annotation", style = "margin : 0px 0px 15px 0px"),
           helpText("You can add an annotation file to enrich the exploration of the PIP profile or refine the search during alignments."),
           actionButton("annotButton",
                        "Select file",
                        icon = icon("upload"),
                        width = 200
           ))
  ),
  HTML('</div>')
)
