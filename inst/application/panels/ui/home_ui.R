home <- fluidPage(
  HTML('<div class="container">'),
  img(src="img/logo.svg",style= "height:300px; display: block; margin-left: auto; margin-right: auto" ),
  h3("PIPprofileR is a tool to easily generate Profiles of Percent Identical Positions from a fasta file (nucleotide or peptide sequences).
    With the help of an annotation file, it will be possible to simply explore the results."),

  h4("How to use ?"),
  p("1- Firstly, you should select a demo dataset, upload a FASTA file or a Rdata file (previous analysis)."),
  p("2- [not mandatory] Add an annotation file to enrich the exploration of the PIP profile or refine the search during alignments."),
  p("3- Align sequence (if you have imported a fasta file) and explore PIP profile."),

  HTML('</div>')



)
