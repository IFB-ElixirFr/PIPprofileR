home <- fluidPage(

  fluidRow(style="display: flex; align-items: center;",
    column(3,
           img(src="img/logo.svg",style= "height:300px; display: block;
               margin-left: auto; margin-right: auto" )
    ),
    column(9,
           h1("PIPprofileR"),
           h3("A tool to easily generate Profiles of Percent
           Identical Positions from a fasta file (nucleotide or peptide sequences).
    With the help of an annotation file, it will be possible to simply explore
              the results."),

           h4("Why ?"),
           p("PIP profiles are widely used by virologists to detect recombination
             events and can be found in most publications concerning the origins of SARS-CoV-2."),

           h4("How to use ?"),
           p("1- Firstly, you should select a demo dataset, upload a FASTA
             file or a Rdata file (previous analysis)."),
           p("2- [not mandatory] Add an annotation file to enrich the exploration
             of the PIP profile or refine the search during alignments."),
           p("3- Align sequence (if you have imported a fasta file) and explore
             PIP profile."),

           h4("Use cases"),

           p("- Virology researchers who wish to compare genomic or protein
             sequences from different viral strains. A example is available",
             a("here","https://doi.org/10.1051/medsci/2020123")),
           p("- Teachers/trainers in the context of biological sequence
                     analysis TP. A example is available",
             a("here","https://jvanheld.github.io/shnc-origines-sars-cov-2/")),

           h4("Publication"),
           p("Sallard, E., Halloy, J., Casane, D., van Helden, J. & Decroly, É. 2020.
             Retrouver les origines du SARS-CoV-2 dans les phylogénies de coronavirus. Med Sci (Paris) 36: 783–796.
             ", a("https://doi.org/10.1051/medsci/2020123"))
    )
  )
  # HTML('<div class="container">'),
  # # img(src="img/logo.svg",style= "height:300px; display: block; margin-left: auto; margin-right: auto" ),
  # # h3("PIPprofileR is a tool to easily generate Profiles of Percent Identical Positions from a fasta file (nucleotide or peptide sequences).
  # #   With the help of an annotation file, it will be possible to simply explore the results."),
  # #
  # # h4("How to use ?"),
  # # p("1- Firstly, you should select a demo dataset, upload a FASTA file or a Rdata file (previous analysis)."),
  # # p("2- [not mandatory] Add an annotation file to enrich the exploration of the PIP profile or refine the search during alignments."),
  # # p("3- Align sequence (if you have imported a fasta file) and explore PIP profile."),
  #
  # HTML('</div>')
)
