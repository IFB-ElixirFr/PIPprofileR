helpPage <- fluidPage(
  h1("Help"),
  h2("Documentations"),

  p("To guide you in the use of the application, documentation is available ", a("here", href="https://github.com/IFB-ElixirFr/PIPprofileR/wiki", target="_blank")),

  h2("Contact us"),
  p("Questions, problems or comments ? Contact us !"),

  widgetUserBox(
    title = "Thomas Denecker",
    subtitle = "Lead Developer",
    type = NULL,
    src = "img/TD.jpg",
    color = "light-blue",
    width = 4,
    collapsible = FALSE,
    collapsed = FALSE,
    closable = FALSE,
    style = "border : solid 1px #3c8dbc!important ; box-shadow:0px ;",
    div( style="text-align:center; margin-top :37px ; font-size: 35px;",

         shiny::a(icon("orcid"),
                  href="https://orcid.org/0000-0003-1421-7641",
                  target="_blank",
                  style="display: inline-block; margin: 0 5px;"),

         shiny::a(icon("github"),
                  href="https://github.com/thomasdenecker",
                  target="_blank",
                  style="display: inline-block; margin: 0 5px;"),

         shiny::a(icon("envelope"),
                  href="mailto: thomas.denecker@gmail.com",
                  target="_blank",
                  style="display: inline-block; margin: 0 5px;" )
    )
  ),
  widgetUserBox(
    title = "Jacques van Helden",
    subtitle = "Lead project",
    type = NULL,
    src = "img/JVH.jpg",
    color = "light-blue",
    footer = NULL,
    footer_padding = TRUE,
    width = 4,
    collapsible = FALSE,
    collapsed = FALSE,
    closable = FALSE,
    style = "border : solid 1px #3c8dbc!important ; box-shadow:0px ;",
    div( style="text-align:center; margin-top :37px ; font-size: 35px;",

         shiny::a(icon("orcid"),
                  href="https://orcid.org/0000-0002-8799-8584",
                  target="_blank",
                  style="display: inline-block; margin: 0 5px;"),

         shiny::a(icon("github"),
                  href="https://github.com/jvanheld",
                  target="_blank",
                  style="display: inline-block; margin: 0 5px;"),

         shiny::a(icon("envelope"),
                  href="mailto: Jacques.van-Helden@france-bioinformatique.fr",
                  target="_blank",
                  style="display: inline-block; margin: 0 5px;" )
    )
  ),
  widgetUserBox(
    title = "Hélène Chiapello",
    subtitle = "Lead project",
    type = NULL,
    src = "img/HC.png",
    color = "light-blue",
    width = 4,
    collapsible = FALSE,
    collapsed = FALSE,
    closable = FALSE,
    style = "border : solid 1px #3c8dbc!important ; box-shadow:0px ;",
    div( style="text-align:center; margin-top :37px ; font-size: 35px;",

         shiny::a(icon("orcid"),
                  href="https://orcid.org/0000-0001-5102-0632",
                  target="_blank",
                  style="display: inline-block; margin: 0 5px;"),

         shiny::a(icon("github"),
                  href="https://github.com/hchiapello",
                  target="_blank",
                  style="display: inline-block; margin: 0 5px;"),

         shiny::a(icon("envelope"),
                  href="mailto: helene.chiapello@inrae.fr",
                  target="_blank",
                  style="display: inline-block; margin: 0 5px;" )
    )
  )
)


