home <- fluidPage(
  h1("Home"), 
  p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam a mauris id 
    nulla elementum mattis a sit amet tortor. Nullam eu nisl ex. Curabitur 
    pretium dui eu ante lobortis porta. Donec ut orci ut orci ornare tempor.
    Vivamus nunc velit, pretium sed ante non, consequat rutrum elit. Cras in 
    neque luctus, ornare nisi sit amet, faucibus tellus. Fusce tincidunt semper 
    purus, interdum aliquam nulla. Pellentesque bibendum leo ut rutrum rutrum. 
    Cras arcu augue, rutrum vel scelerisque quis, aliquet quis velit. Donec 
    molestie vitae metus nec molestie. Nullam purus massa, placerat non sagittis 
    eget, lobortis at elit. Ut et justo risus. Etiam elit elit, lobortis ut justo 
    ac, vehicula imperdiet ante. Maecenas ut felis congue, eleifend nulla eget, 
    elementum arcu. Sed malesuada ante nulla, eget pellentesque diam egestas vitae."),
  
  fluidRow( style="display: flex; text-align:center;", 
    column(6,style = "border : 2px #3c8dbc solid ; margin : 5px; padding : 0px 0px 20px 0px; ", 
           h2("Import your sequences", style = "margin : 0px 0px 15px 0px"),
           helpText("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam a mauris id 
    nulla elementum mattis a sit amet tortor. Nullam eu nisl ex. "), 
           actionButton("dataButton", 
                        "Select file", 
                        icon = icon("upload"),
                        width = 200
           )),
    column(6, style = "border : 2px #3c8dbc solid ; margin : 5px; padding :0px 0px 20px 0px; ", 
           h2("Import annotation", style = "margin : 0px 0px 15px 0px"),
           helpText("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam a mauris id 
    nulla elementum mattis a sit amet tortor. Nullam eu nisl ex. que luctus, ornare nisi sit amet, faucibus tellus. Fusce tincidunt semper 
    purus, interdum aliquam nulla. Pellentesque bibendum leo ut rutrum rutrum. 
    Cras arcu augue, rutrum vel scelerisque quis, aliquet quis velit. Donec 
    molestie vitae metus nec mole"), 
           actionButton("annotButton", 
                        "Select file", 
                        icon = icon("upload"),
                        width = 200
           ))
  )
)