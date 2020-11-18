observeEvent(input$goImportSequence, {
  updateTabItems(session, "tabs", selected = "importSequences")
})
