function(input, output, session) {
  # [UI] - Query Selection
  output$table_input <- renderUI({
    selectizeInput(
      inputId = "table_input",
      label = "Table Selection",
      choices = sort(c("book", "cat", "location", "session", "reader")),
      multiple = FALSE,
      options = list(
        placeholder = 'Select a Table to View',
        onInitialize = I('function() { this.setValue(""); }')
        )
    )
  })
  
  # [Database Output] - Reactive Data Based on Query Selected
  table_output <- reactive({
    req(input$table_input)
    dbFetch(dbSendQuery(db, return_table_query(input$table_input)))
  })

  # [Database Output] - In Table (Reactable) Form
  output$table_output <- renderReactable({
    req(table_output())
    reactable(table_output())
  })
}