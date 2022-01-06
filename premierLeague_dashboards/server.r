shinyServer(function(input, output, session) {
  output$bumpPlot <- renderPlot({
    bump_df <- get_bumpData(match_data)
    teams <- unique(bump_df$Team)
    get_bumpPlot(bump_df, teams, input$Team, input$md_range[1], input$md_range[2])
  },
  
  width = 1250,
  height = 450)
})