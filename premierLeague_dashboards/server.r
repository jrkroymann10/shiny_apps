shinyServer(function(input, output, session) {
  output$bumpPlot <- renderPlot({
    bump_df <- get_bumpData(match_data)
    teams <- unique(bump_df$Team)
    get_bumpPlot(bump_df, teams, input$Team, input$md_range[1], input$md_range[2])
    })
  
  output$gkPlot <- renderPlotly({
    gk_data <- get_plKeeper_adv(gk_data, gk_data_adv)
    gk_model_plot(pl_keepers_adv) %>%
      layout(title = list(text = paste0("Who's Beating the Model? (And Does it Matter?)",
                                        "<br>",
                                        "<sup>",
                                        "PSxG - Goals Allowed per 90 by Shots on Target for PL Goalkeepers with > 900 minutes played",
                                        "</sup>")))
    })
})