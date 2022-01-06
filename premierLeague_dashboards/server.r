shinyServer(function(input, output, session) {
  output$plot <- renderPlot({
    prem_2021 <- transform_matchResults(match_data)
    last_week <- find_lastWeek(prem_2021)
    bump_df <- fill_df(prem_2021, last_week)
    
    # setting data type of multiple columns to numeric
    bump_df$Games_Played <- as.numeric(bump_df$Games_Played)
    bump_df$Points <- as.numeric(bump_df$Points)
    bump_df$Total_Points <- as.numeric(bump_df$Total_Points)
    bump_df$Matchday <- as.numeric(bump_df$Matchday)
    bump_df$GD <- as.numeric(bump_df$GD)
    
    teams <- unique(bump_df$Team)
    bump_df <- fill_pointsAndGd(bump_df, teams)
    bump_df <- add_rank(bump_df)
    bump_df <- shorten_teamnames(bump_df)
    
    teams <- unique(bump_df$Team)
    get_bumpPlot(bump_df, teams, input$Team, input$Start_MD, input$End_MD)
  },
  
  width = 1250,
  height = 450)
})