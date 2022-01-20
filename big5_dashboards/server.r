shinyServer(function(input, output, session) {
  # [Bump Plot] - Plot Output ----
  output$bumpPlot <- renderGirafe({
    bump_df <- get_league_bumpData(input$Competition)
    teams <- unique(bump_df$Team)
    
    validate(
      need(input$md_range[2] <= get_league_maxMD(input$Competition), "")
    )

    girafe(
      ggobj = get_bumpPlot(bump_df, teams, input$Teams, input$md_range[1], input$md_range[2], input$bumpRank,
                           substr(input$back_color, 1, 7), get_leaguePalette(input$Competition)),
      width_svg = 35, height_svg = 16,
      options = list(
        opts_hover_inv(css = "opacity:0.1;"),
        opts_hover(css = "stroke-width:2;"),
        opts_selection(type = "none")
      )
    )
  })
  
  # [Bump Plot] - Updating Team Input Options based off Competition Input ----
  observe({
    val <- input$Competition
    updateSelectizeInput(
      session,
      "Teams",
      "Teams",
      choices = get_team_choices(input$Competition),
      options = list(
        placeholder = 'Select team(s) below',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })
  # [Bump Plot] - Resetting Team Input(s) ----
  observeEvent(
    input$resetBumpTeams,
    updateSelectizeInput(session,
                         "Teams",
                         "Teams",
                         choices = get_team_choices(input$Competition),
                         options = list(
                           placeholder = 'Select team(s) below',
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
  )
  
  
  # [Bump Plot] - Updating Slider MD Range based off Competition Input ----
  observe({
    val <- input$Competition
    updateSliderInput(session,
                      "md_range",
                      min = 1,
                      max = get_league_maxMD(val),
                      value = c(1, get_league_maxMD(val)),
                      step = 1
                      )
    })
  # [Bump Plot] - Resetting Slider MD Range ----
  observeEvent(
    input$resetBumpRange,
    updateSliderInput(session,
                      "md_range",
                      min = 1,
                      max = tail(big5_match_data[big5_match_data$Competition_Name == input$Competition & !is.na(big5_match_data$Home_xG),]$Wk, 1),
                      value = c(1, tail(big5_match_data[big5_match_data$Competition_Name == input$Competition & !is.na(big5_match_data$Home_xG),]$Wk, 1)),
                      step = 1
    )
  )

  
  # ------------------------------------------------------------------------

  # [GK Zone] - Plot Output ---- 
  output$gkPlot <- renderGirafe({
    gk_data <- get_plKeeper_adv(gk_data, gk_data_adv)
    
    if (input$gk_viz == "Who's Beating the Model?") {
      girafe(
        ggobj = gk_model_plot(gk_data),
        width_svg = 9, height_svg = 5.5,
        options = list(
          opts_tooltip(use_fill = TRUE),
          opts_hover_inv(css = "opacity:0.5;")
          )
        )
    } else if (input$gk_viz == "Getting Out of the Box") {
        girafe(
          ggobj = gk_sweeper_plot(gk_data),
          width_svg = 9, height_svg = 5.5,
          options = list(
            opts_tooltip(use_fill = TRUE),
            opts_hover_inv(css = "opacity:0.5;")
          )
        )
    } else if (input$gk_viz == "Are Crosses Scary?") {
        girafe(
          ggobj = gk_cross_plot(gk_data),
          width_svg = 9, height_svg = 5.5,
          options = list(
            opts_tooltip(use_fill = TRUE),
            opts_hover_inv(css = "opacity:0.5;")
          )
        )
    } else if (input$gk_viz == "Building From the Back") {
        
      }
    
    
    
    })
  # [GK Zone] - Text Output ----
  output$gk_text <- renderText(
    gk_model_text
  )
  # ------------------------------------------------------------------------
})