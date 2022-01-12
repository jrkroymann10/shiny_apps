shinyServer(function(input, output, session) {
  output$bumpPlot <- renderPlot({
    bump_df <- get_bumpData(match_data)
    teams <- unique(bump_df$Team)
    get_bumpPlot(bump_df, teams, input$Team, input$md_range[1], input$md_range[2])
    })
  
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
      }
    
    
    
    })
})