# shinyServer ----
# ----
shinyServer(function(input, output, session) {
  vals <- reactiveValues()
  
  vals$big5 <- data.frame(get_match_results(country = c("ENG", "ESP", "ITA", "GER", "FRA"), gender = "M", season_end_year = "2022", tier = "1st") %>%
                            mutate(Home = ifelse(Home == "M'Gladbach", "Gladbach", Home),
                                   Away = ifelse(Away == "M'Gladbach", "Gladbach", Away)))
  # [Bump Plot] - UI (Competition) ----
  output$competition <- renderUI({
    selectizeInput(inputId = "competition",
                   label = "Competition",
                   choices = c("Fußball-Bundesliga", "La Liga", "Ligue 1", "Premier League",
                               "Serie A"),
                   multiple = FALSE,
                   options = list(
                     placeholder = 'Select a Competition',
                     onInitialize = I('function() { this.setValue("Premier League"); }')),
                   width = 300
    )
  })
  # [Bump Plot] - Reactive Data? ----
  last_wk <- reactive({
    df <- vals$big5
    
    if(input$competition == "Fußball-Bundesliga") {
      return(find_lastWeek(df %>% dplyr::filter(Competition_Name == "Fußball-Bundesliga") %>%
                             mutate(HomePoints = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals < AwayGoals, 0, 1)),
                                    AwayPoints = ifelse(HomeGoals > AwayGoals, 0, ifelse(HomeGoals < AwayGoals, 3, 1)),
                                    gamePlayed = ifelse(!is.na(HomePoints), 1, 0))))
    }
    else if (input$competition == "La Liga") {
      return(find_lastWeek(df %>% dplyr::filter(Competition_Name == "La Liga") %>%
                             mutate(HomePoints = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals < AwayGoals, 0, 1)),
                                    AwayPoints = ifelse(HomeGoals > AwayGoals, 0, ifelse(HomeGoals < AwayGoals, 3, 1)),
                                    gamePlayed = ifelse(!is.na(HomePoints), 1, 0))))
    }
    else if (input$competition == "Ligue 1") {
      return(find_lastWeek(df %>% dplyr::filter(Competition_Name == "Ligue 1") %>%
                             mutate(HomePoints = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals < AwayGoals, 0, 1)),
                                    AwayPoints = ifelse(HomeGoals > AwayGoals, 0, ifelse(HomeGoals < AwayGoals, 3, 1)),
                                    gamePlayed = ifelse(!is.na(HomePoints), 1, 0))))
    }
    else if (input$competition == "Premier League") {
      return(find_lastWeek(df %>% dplyr::filter(Competition_Name == "Premier League") %>%
                             mutate(HomePoints = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals < AwayGoals, 0, 1)),
                                    AwayPoints = ifelse(HomeGoals > AwayGoals, 0, ifelse(HomeGoals < AwayGoals, 3, 1)),
                                    gamePlayed = ifelse(!is.na(HomePoints), 1, 0))))
    }
    else if (input$competition == "Serie A") {
      return(find_lastWeek(df %>% dplyr::filter(Competition_Name == "Serie A") %>%
                             mutate(HomePoints = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals < AwayGoals, 0, 1)),
                                    AwayPoints = ifelse(HomeGoals > AwayGoals, 0, ifelse(HomeGoals < AwayGoals, 3, 1)),
                                    gamePlayed = ifelse(!is.na(HomePoints), 1, 0))))


    }
  })
  bump_data <- reactive({
    df <- vals$big5
    
    if (input$competition == "Fußball-Bundesliga") {
      get_bumpData(df %>% dplyr::filter(Competition_Name == "Fußball-Bundesliga") %>%
                     mutate(HomePoints = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals < AwayGoals, 0, 1)),
                            AwayPoints = ifelse(HomeGoals > AwayGoals, 0, ifelse(HomeGoals < AwayGoals, 3, 1)),
                            gamePlayed = ifelse(!is.na(HomePoints), 1, 0)),
                   last_wk())
      }
    else if (input$competition == "La Liga") {
      get_bumpData(df %>% dplyr::filter(Competition_Name == "La Liga") %>%
                     mutate(HomePoints = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals < AwayGoals, 0, 1)),
                            AwayPoints = ifelse(HomeGoals > AwayGoals, 0, ifelse(HomeGoals < AwayGoals, 3, 1)),
                            gamePlayed = ifelse(!is.na(HomePoints), 1, 0)),
                   last_wk())
    }
    else if (input$competition == "Ligue 1") {
      get_bumpData(df %>% dplyr::filter(Competition_Name == "Ligue 1") %>%
                     mutate(HomePoints = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals < AwayGoals, 0, 1)),
                            AwayPoints = ifelse(HomeGoals > AwayGoals, 0, ifelse(HomeGoals < AwayGoals, 3, 1)),
                            gamePlayed = ifelse(!is.na(HomePoints), 1, 0)),
                   last_wk())
    }
    else if (input$competition == "Premier League") {
      get_bumpData(df %>% dplyr::filter(Competition_Name == "Premier League") %>%
                     mutate(HomePoints = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals < AwayGoals, 0, 1)),
                            AwayPoints = ifelse(HomeGoals > AwayGoals, 0, ifelse(HomeGoals < AwayGoals, 3, 1)),
                            gamePlayed = ifelse(!is.na(HomePoints), 1, 0)),
                   last_wk())
    }
    else if (input$competition == "Serie A") {
      get_bumpData(df %>% dplyr::filter(Competition_Name == "Serie A") %>%
                     mutate(HomePoints = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals < AwayGoals, 0, 1)),
                            AwayPoints = ifelse(HomeGoals > AwayGoals, 0, ifelse(HomeGoals < AwayGoals, 3, 1)),
                            gamePlayed = ifelse(!is.na(HomePoints), 1, 0)),
                   last_wk())
    }
  })
  # [Bump Plot] - UI Slider (MD Range) ----
  output$md_range <- renderUI({
    req(input$competition)
    sliderInput(inputId = "md_range",
                  label = "Matchday Range",
                  value = c(1, 20),
                  min = 1,
                  max = 20,
                  round = TRUE,
                  step = 1,
                  width = 300)
  })
  # [Bump Plot] - UI (Teams) ----
  output$teams <- renderUI({
    req(input$competition)
    selectizeInput(inputId = "teams",
                   label = "Teams",
                   choices = get_team_choices(input$competition),
                   multiple = TRUE,
                   options = list(
                     placeholder = 'Select team(s) below',
                     onInitialize = I('function() { this.setValue(); }')
                     ),
                   width = 300)
  })
  # [Bump Plot] - UI (Background Color) ----
  output$back_color <- renderUI({
    selectizeInput(
        "back_color",
        "Plot Background Color",
        choices = c("#D3D3D3 (Gray)",
                    "#000000 (Black)"),
        options = list(
          placeholder = 'Select a Background Color',
          onInitialize = I('function() { this.setValue("#D3D3D3 (Gray)"); }')
        ),
        width = 300
    )
  })
  # [Bump Plot] - Building Bump Data + Other Values Needed for Output ----
  teams <- reactive({
    req(bump_data())
    unique(bump_data()$Team)
  })

  league_palette <- reactive({
    req(input$competition)
    get_leaguePalette(input$competition)
  })

  output$firstTeam <- renderText({
    req(input$teams)
    length(input$teams)
  })

  # [Bump Plot] - Plot Output ----
  output$bumpPlot <- renderGirafe({
    req(input$competition)
    req(input$md_range)

    # validate(
    #   need(input$md_range[2] <= get_league_maxMD(input$competition), "")
    # )

    girafe(
      ggobj =
        if (length(input$teams) >= 1) {
          bump_data() %>%
            ggplot(aes(x = Matchday, y = Rank, colour = Team)) +
            geom_bump(smooth = 5, size = 3, lineend = "round") +
            geom_point(size = 5) +
            xlim(input$md_range[1] - 1, input$md_range[2] + 1) +
            scale_colour_manual(
              breaks = teams(),
              values = league_palette()
            ) +
            geom_text(data = bump_data() %>%
                        dplyr::filter(Matchday == input$md_range[1]),
                      aes(label = Team, input$md_range[1] - 1), fontface = "bold", hjust = 0.5, size = 11) +
            geom_text(data = bump_data() %>%
                        dplyr::filter(Matchday == input$md_range[2]),
                      aes(label = Team, x = input$md_range[2] + 1), fontface = "bold", hjust = 0.5, size = 11) +
            gghighlight(any(Team == input$teams),
                        use_direct_label = input$bump_rank,
                        label_key = Rank,
                        label_params = list(colour = "black", size = 10),
                        unhighlighted_params = list(alpha = 0.1)) +
            scale_y_reverse() +
            # labs(title = paste("Premier League Table Progression (Matchday ", start_md, " - ", end_md, ")",
            #                    sep = "")) +
            theme_minimal() +
            theme(
              legend.position = "none",

              panel.grid = element_blank(),
              panel.background = element_rect(substring(input$back_color, 1, 7)),

              plot.title = element_text(size = 45, face = "bold", hjust = 0.5),

              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_blank()
            )
        }
      else {
        bump_data() %>%
          ggplot(aes(x = Matchday, y = Rank, colour = Team, data_id = Team)) +
          geom_path_interactive(size = 3, lineend = "round") + 
          geom_point_interactive(size = 5) + 
          xlim(input$md_range[1] - 1, input$md_range[2] + 1) +
          scale_colour_manual(
            breaks = teams(),
            values = league_palette()
          ) +
          geom_text_interactive(data = bump_data() %>%
                      dplyr::filter(Matchday == input$md_range[1]),
                    aes(label = Team, x = input$md_range[1]), fontface = "bold", hjust = 0.5, size = 11) +
          geom_text_interactive(data = bump_data() %>%
                      dplyr::filter(Matchday == input$md_range[2]),
                    aes(label = Team, x = input$md_range[2]), fontface = "bold", hjust = 0.5, size = 11) +
          scale_y_reverse() +
          # labs(title = paste("Premier League Table Progression (Matchday ", start_md, " - ", end_md, ")",
          #                    sep = "")) +
          theme_minimal() +
          theme(
            legend.position = "none",

            panel.grid = element_blank(),
            panel.background = element_rect(substring(input$back_color, 1, 7)),

            plot.title = element_text(size = 45, face = "bold", hjust = 0.5),

            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank()
          )
      },
      width_svg = 35, height_svg = 16,
      options = list(
        opts_hover_inv(css = "opacity:0.1;"),
        opts_hover(css = "stroke-width:2;"),
        opts_selection(type = "none")
      )
    )
  })
  #
  # [Bump Plot] - Resetting Team Input(s) ----
  observeEvent(
    input$resetBumpTeams,

    updateSelectizeInput(inputId = "teams",
                         label = "Teams",
                         choices = get_team_choices(input$competition),
                         options = list(
                           placeholder = 'Select team(s) below',
                           onInitialize = I('function() { this.setValue(); }')
                         ))
  )
})
  # ------------------------------------------------------------------------
  # [GK Zone] - Plot Output ---- 
  # # # output$gkPlot <- renderGirafe({
  # # #   gk_data <- get_plKeeper_adv(gk_data, gk_data_adv)
  # # #   
  # # #   if (input$gk_viz == "Who's Beating the Model?") {
  # # #     girafe(
  # # #       ggobj = gk_model_plot(gk_data),
  # # #       width_svg = 9, height_svg = 5.5,
  # # #       options = list(
  # # #         opts_tooltip(use_fill = TRUE),
  # # #         opts_hover_inv(css = "opacity:0.5;")
  # # #       )
  # # #     )
  # # #   } else if (input$gk_viz == "Getting Out of the Box") {
  # # #     girafe(
  # # #       ggobj = gk_sweeper_plot(gk_data),
  # # #       width_svg = 9, height_svg = 5.5,
  # # #       options = list(
  # # #         opts_tooltip(use_fill = TRUE),
  # # #         opts_hover_inv(css = "opacity:0.5;")
  # # #       )
  # # #     )
  # # #   } else if (input$gk_viz == "Are Crosses Scary?") {
  # # #     girafe(
  # # #       ggobj = gk_cross_plot(gk_data),
  # # #       width_svg = 9, height_svg = 5.5,
  # # #       options = list(
  # # #         opts_tooltip(use_fill = TRUE),
  # # #         opts_hover_inv(css = "opacity:0.5;")
  # # #       )
  # # #     )
  # # #   } else if (input$gk_viz == "Building From the Back") {
  # # #     
  # # #   }
  # # #   
  # # #   
  # # #   
  # # # })
  # [GK Zone] - Text Output ----
  # # # output$gk_text <- renderText(
  # # #   gk_model_text
  # # # )
  # ------------------------------------------------------------------------