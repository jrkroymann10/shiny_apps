# shinyServer ----
# ----
shinyServer(function(input, output, session) {
  # [Bump Plot] - Loading in Data ----
  big5 <- data.frame(get_match_results(country = c("ENG", "ESP", "ITA", "GER", "FRA"), gender = "M", season_end_year = "2022", tier = "1st") %>%
                       mutate(Home = ifelse(Home == "M'Gladbach", "Gladbach", Home),
                              Away = ifelse(Away == "M'Gladbach", "Gladbach", Away)))
                            # mutate(Home = case_when(
                            #   Home == "Manchester Utd" ~ "Man Utd",
                            #   Home == "Liverpool" ~ "Liverpool",
                            #   Home == "Chelsea" ~ "Chelsea",
                            #   Home == "West Ham" ~ "West Ham",
                            #   Home == "Everton" ~ "Everton",
                            #   Home == "Brentford" ~ "Brentford",
                            #   Home == "Tottenham" ~ "Tottenham",
                            #   Home == "Watford" ~ "Watford", 
                            #   Home == "Brighton" ~ "Brighton", 
                            #   Home == "Leicester City" ~ "Leicester",
                            #   Home == "Leeds United" ~ "Leeds",
                            #   Home == "Norwich City" ~ "Norwich",
                            #   Home == "Crystal Palace" ~ "Palace",
                            #   Home == "Newcastle Utd" ~ "Newcastle",
                            #   Home == "Southampton" ~ "Sthampton",
                            #   Home == "Arsenal" ~ "Arsenal",
                            #   Home == "Manchester City" ~ "Man City",
                            #   Home == "Aston Villa" ~ "Aston Villa",
                            #   Home == "Burnley" ~ "Burnley",
                            #   Home == "Wolves" ~ "Wolves",
                            #   Home == "Arminia" ~ "Arminia",
                            #   Home == "Augsburg" ~ "Augsburg",
                            #   Home == "Bayern Munich" ~ "Bayern",
                            #   Home == "Bochum" ~ "Bochum",
                            #   Home == "Dortmund" ~ "Dortmund",
                            #   Home == "Eint Frankfurt" ~ "Frankfurt",
                            #   Home == "Freiburg" ~ "Freiburg",
                            #   Home == "M'Gladbach" ~ "Gladbach",
                            #   Home == "Greuther Fürth" ~ "Fürth",
                            #   Home == "Hertha BSC" ~ "Hertha BSC", 
                            #   Home == "Hoffenheim" ~ "Hoffenheim",
                            #   Home == "Köln" ~ "Köln",
                            #   Home == "Leverkusen" ~ "Leverkusen",
                            #   Home == "Mainz 05" ~ "Mainz 05",
                            #   Home == "RB Leipzig" ~ "RB Leipzig",
                            #   Home == "Stuttgart" ~ "Stuttgart",
                            #   Home == "Union Berlin" ~ "Union Berlin", 
                            #   Home == "Wolfsburg" ~ "Wolfsburg",
                            #   Home == "Alavés" ~ "Alavés",
                            #   Home == "Athletic Club" ~ "Athletic Club",
                            #   Home == "Atlético Madrid" ~ "Atlético Madrid",
                            #   Home == "Barcelona" ~ "Barcelona",
                            #   Home == "Betis" ~ "Betis",
                            #   Home == "Cádiz" ~ "Cádiz",
                            #   Home == "Celta Vigo" ~ "Celta Vigo",
                            #   Home == "Elche" ~ "Elche", 
                            #   Home == "Espanyol" ~ "Espanyol",
                            #   Home == "Getafe" ~ "Getafe",
                            #   Home == "Granada" ~ "Granada",
                            #   Home == "Levante" ~ "Levante", 
                            #   Home == "Mallorca" ~ "Mallorca", 
                            #   Home == "Osasuna" ~ "Osasuna",
                            #   Home == "Rayo Vallecano" ~ "Rayo",
                            #   Home == "Real Madrid" ~ "Real Madrid",
                            #   Home == "Real Sociedad" ~ "Sociedad",
                            #   Home == "Sevilla" ~ "Sevilla", 
                            #   Home == "Valencia" ~ "Valencia",
                            #   Home == "Villarreal" ~ "Villarreal",
                            #   Home == "Angers" ~ "Angers",
                            #   Home == "Bordeaux" ~ "Bordeaux",
                            #   Home == "Brest" ~ "Brest",
                            #   Home == "Clermont Foot" ~ "Clermont",
                            #   Home == "Lens" ~ "Lens", 
                            #   Home == "Lille" ~ "Lille",
                            #   Home == "Lorient" ~ "Lorient",
                            #   Home == "Lyon" ~ "Lyon",
                            #   Home == "Marseille" ~ "Marseille",
                            #   Home == "Metz" ~ "Mets", 
                            #   Home == "Monaco" ~ "Monaco",
                            #   Home == "Montpellier" ~ "Montpellier",
                            #   Home == "Nantes" ~ "Nantes",
                            #   Home == "Nice" ~ "Nice",
                            #   Home == "Paris S-G" ~ "Paris S-G",
                            #   Home == "Reims" ~ "Reims",
                            #   Home == "Rennes" ~ "Rennes",
                            #   Home == "Saint-Étienne" ~ "Étienne",
                            #   Home == "Strasbourg" ~ "Strasbourg",
                            #   Home == "Troyes" ~ "Troyes",
                            #   Home == "Reimes" ~ "Reims",
                            #   Home == "Atalanta" ~ "Atalanta",
                            #   Home == "Bologna" ~ "Bologna",
                            #   Home == "Cagliari" ~ "Cagliari",
                            #   Home == "Empoli" ~ "Empoli",
                            #   Home == "Fiorentina" ~ "Fiorentina",
                            #   Home == "Genoa" ~ "Genoa",
                            #   Home == "Hellas Verona" ~ "Verona",
                            #   Home == "Inter" ~ "Inter",
                            #   Home == "Juventus" ~ "Juventus",
                            #   Home == "Lazio" ~ "Lazio",
                            #   Home == "Milan" ~ "Milan",
                            #   Home == "Napoli" ~ "Napoli",
                            #   Home == "Roma" ~ "Roma",
                            #   Home == "Salernitana" ~ "Salernitana",
                            #   Home == "Sampdoria" ~ "Sampdoria",
                            #   Home == "Sassuolo" ~ "Sassuolo",
                            #   Home == "Spezia" ~ "Spezia",
                            #   Home == "Torina" ~ "Torino",
                            #   Home == "Udinese" ~ "Udinese",
                            #   Home == "Venezia" ~ "Venezia"
                            # ),
                            # Away = case_when(
                            #   Away == "Manchester Utd" ~ "Man Utd",
                            #   Away == "Liverpool" ~ "Liverpool",
                            #   Away == "Chelsea" ~ "Chelsea",
                            #   Away == "West Ham" ~ "West Ham",
                            #   Away == "Everton" ~ "Everton",
                            #   Away == "Brentford" ~ "Brentford",
                            #   Away == "Tottenham" ~ "Tottenham",
                            #   Away == "Watford" ~ "Watford", 
                            #   Away == "Brighton" ~ "Brighton", 
                            #   Away == "Leicester City" ~ "Leicester",
                            #   Away == "Leeds United" ~ "Leeds",
                            #   Away == "Norwich City" ~ "Norwich",
                            #   Away == "Crystal Palace" ~ "Palace",
                            #   Away == "Newcastle Utd" ~ "Newcastle",
                            #   Away == "Southampton" ~ "Sthampton",
                            #   Away == "Arsenal" ~ "Arsenal",
                            #   Away == "Manchester City" ~ "Man City",
                            #   Away == "Aston Villa" ~ "Aston Villa",
                            #   Away == "Burnley" ~ "Burnley",
                            #   Away == "Wolves" ~ "Wolves",
                            #   Away == "Arminia" ~ "Arminia",
                            #   Away == "Augsburg" ~ "Augsburg",
                            #   Away == "Bayern Munich" ~ "Bayern",
                            #   Away == "Bochum" ~ "Bochum",
                            #   Away == "Dortmund" ~ "Dortmund",
                            #   Away == "Eint Frankfurt" ~ "Frankfurt",
                            #   Away == "Freiburg" ~ "Freiburg",
                            #   Away == "M'Gladbach" ~ "Gladbach",
                            #   Away == "Greuther Fürth" ~ "Fürth",
                            #   Away == "Hertha BSC" ~ "Hertha BSC", 
                            #   Away == "Hoffenheim" ~ "Hoffenheim",
                            #   Away == "Köln" ~ "Köln",
                            #   Away == "Leverkusen" ~ "Leverkusen",
                            #   Away == "Mainz 05" ~ "Mainz 05",
                            #   Away == "RB Leipzig" ~ "RB Leipzig",
                            #   Away == "Stuttgart" ~ "Stuttgart",
                            #   Away == "Union Berlin" ~ "Union Berlin", 
                            #   Away == "Wolfsburg" ~ "Wolfsburg",
                            #   Away == "Alavés" ~ "Alavés",
                            #   Away == "Athletic Club" ~ "Athletic Club",
                            #   Away == "Atlético Madrid" ~ "Atlético Madrid",
                            #   Away == "Barcelona" ~ "Barcelona",
                            #   Away == "Betis" ~ "Betis",
                            #   Away == "Cádiz" ~ "Cádiz",
                            #   Away == "Celta Vigo" ~ "Celta Vigo",
                            #   Away == "Elche" ~ "Elche", 
                            #   Away == "Espanyol" ~ "Espanyol",
                            #   Away == "Getafe" ~ "Getafe",
                            #   Away == "Granada" ~ "Granada",
                            #   Away == "Levante" ~ "Levante", 
                            #   Away == "Mallorca" ~ "Mallorca", 
                            #   Away == "Osasuna" ~ "Osasuna",
                            #   Away == "Rayo Vallecano" ~ "Rayo",
                            #   Away == "Real Madrid" ~ "Real Madrid",
                            #   Away == "Real Sociedad" ~ "Sociedad",
                            #   Away == "Sevilla" ~ "Sevilla", 
                            #   Away == "Valencia" ~ "Valencia",
                            #   Away == "Villarreal" ~ "Villarreal",
                            #   Away == "Angers" ~ "Angers",
                            #   Away == "Bordeaux" ~ "Bordeaux",
                            #   Away == "Brest" ~ "Brest",
                            #   Away == "Clermont Foot" ~ "Clermont",
                            #   Away == "Lens" ~ "Lens", 
                            #   Away == "Lille" ~ "Lille",
                            #   Away == "Lorient" ~ "Lorient",
                            #   Away == "Lyon" ~ "Lyon",
                            #   Away == "Marseille" ~ "Marseille",
                            #   Away == "Metz" ~ "Mets", 
                            #   Away == "Monaco" ~ "Monaco",
                            #   Away == "Montpellier" ~ "Montpellier",
                            #   Away == "Nantes" ~ "Nantes",
                            #   Away == "Nice" ~ "Nice",
                            #   Away == "Paris S-G" ~ "Paris S-G",
                            #   Away == "Reims" ~ "Reims",
                            #   Away == "Rennes" ~ "Rennes",
                            #   Away == "Saint-Étienne" ~ "Étienne",
                            #   Away == "Strasbourg" ~ "Strasbourg",
                            #   Away == "Troyes" ~ "Troyes",
                            #   Away == "Reimes" ~ "Reims",
                            #   Away == "Atalanta" ~ "Atalanta",
                            #   Away == "Bologna" ~ "Bologna",
                            #   Away == "Cagliari" ~ "Cagliari",
                            #   Away == "Empoli" ~ "Empoli",
                            #   Away == "Fiorentina" ~ "Fiorentina",
                            #   Away == "Genoa" ~ "Genoa",
                            #   Away == "Hellas Verona" ~ "Verona",
                            #   Away == "Inter" ~ "Inter",
                            #   Away == "Juventus" ~ "Juventus",
                            #   Away == "Lazio" ~ "Lazio",
                            #   Away == "Milan" ~ "Milan",
                            #   Away == "Napoli" ~ "Napoli",
                            #   Away == "Roma" ~ "Roma",
                            #   Away == "Salernitana" ~ "Salernitana",
                            #   Away == "Sampdoria" ~ "Sampdoria",
                            #   Away == "Sassuolo" ~ "Sassuolo",
                            #   Away == "Spezia" ~ "Spezia",
                            #   Away == "Torina" ~ "Torino",
                            #   Away == "Udinese" ~ "Udinese",
                            #   Away == "Venezia" ~ "Venezia"
                            #   )
                            # )
  
  matches <- reactive({
    big5 %>% dplyr::filter(Competition_Name == input$competition) %>%
      mutate(HomePoints = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals < AwayGoals, 0, 1)),
             AwayPoints = ifelse(HomeGoals > AwayGoals, 0, ifelse(HomeGoals < AwayGoals, 3, 1)),
             gamePlayed = ifelse(!is.na(HomePoints), 1, 0))
  })
  
  # [Bump Plot] - UI (Competition) ----
  output$competition <- renderUI({
    selectizeInput(inputId = "competition",
                   label = "Competition",
                   choices = c("Fußball-Bundesliga", "La Liga", "Ligue 1", "Premier League",
                               "Serie A"),
                   multiple = FALSE,
                   options = list(
                     placeholder = 'Select a Competition',
                     onInitialize = I('function() { this.setValue("Fußball-Bundesliga"); }')),
                   width = 300
    )
  })
  # [Bump Plot] - Reactive Data? ----
  last_wk <- reactive({
    find_lastWeek(matches())
  })
  bump_data <- reactive({
    get_bumpData(matches(), last_wk()) 
  })
  # [Bump Plot] - UI Slider (MD Range) ----
  output$md_range <- renderUI({
    req(input$competition)
    req(last_wk())
    sliderInput(inputId = "md_range",
                  label = "Matchday Range",
                  value = c(1, last_wk()),
                  min = 1,
                  max = last_wk(),
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

    validate(
      need(input$md_range[2] <= last_wk(), "wait a second!")
    )
    
    df <- reactive(bump_data() %>% filter(Matchday >= input$md_range[1] & Matchday <= input$md_range[2]))

    girafe(
      ggobj =
        if (length(input$teams) >= 1) {
          df() %>%
            ggplot(aes(x = Matchday, y = Rank, colour = Team)) +
            geom_bump(smooth = 5, size = 3, lineend = "round") +
            geom_point(size = 5) +
            #xlim(input$md_range[1] - 1, input$md_range[2] + 1) +
            scale_colour_manual(
              breaks = teams(),
              values = league_palette()
            ) +
            geom_text(data = df() %>%
                        dplyr::filter(Matchday == input$md_range[1]),
                      aes(label = Team, input$md_range[1] - 1), fontface = "bold", hjust = 0.5, size = 11) +
            geom_text(data = df() %>%
                        dplyr::filter(Matchday == input$md_range[2]),
                      aes(label = Team, x = input$md_range[2] + 1), fontface = "bold", hjust = 0.35, size = 11) +
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
        df() %>%
          ggplot(aes(x = Matchday, y = Rank, colour = Team, data_id = Team)) +
          geom_path_interactive(size = 3, lineend = "round") + 
          geom_point_interactive(size = 5) + 
          #xlim(input$md_range[1] - 1, input$md_range[2] + 1) +
          scale_colour_manual(
            breaks = teams(),
            values = league_palette()
          ) +
          geom_text_interactive(data = df() %>%
                      dplyr::filter(Matchday == input$md_range[1]),
                    aes(label = Team, x = input$md_range[1] - 1), fontface = "bold", hjust = 0.5, size = 11) +
          geom_text_interactive(data = df() %>%
                      dplyr::filter(Matchday == input$md_range[2]),
                    aes(label = Team, x = input$md_range[2] + 1), fontface = "bold", hjust = 0.35, size = 11) +
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
      width_svg = 40, height_svg = 16,
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
  # [Bump Plot] - Resetting MD Range ----
  observeEvent(
    input$resetBumpRange,
    
    updateSliderInput(inputId = "md_range",
                      label = "Matchday Range",
                      value = c(1, last_wk()),
                      min = 1,
                      max = last_wk(),
                      step = 1)
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