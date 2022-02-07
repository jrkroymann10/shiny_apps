# shinyServer ----
# ----
shinyServer(function(input, output, session) {
  # [Bump Plot] - Reactive Data ----
  matches <- reactive({
    req(input$competition)
    big5 %>% dplyr::filter(Competition_Name == input$competition) %>%
      mutate(HomePoints = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals < AwayGoals, 0, 1)),
             AwayPoints = ifelse(HomeGoals > AwayGoals, 0, ifelse(HomeGoals < AwayGoals, 3, 1)),
             gamePlayed = ifelse(!is.na(HomePoints), 1, 0))
  })
  last_wk <- reactive({
    find_lastWeek(matches())
  })
  bump_data <- reactive({
    get_bumpData(matches(), last_wk()) 
  })
  # [Bump Plot] - UI (Competition) ----
  output$competition <- renderUI({
    selectizeInput(inputId = "competition",
                   label = "Competition",
                   choices = sort(unique(big5$Competition_Name)),
                   multiple = FALSE,
                   options = list(
                     placeholder = 'Select a Competition',
                     onInitialize = I('function() { this.setValue("Premier League"); }')),
                   width = 300
    )
  })
  # [Bump Plot] - UI Slider (MD Range) ----
  output$md_range <- renderUI({
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
                     placeholder = 'Select Team(s) below',
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
    length(input$gkZoneComp)
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
      ggobj = getBumpPlot(df(), input$md_range[1], input$md_range[2], teams(), input$teams, league_palette(),
                          input$bump_rank, input$back_color),
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
  # ------------------------------------------------------------------------
  # [GK Zone] - Reactive Data ----
  gkData <- reactive({
    if (length(input$gkZoneComp) < 1) {
      gkDataCombined %>% dplyr::filter(Min_Playing >= 900)
    }
    else {
      gkDataCombined %>% dplyr::filter(Comp == input$gkZoneComp & Min_Playing >= 900)
    }
  })
  # [GK Zone] - UI (Viz Selection) ----
  output$gkZoneViz <- renderUI({
    selectizeInput(inputId = "gkZoneViz",
                   label = "Visualizations",
                   choices = c("Who's Beating the Model?", "Getting Out of the Box",
                               "Are Crosses Scary?", "Building From the Back"),
                   options = list(
                     placeholder = 'Select a Narrative to Investigate',
                     onInitialize = I('function() { this.setValue(); }'))
                   )
  })
  # [GK Zone] - UI (Competition Selection) ----
  output$gkZoneComp <- renderUI({
    selectizeInput(inputId = "gkZoneComp",
                   label = "Competition(s)",
                   choices = sort(unique(gkDataCombined$Comp)),
                   multiple = TRUE,
                   options = list(
                     placeholder = 'Select Competition(s) to Filter By',
                     onInitialize = I('function() { this.setValue(); }')))
  })
  # [GK Zone] - UI (Player Selection) ----
  output$gkZonePlayer <- renderUI({
    selectizeInput(inputId = "gkZonePlayer",
                   label = "Goalkeeper(s)",
                   choices = sort(gkData()$Player),
                   multiple = TRUE,
                   options = list(
                     placeholder = 'Select Goalkeeper(s) to Track Across Plots',
                     onInitialize = I('function() { this.setValue(); }'))
                   )
  })
  # [GK Zone] - Plot Output ---- 
  output$gkZonePlot <- renderGirafe({
    # browser()
    req(input$gkZoneViz)
    girafe(
      ggobj = getGkZonePlot(input$gkZoneViz, gkData()),
      width_svg = 9, height_svg = 5.5,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover_inv(css = "opacity:0.5;")
        )
      )
    })
  # [GK Zone] - Text Output ----
  # # # output$gk_text <- renderText(
  # # #   gk_model_text
  # # # )
  # ------------------------------------------------------------------------
})