# shinyServer ----
# ----
function(input, output, session) {
  # [Table] - Plot Output ----
  output$standTable <- renderReactable({
    req(input$standTableComp)
    standTable(table_data() %>% filter(Competition_Name == input$standTableComp))
  })
  # [Table] - UI (Competition) ----
  output$standTableComp <- renderUI({
    selectizeInput(inputId = "standTableComp",
                   label = HTML("<p style = 'color: white'>Competition</p>"),
                   choices = sort(unique(big5_table$Competition_Name)),
                   multiple = FALSE,
                   selected = sample(unique(big5_table$Competition_Name), 1),
                   options = list(
                     
                   ))
  })
  # [Table + Bump Plot] - Reactive Data ----
  table_data <- reactive({
    big5_table
  })
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
  # [Bump Plot] - UI (Plot Selection) ----
  output$bumpPlotType <- renderUI({
    selectizeInput(inputId = "bumpPlotType",
                   label = "Vizualization",
                   choices = sort(c("Bump Plot", "Bump Plot w/ Point Difference", "Interactive Line Plot")),
                   selected = "Bump Plot")
  })
  # [Bump Plot] - UI (Competition) ----
  output$competition <- renderUI({
    selectizeInput(inputId = "competition",
                   label = "Competition",
                   choices = sort(unique(big5$Competition_Name)),
                   multiple = FALSE,
                   selected = sample(unique(big5$Competition_Name), 1)
    )
  })
  # [Bump Plot] - UI Slider (MD Range) ----
  output$md_range <- renderUI({
    req(input$competition)
    sliderInput(inputId = "md_range",
                  label = "Matchday Range",
                  value = c(1, last_wk()),
                  min = 1,
                  max = max(table_data() %>% 
                              filter(Competition_Name == input$competition) %>%
                              pull(MP)),
                  round = TRUE,
                  step = 1)
  })
  # [Bump Plot] - UI (Teams) ----
  output$teams <- renderUI({
    req(input$competition)
    selectizeInput(inputId = "teams",
                   label = "Teams (Bump Only)",
                   choices = sort(unique(bump_data()$Team)),
                   multiple = TRUE,
                   options = list(
                     placeholder = 'Select Team(s) below',
                     onInitialize = I('function() { this.setValue(); }')
                     ))
  })
  # [Bump Plot] - UI (Background Color) ----
  output$back_color <- renderUI({
    selectizeInput(
        "back_color",
        "Background Color",
        choices = c("#D3D3D3 (Gray)",
                    "#202124 (Black)",
                    "#FFFFFF (White)"),
        options = list(
          placeholder = 'Select a Background Color',
          onInitialize = I('function() { this.setValue("#202124 (Black)"); }')
        )
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
  
  df <- reactive(bump_data() %>% filter(Matchday >= input$md_range[1] & Matchday <= input$md_range[2]))
  
  # output$firstTeam <- renderText({
  #   is.null(input$gkZonePlayer)
  # })

  # [Bump Plot] - Plot Output ----
  output$bumpPlot <- renderGirafe({
    req(input$competition)
    req(input$md_range)

    validate(
      need(input$md_range[2] <= last_wk(), "wait a second!")
    )
    
    girafe(
      ggobj = getBumpPlot(df(), input$md_range[1], input$md_range[2], teams(), input$teams, league_palette(),
                          input$bump_rank, input$back_color, input$bumpPlotType),
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
                         choices = sort(unique(bump_data()$Team)),
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
      gkDataCombined %>% dplyr::filter(Comp %in% input$team_gkZoneComp & Min_Playing >= 900)
    }
  })
  
  selected_gk <- reactive({
    input$gkZonePlot_selected
  })
  # {GK Zone] - UI Title Text ----
  output$gkZoneTitleText <- renderUI({
    validate(
      need(!is.na(input$gkZoneViz), "")
    )
    
    if (input$gkZoneViz == "") {
      h1("Select A Vizualization To Begin Your Investigation!", align = "center")
    }
    else {
      h5("Hover over plot points to discover who they represent, and utilize the Competition(s) and Goalkeeper(s) selections to redefine plots and track keepers across them!", align = "center")
    }
  })
  # [GK Zone] - UI (Viz Selection) ----
  output$gkZoneViz <- renderUI({
    selectizeInput(inputId = "gkZoneViz",
                   label = "Visualizations",
                   choices = c("Who's Beating the Model?", "Getting Out of the Box", "Distribution Drop-off"),
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
  # [GK Zone] - UI (GK Selection) ----
  output$gkZonePlayer <- renderUI({
    selectizeInput(
      inputId = "gkZonePlayer",
      label = "Goalkeeper(s)",
      choices = sort(gkData()$Player),
      multiple = TRUE,
      options = list(
        placeholder = 'Select Goalkeeper(s) to Track Across Plots',
        onInitialize = I('function() { this.setValue(); }'),
        maxItems = 10
      )
    )
  })
 
  # [GK Zone] - UI (Text Output) ----
  output$gkZoneText <- renderText({
    getGkZoneText(input$gkZoneViz)
  })
  # [GK Zone] - Plot Output ---- 
  output$gkZonePlot <- renderUI({
    validate(
      need(!is.na(input$gkZoneViz), "")
    )
    
    if (input$gkZoneViz == "") {
      tags$img(src = "kepaPkMiss.jpeg", height = 500, width = 1000)
    }
    else {
      renderGirafe({
        req(input$gkZoneViz)
        girafe(
          ggobj = getGkZonePlot(input$gkZoneViz, gkData(), input$gkZonePlayer),
          width_svg = 10, height_svg = 5.5,
          options = list(
            opts_selection(css = NULL,
                           type = "none"),
            opts_tooltip(use_fill = TRUE),
            opts_hover_inv(css = "opacity:0.5;")
          )
        )
      })
    }
  })
  # ------------------------------------------------------------------------
  # [XG Time] - Reactive Data ----
  xgData <- reactive({
    req(input$team_xgTeam)
    
    big5ToXG(big5[big5$Home == input$team_xgTeam | big5$Away == input$team_xgTeam,], input$team_xgTeam)
  })
  
  xgDataInt <- reactive({
    req(input$team_xgTeam)
    
    XGDataInterp(tidyXGData(big5ToXG(big5[big5$Home == input$team_xgTeam | big5$Away == input$team_xgTeam,], input$team_xgTeam)))
  })
  
  shot_data <- reactive({
    req(input$player_xgComp)
    
    big5_shots %>% 
      filter(league == input$player_xgComp,
             team == input$player_xgTeam,
             player == input$player_xgPlayer)
  })
  # [XG Time (Team)] - UI (Viz Selection) ----
  output$team_xgViz <- renderUI({
    selectizeInput(inputId = "team_xgViz",
                   label = "Visualization",
                   choices = c("6 Game Rolling Avg", "Game By Game"),
                   selected = "6 Game Rolling Average"
                   )
  })
  # [XG Time (Team)] - UI (Competition Selection) ----
  output$team_xgComp <- renderUI({
    selectizeInput(inputId = "team_xgComp",
                   label = "Competition",
                   choices = sort(unique(big5$Competition_Name)),
                   selected = sample(big5$Competition_Name))
  })
  # [XG Time (Team)] - UI (Team Selection) ----
  output$team_xgTeam <- renderUI({
    req(input$team_xgComp)
    
    selectizeInput(inputId = "team_xgTeam",
                   label = "Team",
                   choices = sort(unique(big5[big5$Competition_Name == input$team_xgComp,]$Home)),
                   selected = sample(unique(big5[big5$Competition_Name == input$team_xgComp,]$Home), 1))
  })
  # [XG Time (Team)] - UI (Palette Selection) ----
  output$team_xgPalette <- renderUI({
    selectizeInput(inputId = "team_xgPalette",
                   label = "Palette",
                   choices = sort(c("Liverpool", "Venezia", "Real Betis")),
                   selected = sample(c("Liverpool", "Venezia", "Real Betis"), 1))
  })
  # [XG Time (Team)] - UI (Text Output) ----
  # [XG Time (Team)] - Plot Output ----
  output$team_xgPlot <- renderUI(
    renderGirafe({
      req(input$team_xgTeam)
      req(input$team_xgComp)
      
      girafe(
        ggobj = getXGPlot(viz = input$team_xgViz, df_int = xgDataInt(), df = xgData(), team = input$team_xgTeam, comp = input$team_xgComp,
                          bund = if_else(input$team_xgComp == "Bundesliga", TRUE, FALSE), 
                          the = if_else(input$team_xgComp == "Bundesliga" | input$team_xgComp == "Premier League", TRUE, FALSE),
                          getXGPalette(input$team_xgPalette)),
        
        width_svg = 20,
        height_svg = 6,
        options = list(
          opts_hover_inv(css = "opacity:0.25;"),
          opts_hover(css = "stroke-width:2;"),
          opts_selection(type = "none")
        )
      )
    })
    )
  # [XG Time (Player)] - UI (Viz Selection) ----
  output$player_xgViz <- renderUI({
    selectizeInput(inputId = "player_xgViz",
                   label = "Visualization",
                   choices = c("10 Shot Rolling Avg", "Expected vs. Actual"),
                   selected = "Shot by Shot"
                   )
    })
  # [XG Time (Player)] - UI (Competition Selection) ----
  output$player_xgComp <- renderUI({
    selectizeInput(inputId = "player_xgComp",
                   label = "Competition",
                   choices = sort(unique(big5_shots$league)),
                   selected = sample(big5_shots$league, 1))
  })
  # [XG Time (Player)] - UI (Team Selection) ----
  output$player_xgTeam <- renderUI({
    req(input$player_xgComp)
    
    selectizeInput(inputId = "player_xgTeam",
                   label = "Team",
                   choices = sort(unique(big5_shots[big5_shots$league == input$player_xgComp,]$team)),
                   selected = sample(unique(big5_shots[big5_shots$league == input$player_xgComp,]$team), 1))
  })
  # [XG Time (Player)] - UI (Player Selection) ----
  output$player_xgPlayer <- renderUI({
    req(input$player_xgTeam)
    
    selectizeInput(inputId = "player_xgPlayer",
                   label = "Player",
                   choices = sort(unique(big5_shots[big5_shots$team == input$player_xgTeam,]$player)),
                   multiple = FALSE,
                   selected = sample(unique(big5_shots[big5_shots$team == input$player_xgTeam,]$player), 1))
  })
  
  # [XG Time (Player)] - UI (Palette Selection) ----
  output$player_xgPalette <- renderUI({
    selectizeInput(inputId = "player_xgPalette",
                   label = "Palette",
                   choices = sort(c("Hokusai1", "Egypt", "Archambault", "Tsimshian")),
                   multiple = FALSE,
                   selected = sample(c("Hokusai1", "Egypt", "Archambault", "Tsimshian"), 1))
  })
  # [XG Time (Player)] - Plot Output ----
  output$player_shotMap <- renderUI(
    renderGirafe({
      req(input$player_xgPlayer)
      
      validate(
        need(unique(shot_data()$player == input$player_xgPlayer) > 0 , "wait a second!")
      )
      
      girafe(
        ggobj = get_shotMap(shot_data(), input$player_xgPalette),
        
        options = list(
          opts_selection(css = NULL,
                         type = "none"),
          opts_tooltip(use_fill = TRUE),
          opts_hover_inv(css = "opacity:0.5;")
        )
      )
    })
  )
}