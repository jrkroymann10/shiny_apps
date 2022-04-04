# shinyServer ----
# ----
function(input, output, session) {
  # [Message Handler] - Background Color of Tab Panels ----
  observeEvent(input$navbarID, {
    if (input$navbarID == "Standard Table") {
      session$sendCustomMessage("background-color", "#303134")
    }
    else {
      session$sendCustomMessage("background-color", "white")
    }
  })
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
                   selected = "Premier League")
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
                   options = list(
                     placeholder = 'Select a Competition',
                     onInitialize = I('function() { this.setValue("Premier League"); }'))
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
                    "#000000 (Black)",
                    "#FFFFFF (White)"),
        options = list(
          placeholder = 'Select a Background Color',
          onInitialize = I('function() { this.setValue("#D3D3D3 (Gray)"); }')
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
      gkDataCombined %>% dplyr::filter(Comp %in% input$gkZoneComp & Min_Playing >= 900)
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
    req(input$xgTeam)
    
    big5ToXG(big5[big5$Home == input$xgTeam | big5$Away == input$xgTeam,], input$xgTeam)
  })
  
  xgDataInt <- reactive({
    req(input$xgTeam)
    
    XGDataInterp(tidyXGData(big5ToXG(big5[big5$Home == input$xgTeam | big5$Away == input$xgTeam,], input$xgTeam)))
  })
  # [XG Time] - UI (Viz Selection) ----
  output$xgViz <- renderUI({
    selectizeInput(inputId = "xgViz",
                   label = "Visualization",
                   choices = c("6 Game Rolling Avg", "Game By Game"),
                   selected = "6 Game Rolling Average"
                   )
  })
  # [XG Time] - UI (Competition Selection) ----
  output$xgComp <- renderUI({
    selectizeInput(inputId = "xgComp",
                   label = "Competition",
                   choices = sort(unique(big5$Competition_Name)),
                   selected = "Premier League")
  })
  # [XG Time] - UI (Team Selection) ----
  output$xgTeam <- renderUI({
    req(input$xgComp)
    
    selectizeInput(inputId = "xgTeam",
                   label = "Team",
                   choices = sort(unique(big5[big5$Competition_Name == input$xgComp,]$Home)),
                   selected = "Liverpool")
  })
  # [XG Time] - UI (Palette Selection) ----
  output$xgPalette <- renderUI({
    selectizeInput(inputId = "xgPalette",
                   label = "Palette",
                   choices = sort(c("Liverpool", "Venezia", "Real Betis")),
                   selected = "Liverpool")
  })
  # [XG Time] - UI (Text Output) ----
  # [XG Time] - Plot Output ----
  output$xgPlot <- renderUI(
    renderGirafe({
      req(input$xgTeam)
      req(input$xgComp)
      
      girafe(
        ggobj = getXGPlot(viz = input$xgViz, df_int = xgDataInt(), df = xgData(), team = input$xgTeam, comp = input$xgComp,
                          bund = if_else(input$xgComp == "Bundesliga", TRUE, FALSE), 
                          the = if_else(input$xgComp == "Bundesliga" | input$xgComp == "Premier League", TRUE, FALSE),
                          getXGPalette(input$xgPalette)),
        
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
}