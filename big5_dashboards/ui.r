tagList(tags$head(
  tags$script("
    Shiny.addCustomMessageHandler('background-color', function(color) {
      document.body.style.backgroundColor = color;
    });
    "),
  tags$style(HTML("
                  * {
                    font-family: Roboto Mono;
                  }
                  "
                  ))
),
navbarPage(title = "Big 5 Dashboards by Joe", id = "navbarID",
  # # Intro Tab -----
  # tabPanel("Introduction"),
  # Standings Tab (regular, bump) ---- 
  navbarMenu("League Table",
             tabPanel("Standard Table",
               column(3,
                 wellPanel(
                   uiOutput(outputId = "standTableComp", style = ""),
                   style = "background-color:#202124;"
                 )
               ),
               column(9,
                 reactableOutput(outputId = "standTable", width = "78.15%")
                )
               ),
             tabPanel("Rank Display",
                # h1("View your team's journey up and down the table!", align = "center"),
                h3("Select team(s) in the sidebar (Bump), or hover over a team's path (Line) to highlight them in the plot!", align = "center"),
                br(),
                sidebarLayout(
                  sidebarPanel(
                    uiOutput(outputId = "bumpPlotType"),
                    uiOutput(outputId = "competition"),
                    uiOutput(outputId = "teams"),
                    uiOutput(outputId = "md_range"),
                    uiOutput(outputId = "back_color"),
                    textOutput(outputId = "firstTeam"),
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        checkboxInput(
                          inputId = "bump_rank",
                          label = "See Weekly Ranks of Selected Team(s)",
                          value = FALSE),
                        style = 'padding-bottom:5px;'
                        )
                      ),
                    fluidRow(
                      column(
                        width = 6,
                        align = "center",
                        actionButton(inputId = "resetBumpTeams", "Reset Teams", width = 150)),
                      column(
                        width = 6,
                        align = "center",
                        actionButton(inputId = "resetBumpRange", "Reset MD Range", width = 150))
                      ),                              
                    width = 3),
                  mainPanel(girafeOutput(outputId = "bumpPlot"), width = 9)
                  )
                )
             ),      
  # GK Zone Tab ----
  tabPanel("GK Zone",
           titlePanel(uiOutput(outputId = "gkZoneTitleText")),
           br(),
           sidebarLayout(
             sidebarPanel(
               uiOutput(outputId = "gkZoneViz"),
               uiOutput(outputId = "gkZoneComp"),
               uiOutput(outputId = "gkZonePlayer"),
               h4("Background"),
               uiOutput(outputId = "gkZoneText"), width = 4),
             mainPanel(uiOutput("gkZonePlot"), width = 8
                       )                 
             )
           ),
  # XG Timelines Tab ----
  tabPanel("xG Timelines",
           wellPanel(
             fluidRow(
               column(2, uiOutput(outputId = "team_xgViz")),
               column(2, uiOutput(outputId = "team_xgComp")),
               column(2, uiOutput(outputId = "team_xgTeam")),
               column(2, uiOutput(outputId = "team_xgPalette")),
               column(3, uiOutput(outputId = "team_xgVizText"))
               )
             ),
           uiOutput("team_xgPlot"),
           ),    
  # Shot Maps Tab ----
  tabPanel("Shot Maps",
           sidebarLayout(
             position = "right",
             sidebarPanel(
               uiOutput(outputId = "player_xgComp"),
               uiOutput(outputId = "player_xgTeam"),
               uiOutput(outputId = "player_xgPlayer"),
               uiOutput(outputId = "player_xgPalette")
             ),
             mainPanel(uiOutput("player_shotMap"))
             )
           )
  )
)