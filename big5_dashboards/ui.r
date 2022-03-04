shinyUI(
  fluidPage(
    tags$head(
      tags$style(HTML("hr {
                      }
                      
                      * {
                        font-family: Roboto Mono;
                      }"
                      ))
    ),
    navbarPage(
      "Big 5 Dashboards by Joe",
      
      # Intro Tab -----
      tabPanel(
        "Introduction"
      ),
      
      # Standings Tab (regular, bump) ----
      navbarMenu(
         "League Table",
         tabPanel(
           "Standard Table"
         ),
         tabPanel(
           "Interactive Bump Plot",
           titlePanel("View your team's journey up and down the table!"),

           p("Hover over a team's name to view their path, or select a team in the sidebar for a smoother, detailed view"),

           sidebarLayout(
             sidebarPanel(
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
             mainPanel(girafeOutput(outputId = "bumpPlot"),
                       width = 9)
           )
         )
      ),


      # GK Zone Tab ----
      tabPanel(
        "GK Zone",

        titlePanel("Pick the Visualization you'd like to view (Or the one the makes you feel best about your team's keeper)"),
        br(),
        sidebarLayout(
          sidebarPanel(
            uiOutput(outputId = "gkZoneViz"),
            uiOutput(outputId = "gkZoneComp"),
            uiOutput(outputId = "gkZonePlayer"),
            h4("Background"),
            uiOutput(outputId = "gkZoneText"),
            width = 4
          ),
          mainPanel(uiOutput("gkZonePlot"),
                    width = 8
          )
        )
      ),
      
      # XG Timelines Tab ----
      tabPanel(
        "xG Timelines",
        
        wellPanel(
          fluidRow(
            column(3,
                   uiOutput(outputId = "xgViz"),
            ),
            column(2,
                   uiOutput(outputId = "xgComp")
            ),
            column(2,
                   uiOutput(outputId = "xgTeam")
            ),
            column(2,
                   uiOutput(outputId = "xgPalette")
            ),
            column(3,
                   uiOutput(outputId = "xgVizText"))
          )
        ),
        
        uiOutput("xgPlot"),
      )
      )
    )
  )

