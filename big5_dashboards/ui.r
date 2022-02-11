shinyUI(
  fluidPage(
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
               checkboxInput(
                 inputId = "bump_rank",
                 label = "See Weekly Ranks of Selected Team(s)",
                 value = FALSE),
               fluidRow(
                 column(
                   width = 6,
                   align = "center",
                   actionButton(inputId = "resetBumpTeams", "Reset Team Input(s)", width = 150)),
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
            uiOutput(outputId = "gkZoneText"),
            tableOutput("gkZoneSelected"),
            width = 3
          ),
          mainPanel(br(),
                    girafeOutput("gkZonePlot"),
                    width = 9
          )
        )
        # fluidRow(
        #          # selectInput(inputId = "gk_highlight",
        #          #             label = "Highlight a Goalkeeper",
        #          #             choices = pl_gks$Player,
        #          #             selected = "Aaron Ramsdale"),
        #          textOutput("gk_text"),
        #   ),
        #   column(9,
        #          girafeOutput("gkPlot")
        #   )
        # ),
      )
      )
    )
  )
