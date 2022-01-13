shinyUI(
  fluidPage(
    navbarPage(
      "PL Dashboards by Joe",
      
      # Intro Tab -----
      tabPanel(
        "Introduction",
        
        titlePanel("Welcome to Joe's Premier League Dashboards!")
      ),
      
      # Standings Tab (bump, regular) ----
      navbarMenu(
        "League Table",
        tabPanel(
          "Standard Table"
        ),
        tabPanel(
          "Interactive Bump Plot",
          titlePanel("View your team's journey up and down the table!"),
          br(),
          fluidRow(
            column(2,
                   selectInput(
                   inputId = "Team",
                   label = "Teams",
                   choices = team_values,
                   selected = "All")),
          
            column(5,
                   sliderInput(inputId = "md_range", 
                   label = "Matchday Range",
                   value = c(1, max(md_values)),
                   min = 1,
                   max = max(md_values),
                   round = TRUE,
                   step = 1)),
            column(2,
                   checkboxInput(inputId = "bumpRank",
                                label = "See Selected Team's Rank",
                                value = FALSE)
                   )
          ),
          br(),
          girafeOutput(outputId = "bumpPlot")
        )
      ),
      # GK Zone Tab ----
      tabPanel(
        "GK Zone",
        
        titlePanel("Pick the Visualization you'd like to view (Or the one the makes you feel best about your team's keeper)"),
        br(),
        fluidRow(
          column(3,
                 selectInput(inputId = "gk_viz",
                             label = "Select a Viz",
                             choices = c("Who's Beating the Model?", "Getting Out of the Box", "Are Crosses Scary?"),
                             selected = "Who's Beating the Model?"
                             ),
                 selectInput(inputId = "gk_highlight",
                             label = "Highlight a Goalkeeper",
                             choices = pl_gks$Player,
                             selected = NULL),
                 textOutput("gk_text"),
                 ),
          column(9,
                 girafeOutput("gkPlot")
                 )
          ),
        br()
      )
    )
  )
)