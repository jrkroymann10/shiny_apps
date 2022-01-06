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
          fluidRow(
            column(2,
                   selectInput(
                   inputId = "Team",
                   label = "Teams",
                   choices = team_values,
                   selected = "All")),
          
            column(5,
                   sliderInput(inputId = "md_range", 
                   label = "Select a Matchday Range to focus on!",
                   value = c(1, max(md_values)),
                   min = 1,
                   max = max(md_values),
                   round = TRUE,
                   step = 1))
          ),
          
          plotOutput(outputId = "bumpPlot")
        )
      ),
      # GK Zone Tab ----
      tabPanel(
        "GK Zone"
      )
    )
  )
)