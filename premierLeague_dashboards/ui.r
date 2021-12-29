# load libraries, data -----------------------------------------------------------------
library(readr)
match_data <- read_csv("data/pl_matchdata.csv")

# page 1 - introduction ----------------------------------------------------------------
intro_panel <- tabPanel(
  "Introduction",
  
  titlePanel("Welcome to Joe's Premier League Dashboards!")
)

# page 2 - table bump plot -------------------------------------------------------------
select_values <- unique(match_data$Home)

bump_sidebar <- sidebarPanel(
  selectInput(
    "Team",
    label = "Teams",
    choices = select_values,
    selected = "Liverpool"
  )
)

bump_content <- mainPanel(
  plotOutput("plot")
)

bump_panel <- tabPanel(
  "Table Bump Plot",
  
  titlePanel("View your Team's Journey Up and Down the Table!"),
  
  p("use the selector input below to choose a team to focus on"),
  
  sidebarLayout(
    bump_sidebar, bump_content
  )
)

ui <- navbarPage(
  "Premier League 2021-2022",
  intro_panel,
  bump_panel
)