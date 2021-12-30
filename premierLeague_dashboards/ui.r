# load libraries, data -----------------------------------------------------------------
library(readr)
match_data <- read_csv("data/pl_matchdata.csv")

team_values <- c("ALL", "ARS", "AVL", "BRE", "BRI", "BUR", "CHE", "CRY", 
                 "EVE", "LEE", "LEI", "LFC", "MCI", "MUN", "NEW", "NOR",
                 "SOU", "TOT", "WAT", "WHU", "WOL")

md_values <- 1:tail(match_data[!is.na(match_data$Home_xG),]$Wk, 1)

# page 1 - introduction ----------------------------------------------------------------
intro_panel <- tabPanel(
  "Introduction",
  
  titlePanel("Welcome to Joe's Premier League Dashboards!")
)

# page 2 - table bump plot -------------------------------------------------------------
bump_sidebar <- sidebarPanel(
  selectInput(
    "Team",
    label = "Teams",
    choices = team_values,
    selected = "ALL"
  ),
  
  selectInput(
    "Start_MD",
    label = "Starting Matchday",
    choices = md_values,
    selected = 1
  ),
  
  selectInput(
    "End_MD",
    label = "Ending Matchday",
    choices = md_values,
    selected = max(md_values)
  ),
  
  width = 2
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