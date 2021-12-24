# load libraries, data -----------------------------------
load("data/characters.Rdata")

# page 1 - Introduction -----------------------------------
intro_panel <- tabPanel(
  "Introduction",
  
  titlePanel("Characterisitcs of Mario Kart Drivers"),
  
  img(src = "mariokart8.jpg", height = 400, width = 800),
  
  br(),
  br(),
  
  p(a(href = "https://github.com/yhejazi/tutorials/tree/main/rshiny", "An R shiny tutorial from @yhejazi")),
  p(a(href = "https://www.kaggle.com/barelydedicated/mariokart8?select=characters.csv", "Data Source (Kaggle)"))
)

# page 2 - Visualization ----------------------------------
select_values <- colnames(characters)

sidebar_content <- sidebarPanel(
  selectInput(
    "y_var",
    label = "Y variable",
    choices = select_values,
    selected = "Speed"
  )
)

main_content <- mainPanel(
  plotOutput("plot")
)

second_panel <- tabPanel(
  "Visualization",
  
  titlePanel("What are the Characteristics of each Mario Kart 8 Driver"),
  
  p("Use the selector input below to choose which variable you would like to see."),
  
  sidebarLayout(
    sidebar_content, main_content
  )
)

# user interface ------------------------------------------------------------
ui <- navbarPage(
  "Mario Kart Characteristics",
  intro_panel,
  second_panel
)