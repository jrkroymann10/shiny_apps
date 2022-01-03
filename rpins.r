install.packages("pins")
library(pins)

board <- board_register_github(board = "github",
                               repo = "jrkroymann10/shiny_apps",
                               branch = "main",
                               token = Sys.getenv("GITHUB_JOE"))
  
board %>% pin(x = get_match_results(country = "ENG", gender = "M", season_end_year = "2022", tier = "1st"),
              name = "prem2021")

