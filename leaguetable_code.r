library(reactable)
library(tidyverse)

pl_table <- get_season_team_stats(country = "ENG", season_end_year = "2022",
                                  gender = "M", tier = "1st", stat_type = "league_table")

pl_table <- pl_table %>%
  select(Squad, MP, W, D, L, GD, Pts)

reactable(pl_table,
          defaultSortOrder = "desc",
          defaultSorted = c("Pts", "GD"),
          showSortIcon = FALSE,
          
          defaultPageSize = 20)

