# Big 5 Match Data ----
big5 <- data.frame(get_match_results(country = c("ENG", "ESP", "ITA", "GER", "FRA"), gender = "M", season_end_year = "2022", tier = "1st") %>%
                     mutate(Home = ifelse(Home == "M'Gladbach", "Gladbach",
                                          ifelse(Home == "Union Berlin", "Berlin",
                                                 ifelse(Home == "Bayern Munich", "Bayern",
                                                        ifelse(Home == "Greuther Fürth", "Fürth",
                                                               ifelse(Home == "Eint Frankfurt", "Frankfurt",
                                                                      ifelse(Home == "Atlético Madrid", "Atleti",
                                                                             ifelse(Home == "Athletic Club", "Athletic",
                                                                                    ifelse(Home == "Rayo Vallecano", "Rayo",
                                                                                           ifelse(Home == "Real Sociedad", "La Real",
                                                                                                  ifelse(Home == "Clermont Foot", "Clermont",
                                                                                                         ifelse(Home == "Saint-Étienne", "Étienne",
                                                                                                                ifelse(Home == "Manchester Utd", "Man Utd",
                                                                                                                       ifelse(Home == "Manchester City", "Man City",
                                                                                                                              ifelse(Home == "Leicester City", "Leicester",
                                                                                                                                     ifelse(Home == "Leeds United", "Leeds",
                                                                                                                                            ifelse(Home == "Norwich City", "Norwich",
                                                                                                                                                   ifelse(Home == "Crystal Palace", "Palace",
                                                                                                                                                          ifelse(Home == "Newcastle Utd", "Newcastle",
                                                                                                                                                                 ifelse(Home == "Hellas Verona", "Verona",
                                                                                                                                                                        Home))))))))))))))))))),
                            Away = ifelse(Away == "M'Gladbach", "Gladbach",
                                          ifelse(Away == "Union Berlin", "Berlin",
                                                 ifelse(Away == "Bayern Munich", "Bayern",
                                                        ifelse(Away == "Greuther Fürth", "Fürth",
                                                               ifelse(Away == "Eint Frankfurt", "Frankfurt",
                                                                      ifelse(Away == "Atlético Madrid", "Atleti",
                                                                             ifelse(Away == "Athletic Club", "Athletic",
                                                                                    ifelse(Away == "Rayo Vallecano", "Rayo",
                                                                                           ifelse(Away == "Real Sociedad", "La Real",
                                                                                                  ifelse(Away == "Clermont Foot", "Clermont",
                                                                                                         ifelse(Away == "Saint-Étienne", "Étienne",
                                                                                                                ifelse(Away == "Manchester Utd", "Man Utd",
                                                                                                                       ifelse(Away == "Manchester City", "Man City",
                                                                                                                              ifelse(Away == "Leicester City", "Leicester",
                                                                                                                                     ifelse(Away == "Leeds United", "Leeds",
                                                                                                                                            ifelse(Away == "Norwich City", "Norwich",
                                                                                                                                                   ifelse(Away == "Crystal Palace", "Palace",
                                                                                                                                                          ifelse(Away == "Newcastle Utd", "Newcastle",
                                                                                                                                                                 ifelse(Away == "Hellas Verona", "Verona",
                                                                                                                                                                        Away))))))))))))))))))),
                            Competition_Name = ifelse(Competition_Name != "Premier League" & Competition_Name != "La Liga" & Competition_Name != "Ligue 1" & Competition_Name != "Serie A", "Bundesliga", Competition_Name))) # weird work around for unicode reading on shiny apps

# Big 5 Table Data ----
big5_table <- get_season_team_stats(country = c("ENG", "ESP", "ITA", "GER", "FRA"), gender = "M", season_end_year = "2022",
                                    tier = "1st", stat_type = "league_table") %>%
  mutate(Competition_Name = ifelse(Competition_Name != "Premier League" & Competition_Name != "La Liga" &
                                     Competition_Name != "Ligue 1" & Competition_Name != "Serie A",
                                   "Bundesliga", Competition_Name),
         Last.5 = trimws(Last.5))

# Big 5 GK Data ----
gkData <- fb_big5_advanced_season_stats(season_end_year = 2022,
                                         stat_type = "keepers",
                                         team_or_player = "player")

gkAdvData <- fb_big5_advanced_season_stats(season_end_year = 2022,
                                           stat_type = "keepers_adv",
                                           team_or_player = "player")

colnames(gkAdvData)[colnames(gkAdvData) == "#OPA_Sweeper"] <- "OPA_Sweeper"
colnames(gkAdvData)[colnames(gkAdvData) == "#OPA_per_90_Sweeper"] <- "OPA_Sweeper_per_90"


passingData <- fb_big5_advanced_season_stats(season_end_year = 2022,
                                             stat_type = "passing",
                                             team_or_player = "player")

gkDataCombined <- data.frame(merge(merge(gkData,
                                         gkAdvData),
                                   passingData))

colnames(gkDataCombined)[colnames(gkDataCombined) == "X_per_90_Expected"] <- "per_90"
colnames(gkDataCombined)[colnames(gkDataCombined) == "PSxG._per__minus__Expected"] <- "PSxG_minus_GA"

# Big 5 Shots ----
bund_shots <- understat_league_season_shots("Bundesliga", 2021) %>%
  filter(situation != "Penalty") %>%
  select(-c(player_id, season)) %>%
  mutate(goal = if_else(result == "Goal", 1, 0))

laliga_shots <- understat_league_season_shots("La liga", 2021)  %>%
  filter(situation != "Penalty") %>%
  select(-c(player_id, season)) %>%
  mutate(goal = if_else(result == "Goal", 1, 0))

ligue1_shots <- understat_league_season_shots("Ligue 1", 2021)  %>%
  filter(situation != "Penalty") %>%
  select(-c(player_id, season)) %>%
  mutate(goal = if_else(result == "Goal", 1, 0))

pl_shots <- understat_league_season_shots("EPL", 2021)  %>%
  filter(situation != "Penalty") %>%
  select(-c(player_id, season)) %>%
  mutate(goal = if_else(result == "Goal", 1, 0))

serieA_shots <- understat_league_season_shots("Serie A", 2021)  %>%
  filter(situation != "Penalty") %>%
  select(-c(player_id, season)) %>%
  mutate(goal = if_else(result == "Goal", 1, 0))

big5_shots <- rbind(bund_shots,
                    rbind(laliga_shots,
                          rbind(ligue1_shots,
                                rbind(pl_shots, serieA_shots))))

big5_shots <- big5_shots %>%
  mutate(
    team = if_else(h_a == "h", home_team, away_team),
    player = case_when(
      player == "Danilo D&#039;Ambrosio" ~ "Danilo D'Ambrosio",
      player == "N&#039;Golo Kanté" ~ "N'Golo Kanté",
      TRUE ~ player
    ),
    league = case_when(
      league == "EPL" ~ "Premier League",
      league == "La_liga"  ~ "La Liga",
      league == "Ligue_1" ~ "Ligue 1",
      league == "Serie_A" ~ "Serie A",
      TRUE ~ league
    ),
    team = case_when(
      team == "Alaves" ~ "Alavés",
      team == "Arminia Bielefeld" ~ "Arminia",
      team == "Athletic Club" ~ "Athletic",
      team == "Atletico Madrid" ~ "Atleti",
      team == "Bayern Munich" ~ "Bayern",
      team == "Borussia Dortmund" ~ "Dortmund",
      team == "Borussia M.Gladbach" ~ "Gladbach",
      team == "Clermont Foot" ~ "Clermont",
      team == "Crystal Palace" ~ "Palace",
      team == "Eintracht Frankfurt" ~ "Frankfurt",
      team == "FC Cologne" ~ "Köln",
      team == "Greuther Fuerth" ~ "Furth",
      team == "Hertha Berlin" ~ "Hertha BSC",
      team == "Bayer Leverkusen" ~ "Leverkusen",
      team == "Manchester City" ~ "Man City",
      team == "Manchester United" ~ "Man Utd",
      team == "AC Milan" ~ "Milan",
      team == "Newcastle United" ~ "Newcastle",
      team == "Paris Saint Germain" ~ "Paris S-G",
      team == "RasenBallsport Leipzig" ~ "RB Leipzig",
      team == "Rayo Vallecaon" ~ "Rayp",
      team == "Real Betis" ~ "Betis",
      team == "Real Sociedad" ~ "La Real",
      team == "Saint-Etienne" ~ "Étienne",
      team == "VfB Stuttgart" ~ "Stuttgart",
      team == "Union Berlin" ~ "Berlin",
      team == "Wolverhampton Wanderers" ~ "Wolves",
      TRUE ~ team
    )
    )

# Writing Dataframes to data/ ----
write.csv(big5, "~/R/shiny_applications/big5_dashboards/data/big5.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(big5_table, "~/R/shiny_applications/big5_dashboards/data/big5_table.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(gkDataCombined, "~/R/shiny_applications/big5_dashboards/data/big5_GK.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(big5_shots, "~/R/shiny_applications/big5_dashboards/data/big5_shots.csv", row.names = FALSE, fileEncoding = "UTF-8")
