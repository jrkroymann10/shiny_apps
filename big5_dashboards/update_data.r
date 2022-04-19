# Big 5 Match Data ----
big5 <- data.frame(get_match_results(country = c("ENG", "ESP", "ITA", "GER", "FRA"), gender = "M", season_end_year = "2022", tier = "1st")) %>%
                     mutate(
                       Home = case_when(
                         Home == "M'Gladbach" ~ "Gladbach",
                         Home == "Union Berlin" ~ "Berlin",
                         Home == "Bayern Munich" ~ "Bayern",
                         Home == "Greuther Fürth" ~ "Fürth",
                         Home == "Eint Frankfurt" ~ "Frankfurt",
                         Home == "Atlético Madrid" ~ "Atleti",
                         Home == "Athletic Club" ~ "Athletic",
                         Home == "Rayo Vallecano" ~ "Rayo",
                         Home == "Real Sociedad" ~ "La Real",
                         Home == "Clermont Foot" ~ "Clermont",
                         Home == "Saint-Étienne" ~ "Étienne",
                         Home == "Manchester Utd" ~ "Man Utd",
                         Home == "Manchester City" ~ "Man City",
                         Home == "Leicester City" ~ "Leicester",
                         Home == "Leeds United" ~ "Leeds",
                         Home == "Norwich City" ~ "Norwich",
                         Home == "Crystal Palace" ~ "Palace",
                         Home == "Newcastle Utd" ~ "Newcastle",
                         Home == "Hellas Verona" ~ "Verona",
                         TRUE ~ Home
                         ),
                       Away = case_when(
                         Away == "M'Gladbach" ~ "Gladbach",
                         Away == "Union Berlin" ~ "Berlin",
                         Away == "Bayern Munich" ~ "Bayern",
                         Away == "Greuther Fürth" ~ "Fürth",
                         Away == "Eint Frankfurt" ~ "Frankfurt",
                         Away == "Atlético Madrid" ~ "Atleti",
                         Away == "Athletic Club" ~ "Athletic",
                         Away == "Rayo Vallecano" ~ "Rayo",
                         Away == "Real Sociedad" ~ "La Real",
                         Away == "Clermont Foot" ~ "Clermont",
                         Away == "Saint-Étienne" ~ "Étienne",
                         Away == "Manchester Utd" ~ "Man Utd",
                         Away == "Manchester City" ~ "Man City",
                         Away == "Leicester City" ~ "Leicester",
                         Away == "Leeds United" ~ "Leeds",
                         Away == "Norwich City" ~ "Norwich",
                         Away == "Crystal Palace" ~ "Palace",
                         Away == "Newcastle Utd" ~ "Newcastle",
                         Away == "Hellas Verona" ~ "Verona",
                         TRUE ~ Away
                       ),
                       Competition_Name = case_when(
                        Competition_Name == "Fußball-Bundesliga" ~ "Bundesliga" 
                       )
                     )

# Big 5 Table Data ----
big5_table <- get_season_team_stats(country = c("ENG", "ESP", "ITA", "GER", "FRA"), gender = "M", season_end_year = "2022",
                                    tier = "1st", stat_type = "league_table") %>%
  mutate(
    Competition_Name = case_when(
      Competition_Name == "Fußball-Bundesliga" ~ "Bundesliga" 
      ),
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
      player == "Yannis M&#039;Bemba" ~ "Yannis M'Bemba",
      player == "Vital N&#039;Simba" ~ "Vital N'Simba",
      player == "Hianga&#039;a M&#039;Bock" ~ "Hianga'a Mbock",
      player == "N&#039;Dri Koffi" ~ "N'Dri Koffi",
      player == "Kevin N&#039;Doram" ~ "Kévin N'Doram",
      player == "M&#039;Baye Niang" ~ "M'Baye Niang",
      player == "M&#039;Bala Nzola" ~ "M'Bala Nzola",
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
