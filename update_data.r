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

# Writing Dataframes to data/ ----
write.csv(big5, '~/R/shiny_applications/big5_dashboards/data/big5.csv', row.names = FALSE)
write.csv(big5_table, "~/R/shiny_applications/big5_dashboards/data/big5_table.csv", row.names = FALSE)
write.csv(gkDataCombined, "~/R/shiny_applications/big5_dashboards/data/big5_GK.csv", row.names = FALSE)
