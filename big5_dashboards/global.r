# [Loading] - Libraries -----------------------------------------------------------------
library(readr)
library(shiny)
library(devtools)
library(tidyverse)
library(ggbump)
library(readr)
library(gghighlight)
library(ggplot2)
library(ggiraph)
library(MetBrewer)
library(worldfootballR)
library(zoo)
library(stringr)
library(DescTools)
library(ggtext)
library(glue)
library(showtext)
#
font_add_google("Roboto Mono", "Roboto")
showtext_auto()

colGrid <- rgb(235, 235, 235, 225, maxColorValue = 255)

# [Loading] - Data ----
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
# ---------------------------------------------------------------------------------------------------
# [Bump Plot] - Team Hex Codes ----
bund_hex <- c("#005CA9", "#DE023F", "#005CA9", "#E1000F", "#46714d", "#009932",
              "#FFFFFF", "#DC052D", "#FDDC02", "#E32221", "#004E95", "#000000",
              "#65B32E", "#918F90", "#ED1C24", "#FDE100", "#1C63B7", "#E32219")

laLiga_hex <- c("#004fa3", "#8AC3EE", "#0067B1", "#0761AF", "#e53027",
                "#E20613", "#0BB363", "#fde607", "#B4053F", "#AD8F1F",
                "#007FC8", "#FFE667", "#A61B2B", "#05642c", "black",
                "#D18816", "#CB3524", "#A50044", "#ffffff", "#F43333")

ligue1_hex <- c("#006eb2", "#d87043", "#009fe3", "#001b50", "#e51b22",
                "#fcd405", "#ffffff", "#ed1c24", "#000000", "#fff200",
                "#008d3f", "#f58113", "#b59a54", "#ee2223", "#6e0f12",
                "#24216a", "#004170", "#2faee0", "#000000", "#c50c46")

pl_hex <- c("#FDB913", "#630F33", "#670E36", "#6CABDD", "#9C824A",
            "#D71920", "#241F20", "#A7A5A6", "#00A650", "#AC944D",
            "#0053A0", "#ffffff", "#fbee23", "#132257", "#e30613",
            "#274488", "#7A263A", "#034694", "#D01317", "#B80102")

serieA_hex <- c("#005395", "#8A1E03", "#742038", "#1B5497", "#00579C",
                "#fd9b00", "#482E92", "#AD1919", "#000000", "#FFFFFF",
                "#002350", "#99834a", "#00A752", "#1E71B8", "#A21C26",
                "#FB090B", "#87D8F7", "#12A0D7", "#8E1F2F", "#010E80")
#
#
# [Bump Plot] - Data Transformation Functions ----
# finding latest md in which a match was played
find_lastWeek <- function(df) {
  # cat(file=stderr(), nrow(df), "\n")
  total_wk <- tail(df$Wk, 1)

  for (i in 1:total_wk) {
    if (all(is.na(df[df$Wk == i,]$HomePoints))) {
      return(i - 1)
    }
  }

  return(total_wk)
}

# creating empty df with 7 columns
create_df <- function() {
  df <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(df) <- c("Team", "Matchday", "Games_Played", "Points", "GD", "Total_Points", "Total_GD")
  return(df)
}

# filling empty df with a row for each team on each md
fill_df <- function(last_week, pull_df) {
  md_count <- count(pull_df[pull_df$Wk == 1,])
  total_matches <- last_week * md_count$n
  f_df <- create_df()

  for (i in 1:total_matches) {
    if (pull_df[i,]$gamePlayed == 1) {
      f_df[nrow(f_df) + 1,] = c(pull_df[i,]$Home, pull_df[i,]$Wk,
                                ifelse(i > md_count, as.numeric(tail(f_df[which(f_df$Team == pull_df[i,]$Home & !is.na(f_df$Games_Played)),]$Games_Played, 1)) + 1,1),
                                pull_df[i,]$HomePoints, pull_df[i,]$HomeGoals - pull_df[i,]$AwayGoals, NA, NA)
      f_df[nrow(f_df) + 1,] = c(pull_df[i,]$Away, pull_df[i,]$Wk,
                                ifelse(i > md_count, as.numeric(tail(f_df[which(f_df$Team == pull_df[i,]$Away & !is.na(f_df$Games_Played)),]$Games_Played, 1)) + 1,1),
                                pull_df[i,]$AwayPoints, pull_df[i,]$AwayGoals - pull_df[i,]$HomeGoals, NA, NA)
      }
    else {
      f_df[nrow(f_df) + 1,] = c(pull_df[i,]$Home, pull_df[i,]$Wk,
                                as.numeric(tail(f_df[which(f_df$Team == pull_df[i,]$Home & !is.na(f_df$Games_Played)),]$Games_Played, 1)),
                                0, 0, NA, NA)
      f_df[nrow(f_df) + 1,] = c(pull_df[i,]$Away, pull_df[i,]$Wk,
                                as.numeric(tail(f_df[which(f_df$Team == pull_df[i,]$Away & !is.na(f_df$Games_Played)),]$Games_Played, 1)),
                                0, 0, NA, NA)
      }
  }
  return(f_df)
  }

# filling in total_points and total_gd based on cumulative sum of rows
fill_pointsAndGd <- function(df, teams) {
  total_matches <- (length(teams) - 1) * 2

  for (i in 1:length(teams)) {
    df[df$Team == teams[i],]$Total_Points = cumsum(df[which(df$Team == teams[i] & df$Games_Played <= total_matches),]$Points)
    df[df$Team == teams[i],]$Total_GD = cumsum(df[which(df$Team == teams[i] & df$Games_Played <= total_matches),]$GD)
  }

  return(df)
}

# group rows by matchday + rank teams by total points + ungroup
add_rank <- function(df, team_count) {
  df <- df %>%
    group_by(Matchday) %>%
    arrange(Total_Points, Total_GD, .by_group = TRUE) %>%
    mutate(Rank = row_number(Matchday)) %>%
    mutate(Rank = -(Rank - (team_count + 1))) %>%
    ungroup()

  return(df)
}

# one big guy that gets the df we need for the bump plot :)
get_bumpData <- function(match_data, last_week) {
  bump_df <- fill_df(last_week, match_data)

  # setting data type of multiple columns to numeric
  bump_df$Games_Played <- as.numeric(bump_df$Games_Played)
  bump_df$Points <- as.numeric(bump_df$Points)
  bump_df$Total_Points <- as.numeric(bump_df$Total_Points)
  bump_df$Matchday <- as.numeric(bump_df$Matchday)
  bump_df$GD <- as.numeric(bump_df$GD)

  teams <- unique(bump_df$Team)
  bump_df <- fill_pointsAndGd(bump_df, teams)
  bump_df <- add_rank(bump_df, length(unique(bump_df$Team)))
  return(bump_df)
}

# [Bump Plot] - Functions ----
get_leaguePalette <- function(comp) {
  if (comp == "La Liga") {
    return(laLiga_hex)
  } else if (comp == "Ligue 1") {
    return(ligue1_hex)
  } else if (comp == "Premier League") {
    return(pl_hex)
  } else if (comp == "Serie A") {
    return(serieA_hex)
  } else {
    return(bund_hex)
  }
}

# [Bump Plot] - Plot Output ----
getBumpPlot <- function(df, md_start, md_end, teams, sel_teams, league_palette, bump_rank, background) {
  if (length(sel_teams) >= 1) {
    df %>%
      ggplot(aes(x = Matchday, y = Rank, colour = Team)) +
      geom_bump(smooth = 5, size = 3, lineend = "round") +
      geom_point(size = 5) +
      scale_colour_manual(
        breaks = teams,
        values = league_palette
      ) +
      geom_text(data = df %>%
                  dplyr::filter(Matchday == md_start), aes(label = str_trim(Team, "both"), md_start - .4),
                fontface = "bold", size = 11, hjust = 1, family = "Roboto") +
      geom_text(data = df %>%
                  dplyr::filter(Matchday == md_end), aes(label = Team, x = md_end + .4),
                fontface = "bold", size = 11, hjust = 0, family = "Roboto") +
      gghighlight(any(Team == sel_teams),
                  use_direct_label = bump_rank,
                  label_key = Rank,
                  label_params = list(colour = "black", size = 10),
                  unhighlighted_params = list(alpha = 0.25)) +
      scale_y_reverse() +
      scale_x_continuous(limits = c(md_start - 1.5, md_end + 1.5)) +
      theme(
        legend.position = "none",
        
        panel.grid = element_blank(),
        panel.background = element_rect(fill = substring(background, 1, 7), colour = "black",
                                        size = 5),        
        plot.title = element_text(size = 45, face = "bold", hjust = 0.5),
        
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
  }
  else {
    df %>%
      ggplot(aes(x = Matchday, y = Rank, colour = Team)) +
      geom_path_interactive(size = 3, lineend = "round", aes(data_id = Team)) + 
      geom_point_interactive(size = 5, aes(data_id = Team)) + 
      geom_text_interactive(data = df %>% dplyr::filter(Matchday == md_start),
                            aes(label = StrAlign(Team, sep = "\\r"), x = md_start - .4, data_id = Team),
                            fontface = "bold", size = 11, hjust = 1, family = "Roboto") +
      geom_text_interactive(data = df %>% dplyr::filter(Matchday == md_end), 
                            aes(label = Team, x = md_end + .4, data_id = Team), 
                            fontface = "bold", size = 11, hjust = 0, family = "Roboto") + 
      scale_colour_manual(
        breaks = teams,
        values = league_palette
      ) +
      scale_y_reverse() +
      scale_x_continuous(limits = c(md_start - 1.5, md_end + 1.5), expand = c(0.01, 1)) +
      theme(
        legend.position = "none",
        
        panel.grid = element_blank(),
        panel.background = element_rect(fill = substring(background, 1, 7), colour = "black",
                                        size = 5),
        
        # plot.background = element_rect(substring(background, 1, 7)),
        
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        # axis.line = element_line(colour = "black", size = 3)
      )
  }
}
# ----------------------------------------------------------------
# [GK Zone] - Plot Output(s) ----
getGkZonePlot <- function(vizSelected, gkData) {
  if (vizSelected == "Who's Beating the Model?") {
    gkZoneModelPlot(gkData)
  }
  else if (vizSelected == "Getting Out of the Box") {
    gkZoneSweeperPlot(gkData)
  }
  else {
    gkZoneCrossPlot(gkData)
  }
}

gkZoneModelPlot <- function(data) {
  ggplot(data = data, aes(x = SoTA, y = PSxG_minus_GA, colour = Comp)) +
    geom_vline(xintercept = median(data$SoTA), colour = "white", linetype = "dashed") +
    geom_hline(yintercept = 0, colour = "white", linetype = "dashed") +
    geom_point_interactive(aes(size = PSxG_per_SoT_Expected, tooltip = paste(Player, " - ", Squad, "\n",
                                                         "PSxG - GA: ", PSxG_minus_GA, "\n",
                                                         "PSxG: ", PSxG_Expected, "\n",
                                                         "GA: ", GA, "\n",
                                                         "SoTA: ", SoTA, "\n",
                                                         "PSxG/SoTA: ", PSxG_per_SoT_Expected,
                                                         sep = ""),
                               data_id = Player)) +
    scale_size_continuous(range = c(2, 5)) +
    scale_colour_manual_interactive(breaks = c("Bundesliga", "La Liga", "Ligue 1", "Premier League", "Serie A"),
                        values = c(met.brewer("Isfahan2")[1], met.brewer("Isfahan2")[2], met.brewer("Isfahan2")[3], met.brewer("Isfahan2")[4], met.brewer("Isfahan2")[5])) +
    labs(title = "Who's Beating the Model? (And Does it Matter?)",
         subtitle = "Shot Stopping Ability by Shots on Target Against for Big 5 GKs",
         tag = "Data: StatsBomb via fbref.com (GK's with > 900 minutes played in 21/22 season)") +
    xlab("Shots on Target Against") +
    ylab("Post-Shot Expected Goals - Goals Allowed") +
    guides(
      size = guide_legend(
        label.hjust = 0.5,
        title = "PSxG/SoTA",
        title.theme = element_text(size = 10, colour = 
                                     "white", hjust = 0.5,
                                   face = "bold"),
        title.position = "top",
        override.aes = list(colour = "white")
        ),
      colour = guide_legend(
        override.aes = list(size = 5),
        title = "Competition",
        title.theme = element_text(size = 10, colour = 
                                     "white", hjust = 0.5,
                                   face = "bold"))
      ) +
    theme(
      text = element_text(colour = "white", family = "Roboto"),
      title = element_text(size = 14, margin = margin(2.5, 0, 0, 0), face = "bold"),

      plot.background = element_rect(fill = "black"),
      plot.title = element_text(margin = margin(6.25, 0, 3.75, 0)),
      plot.subtitle = element_text(face = "plain", margin = margin(0, 0, 10, 0)),
      plot.tag = element_text(face = "plain", size = 10),
      plot.tag.position = c(0.325, 0.00875),

      axis.title = element_text(colour = "white", size = 12, hjust = 0.5),
      axis.title.y = element_text(margin = margin(0, 15, 0, 15)),
      axis.title.x = element_text(margin = margin(15, 0, 25, 0)),
      axis.text = element_text(colour = "white"),
      axis.ticks = element_line(colour = "white", linetype = "longdash"),
      axis.line = element_line(colour = "white"),

      panel.background = element_rect(colour = "black", fill = "black"),
      panel.grid = element_blank(),

      legend.background = element_rect(colour = "white", fill = "black"),
      legend.text = element_text(colour = "white", margin = margin(0, 1, 0, 0)),
      legend.key = element_rect(fill = "black"),
      legend.title.align = 0.5)
}
gkZoneSweeperPlot <- function(data) {
  ggplot(data = data, aes(x = AvgDist_Sweeper, y = OPA_Sweeper_per_90, colour = Comp)) +
    annotate(geom = "text", x = 18, y = 0.25, label = "Edge of Box ->", hjust = 1.075, colour = "white", fontface = "bold", family = "Roboto") +
    annotate(geom = "text", x = 12, y = 1.41, label = "<- Penalty Spot", hjust = -.075, colour = "white", fontface = "bold", family = "Roboto") +
    geom_vline(xintercept = 12, colour = "white", linetype = "dashed") +
    geom_vline(xintercept = 18, colour = "white", linetype = "dashed") +

    geom_point_interactive(size = 3, aes(tooltip = paste(Player, " - ", Squad, "\n",
                                                         "Avg Distance: ", AvgDist_Sweeper, "\n",
                                                         "Def Actions per 90: ", OPA_Sweeper_per_90, "\n",
                                                         sep = ""),
                               data_id = Player)) +
    # geom_label_repel(data = data %>% filter(Player == "Manuel Neuer"), aes(label = Player), show.legend = FALSE,
    #                  color = met.brewer("Isfahan2")[1], fill = "black", fontface = "bold", nudge_x = -.65, nudge_y = -.2) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 2), breaks = c(0, 0.5, 1, 1.5, 2)) +
    scale_x_continuous(breaks = c(12, 14, 16, 18, 20)) +
  
    scale_colour_manual(breaks = c("Bundesliga", "La Liga", "Ligue 1", "Premier League", "Serie A"),
                        values = c(met.brewer("Isfahan2")[1], met.brewer("Isfahan2")[2], met.brewer("Isfahan2")[3], met.brewer("Isfahan2")[4], met.brewer("Isfahan2")[5])) +
    guides(size = "none",
           colour = guide_legend(
             title = "Competition",
             title.theme = element_text(size = 10, colour = 
                                          "white", hjust = 0.5,
                                        face = "bold"),
             override.aes = list(size = 5)
           )) +
    labs(title = "Sweeper or Nah?",
         subtitle = "Sweeper Activity by Distance From Goal of Defensive Actions for Big 5 GKs",
         tag = "Data: StatsBomb via fbref.com (GK's with > 900 minutes played in 21/22 season)") +

    ylab("Actions Beyond Penalty Area per 90") +
    xlab("Average Distance from Goal of All Defensive Actions (Yards)") +

    theme(
      text = element_text(colour = "white", family = "Roboto"),
      
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "black"),

      plot.background = element_rect(fill = "black", colour = "black"),
      plot.title = element_text(margin = margin(6.25,0,3.75,0), face = "bold", size = 18),
      plot.subtitle = element_text(margin = margin(0,0,10,0), face = "plain", size = 14),
      plot.tag = element_text(face = "plain", size = 10),
      plot.tag.position = c(0.325, 0.00875),
      
      legend.background = element_rect(colour = "white", fill = "black"),
      legend.key = element_rect(fill = "black"),
      legend.title.align = 0.5,
      legend.position = c(.875, .3),

      axis.text = element_text(colour = "white"),
      axis.line = element_line(colour = "white"),
      axis.title = element_text(colour = "white", size = 12, face = "bold"),
      axis.ticks = element_line(colour = "white"),
      axis.title.y = element_text(margin = margin(0, 16.25, 0, 15)),
      axis.title.x = element_text(margin = margin(15, 0, 25, 0)),
    )
}

# [GK Zone] - Viz Text(s) ----
getGkZoneText <- function(vizSelected) {
  if (vizSelected == "Who's Beating the Model?") {
    gkModelText
  }
  else {
    gkSweeperText
  }
}

gkModelText <- HTML("<p>This plot, inspired by <a href = 'https://fivethirtyeight.com/features/the-most-valuable-soccer-player-in-america-is-a-goalkeeper/'> John Muller</a>,
                    attempts to showcase how goalkeepers have performed against Statsbomb's <a href = 'https://statsbomb.com/2018/11/a-new-way-to-measure-keepers-s
                    hot-stopping-post-shot-expected-goals/'> Post-Shot Expected Goals Model</a> (PSxG). The PSxG Model is similar to Statsbomb's Expected Goals Model, but 
                    is designed to evaluate goalkeepers rather than shooters. This change in focus is accomplished by restricting the shot sample to those on target, and using 
                    post-shot information, such as shot speed and trajectory, to train the model. This plot's main statistic, PSxG - Goals Allowed, allows us to evaluate goalkeeper's 
                    shot-stopping ability. Positive values suggest an above average shot-stopping ability (or better luck). Another useful statistic for analysis, PSxG
                    per Shot on Target (PSxG/SoTa), is represented in the size of each point, and can be used to compare the quality of shots keepers have faced.</p>")

gkSweeperText <- paste0("Hi, here's some text")

# ----------------------------------------------------------------
# [XG Time] - Data Transformation ----
# transforming big 5 data (calculating rolling xG averages for and against + adding gameNum column)
big5ToXG <- function(df, team, rollN = 6) {
  df_trans <- df %>%
    filter(Home == team | Away == team) %>%
    select(Wk, Date, Home, HomeGoals, Home_xG, Away, AwayGoals, Away_xG) %>%
    drop_na() %>%
    mutate(HomeAway = if_else(Home == team, "Home", "Away"),
           xgFor = if_else(Home == team, Home_xG, Away_xG),
           xgAgainst = if_else(Home == team, Away_xG, Home_xG),
           rollFor = round(rollmean(x = xgFor, k = rollN, na.pad = TRUE, align = "right"),2),
           rollAgainst = round(rollmean(x = xgAgainst, k = rollN, na.pad = TRUE, align = "right"),2),
           selOpp = "Selected")
  
  for (i in 1:(rollN-1)) {
    df_trans[i,]$rollFor = round(mean(df_trans[1:i,]$xgFor), 2)
    df_trans[i,]$rollAgainst = round(mean(df_trans[1:i,]$xgAgainst), 2)
  }
  
  return(df_trans %>%
           arrange(Date) %>%
           mutate(gameNum = 1:nrow(df_trans)))
}

# adding a row for each week to prepare for interpolation
tidyXGData <- function(df) {
  df %>%
    group_by(Wk) %>%
    summarise(Date = last(Date), Home = last(Home), HomeGoals = last(HomeGoals), Home_xG = last(Home_xG), Away = last(Away),
              AwayGoals = last(AwayGoals), Away_xG = last(Away_xG), HomeAway = last(HomeAway), xgFor = last(xgFor),
              xgAgainst = last(xgAgainst), gameNum = last(gameNum), rollFor = last(rollFor), rollAgainst = last(rollAgainst)) %>%
    mutate(selOpp = "Opposition") %>%
    bind_rows(df, .) %>%
    arrange(Wk)
}

# interpolating along both rollFor + rollAgainst line segments to make geom_ribbon() gaps indistinguishable
XGDataInterp <- function(df) {
  df %>%
    split(.$selOpp) %>%
    map_df(~data.frame(sel = approx(.x$gameNum, .x$rollFor, n = 80),
                       opp = approx(.x$gameNum, .x$rollAgainst, n = 80),
                       team = .x$selOpp[1]))
}

# [XG Time] - Plot Output(s) ----
xgRollPlot <- function(df, team, comp, bund) {
  ggplot(data = df, aes(x = sel.x, y = max(sel.y, opp.y))) +
      annotate(geom = "rect", xmin = 1.05, xmax = 6, ymin = -Inf, ymax = max(df$sel.y, df$opp.y) + 0.24,
               fill = "#ECECEC", alpha = 0.85) +
      geom_ribbon(aes(ymin = sel.y, ymax = pmin(sel.y, opp.y)), fill = "#5E1208", alpha = 0.5) + 
      geom_ribbon(aes(ymin = opp.y, ymax = pmin(sel.y, opp.y)), fill = "#009782", alpha = 0.5) +
      geom_line(aes(y = sel.y, colour = "#009782"), size = 1.5) +
      geom_line(aes(y = opp.y, colour = "#5E1208"), size = 1.5) +
      scale_x_continuous(
        expand = c(0,0), 
        limits = c(1, if_else(bund == TRUE, 36, 38)),
        breaks = seq(5, 35, by = 5)) +
      scale_y_continuous(
        expand = c(0,0),
        limits = c((min(df$sel.y, df$opp.y) - 0.25), (max(df$sel.y, df$opp.y) + 0.25)), 
        breaks = seq(0, (max(df$sel.y, df$opp.y) + 1), by = 0.2)) +
      
      labs(title = paste(team, "'s Underlying Expected Performance", sep = ""),
           subtitle = glue("<span>6 game rolling average of {team}'s
                            <span style = 'color:#5E1208'>**expected goals for**</span> and 
                            <span style = 'color:#009782'>**expected goals against**</span>
                            in the {comp} for 2021/2022</span>"),
           caption = "Data from Statsbomb via fbref.com. This data includes penalties. The first six games shown are a partial average. Recreation of a @petermckeever viz using R by @biglake402.") +
      xlab("Matchday") + ylab("Rolling xG") +
      scale_color_manual(labels = c(paste("xG for ", team, sep = ""), paste("xG against ", team, sep = "")),
                         values = c("#5E1208", "#009782")) +
      
      theme(
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid = element_line(colour = colGrid),
        panel.grid.minor = element_blank(),
        
        text = element_text(family = "Roboto"),
        
        title = element_text(colour = "black"),
        plot.title = element_text(face = "bold", size = 20, margin = margin(6.25, 0, 3.75, 0)),
        plot.subtitle = element_markdown(family = "Roboto", face = "plain", size = 16, lineheight = 0.625, ),
        plot.caption = element_text(hjust = 0, size = 12.5, margin = margin(0,0,0,0)),

        axis.ticks = element_blank(),
        axis.text = element_text(colour = "black", size = 12.5),
        axis.text.y = element_text(margin = margin(0, 7.5, 0, 0)),
        axis.title = element_text(face = "bold", size = 18),
        axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(margin = margin(20, 0, 20, 0)),
        
        legend.position = "none"
      )
}