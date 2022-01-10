# load libraries, data -----------------------------------------------------------------
library(readr)
library(worldfootballR)
library(tidyverse)
library(ggbump)
library(readr)
library(gghighlight)
library(ggplot2)
library(plotly)

match_data <- get_match_results(country = "ENG", gender = "M", season_end_year = "2022", tier = "1st")

gk_data <- fb_big5_advanced_season_stats(season_end_year = 2022,
                                         stat_type = "keepers",
                                         team_or_player = "player")

gk_data_adv <- fb_big5_advanced_season_stats(season_end_year = 2022,
                                             stat_type = "keepers_adv",
                                             team_or_player = "player")

team_values <- c("All", "Arsenal", "Aston Villa", "Brentford", "Brighton", "Burnley", "Chelsea",
                 "Everton", "Leeds", "Leicester", "Liverpool", "Man City", "Man Utd", "Newcastle",
                 "Norwich", "Palace", "Southampton", "Tottenham", "Watford", "West Ham", "Wolves")

md_values <- 1:tail(match_data[!is.na(match_data$Home_xG),]$Wk, 1)

# [Server] GK Viz Functions ----
get_plKeeper_adv <- function(gk_data, gk_data_adv) {
  pl_keepers <- gk_data %>%
    filter(Comp == "Premier League", Min_Playing > 900)
  
  pl_keepers_adv <- merge(gk_data_adv, pl_keepers)
  colnames(pl_keepers_adv)[colnames(pl_keepers_adv) == "#OPA_Sweeper"] <- "OPA_Sweeper"
  colnames(pl_keepers_adv)[colnames(pl_keepers_adv) == "#OPA_per_90_Sweeper"] <- "OPA_Sweeper_per_90"
  colnames(pl_keepers_adv)[colnames(pl_keepers_adv) == "_per_90_Expected"] <- "per_90"
  
  return(pl_keepers_adv)
}

gk_model_plot <- function(data) {
  ggplotly(ggplot(data = data, aes(x = SoTA, y = per_90, colour = Save_percent,
                                             text = paste(Player, " (", Squad, ")", "\n",
                                                          "PSxG - GA per 90: ", per_90, "\n",
                                                          "Shots on Target Against: ", SoTA, "\n",
                                                          "Goals Allowed: ", GA, "\n",
                                                          sep = ""))) +
             xlim(40, 100) +
             ylim(-0.4, 0.4) +
             geom_vline(xintercept = 70) +
             geom_hline(yintercept = 0) +
             geom_point(aes(size = GA)) + 
             ylab("Post-Shot Expected Goals - Goals Allowed per 90") +
             xlab("Shots on Target Against") +
             labs(title = "Who's Beating the Model? (And Does it Matter?)",
                  subtitle = "PSxG - Goals Allowed per 90 by Shots on Target for PL Goalkeepers with > 900 minutes played") +
             theme_minimal() +
             theme(title = element_text(size = 14),
                   axis.title = element_text(size = 10)),
           
           tooltip = "text"
  )
}

gk_sweeper_plot <- function(data) {
  ggplotly(ggplot(data = data, aes(x = OPA_Sweeper_per_90, y = AvgDist_Sweeper,
                                   text = paste(Player, " (", Squad,")",
                                                          sep = ""))) +
             geom_point(colour = "#1DB954", size = 3) +
             scale_size_identity() +
             
             scale_x_continuous(expand = c(0,0), limits = c(0, 1.55)) +
             scale_y_continuous(expand = c(0,0), limits = c(11.25, 18.1)) + 
             
             labs(title = "Sweeper or Nah?",
                  subtitle = "Distance of Defensive Actions from Goal by Activity Outside of the Penalty Area") +
             
             theme(
               legend.position = "none",
               
               panel.grid = element_blank(),
               panel.background = element_rect(fill = "black"),
               text = element_text(colour = "white"),
               
               plot.background = element_rect(fill = "black"),
               
               axis.line = element_line(colour = "white"),
               axis.text = element_text(colour = "white"),
               axis.title = element_text(colour = "white"),
               axis.ticks = element_line(colour = "white")
             ),
           tooltip = "text")
}


# [Server] Bump Plot Functions ------------------------------------------------------------------------------------------------------
transform_matchResults <- function(fbref_mr) {
  fbref_mr %>%
    mutate(HomePoints = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals < AwayGoals, 0, 1)),
           AwayPoints = ifelse(HomeGoals > AwayGoals, 0, ifelse(HomeGoals < AwayGoals, 3, 1))) %>%
    select(Date, Wk, Home, HomePoints, HomeGoals, Away, AwayPoints, AwayGoals)
}

# finding latest md in which a match was played
find_lastWeek <- function(df) {
  for (i in 1:38) {
    if (nrow(df[which(df$Wk == i & is.na(df$HomePoints)),]) == 10) {
      return(i - 1)
    }
  }
  return(38)
}

# creating empty df with 7 columnds
create_df <- function() {
  df <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(df) <- c("Team", "Matchday", "Games_Played", "Points", "GD", "Total_Points", "Total_GD")
  return(df)
}

# filling empty df with a row for each team on each md
fill_df <- function(pull_df, last_week) {
  f_df <- create_df()
  
  for (i in 1:last_week) {
    temp_data <- pull_df[pull_df$Wk == i,]
    
    for (j in 1:nrow(temp_data)) {
      if (!is.na(temp_data[j,]$HomePoints)) {
        f_df[nrow(f_df) + 1,] = c(temp_data[j,]$Home, temp_data[j,]$Wk, 
                                  ifelse(i > 1, as.numeric(tail(f_df[which(f_df$Team == temp_data[j,]$Home & !is.na(f_df$Games_Played)),]$Games_Played, 1)) + 1,1), 
                                  temp_data[j,]$HomePoints, temp_data[j,]$HomeGoals - temp_data[j,]$AwayGoals, NA, NA)
        
        f_df[nrow(f_df) + 1,] = c(temp_data[j,]$Away, temp_data[j,]$Wk, 
                                  ifelse(i > 1, as.numeric(tail(f_df[which(f_df$Team == temp_data[j,]$Away & !is.na(f_df$Games_Played)),]$Games_Played, 1)) + 1,1), 
                                  temp_data[j,]$AwayPoints, temp_data[j,]$AwayGoals - temp_data[j,]$HomeGoals, NA, NA)
      }
      
      else {
        f_df[nrow(f_df) + 1,] = c(temp_data[j,]$Home, temp_data[j,]$Wk,
                                  as.numeric(tail(f_df[which(f_df$Team == temp_data[j,]$Home & !is.na(f_df$Games_Played)),]$Games_Played, 1)),
                                  0, 0, NA, NA)
        
        f_df[nrow(f_df) + 1,] = c(temp_data[j,]$Away, temp_data[j,]$Wk, 
                                  as.numeric(tail(f_df[which(f_df$Team == temp_data[j,]$Away & !is.na(f_df$Games_Played)),]$Games_Played, 1)), 
                                  0, 0, NA, NA)
      }
    }
  }
  return(f_df)
}

# filling in total_points and total_gd based on cumulative sum of rows
fill_pointsAndGd <- function(df, teams) {
  for (i in 1:20) {
    df[df$Team == teams[i],]$Total_Points = cumsum(df[which(df$Team == teams[i] & df$Games_Played <= 38),]$Points)
    df[df$Team == teams[i],]$Total_GD = cumsum(df[which(df$Team == teams[i] & df$Games_Played <= 38),]$GD)
  }
  
  return(df)
}

# group rows by matchday + rank teams by total points + ungroup
add_rank <- function(df) {
  df <- df %>%
    group_by(Matchday) %>%
    arrange(Total_Points, Total_GD, .by_group = TRUE) %>%
    mutate(Rank = row_number(Matchday)) %>%
    mutate(Rank = -(Rank - 21)) %>%
    ungroup()
  
  return(df)
}

# shortening team names
shorten_teamnames <- function(df) {
  df <- df %>%
    mutate(Team = replace(Team, Team == "Arsenal", "Arsenal"),
           Team = replace(Team, Team == "Aston Villa", "Aston Villa"),
           Team = replace(Team, Team == "Brentford", "Brentford"),
           Team = replace(Team, Team == "Brighton", "Brighton"),
           Team = replace(Team, Team == "Burnley", "Burnley"),
           Team = replace(Team, Team == "Chelsea", "Chelsea"),
           Team = replace(Team, Team == "Crystal Palace", "Palace"),
           Team = replace(Team, Team == "Everton", "Everton"),
           Team = replace(Team, Team == "Leeds United", "Leeds"),
           Team = replace(Team, Team == "Leicester City", "Leicester"),
           Team = replace(Team, Team == "Liverpool", "Liverpool"),
           Team = replace(Team, Team == "Manchester City", "Man City"),
           Team = replace(Team, Team == "Manchester Utd", "Man Utd"),
           Team = replace(Team, Team == "Newcastle Utd", "Newcastle"),
           Team = replace(Team, Team == "Norwich City", "Norwich"),
           Team = replace(Team, Team == "Southampton", "Southampton"),
           Team = replace(Team, Team == "Tottenham", "Tottenham"),
           Team = replace(Team, Team == "Watford", "Watford"),
           Team = replace(Team, Team == "West Ham", "West Ham"),
           Team = replace(Team, Team == "Wolves", "Wolves"))
  
  return(df)
}

# one big guy that gets the df we need for the bump plot :)
get_bumpData <- function(match_data) {
  pl_data <- transform_matchResults(match_data)
  last_week <- find_lastWeek(pl_data)
  bump_df <- fill_df(pl_data, last_week)
  
  # setting data type of multiple columns to numeric
  bump_df$Games_Played <- as.numeric(bump_df$Games_Played)
  bump_df$Points <- as.numeric(bump_df$Points)
  bump_df$Total_Points <- as.numeric(bump_df$Total_Points)
  bump_df$Matchday <- as.numeric(bump_df$Matchday)
  bump_df$GD <- as.numeric(bump_df$GD)
  
  teams <- unique(bump_df$Team)
  bump_df <- fill_pointsAndGd(bump_df, teams)
  bump_df <- add_rank(bump_df)
  bump_df <- shorten_teamnames(bump_df)
}

# bump plot
get_bumpPlot <- function(df, teams, h_team, start_md, end_md) {
  start_md <- as.numeric(start_md)
  end_md <- as.numeric(end_md) 
  
  df <- df %>%
    filter(Matchday <= end_md & Matchday >= start_md)
  
  if (h_team == "All") {
    df %>%
      ggplot(aes(Matchday, Rank, group = Team, colour = Team)) +
      geom_bump(smooth = 5, size = 2, lineend = "round") + 
      geom_point(size = 3.75) +
      scale_colour_manual(
        breaks = teams,                             
        values = c("#FDB913", "#630F33", "#670E36", "#6CABDD", "#9C824A",
                   "#D71920", "#241F20", "#A7A5A6", "#00A650", "#AC944D",
                   "#0053A0", "#ffffff", "#fbee23", "#132257", "#e30613", 
                   "#274488", "#7A263A", "#034694", "#D01317", "#B80102")) +
      geom_text(data = df %>% 
                  filter(Matchday == start_md),
                aes(label = Team, x = start_md - 1), hjust = 0.5, fontface = "bold", size = 5) +
      geom_text(data = df %>% 
                  filter(Matchday == end_md),
                aes(label = Team, x = end_md + 1), hjust = 0.5, fontface = "bold", size = 5) +
      scale_y_reverse() +
      theme_minimal() +
      theme(
        legend.position = "none",
        
        panel.grid = element_blank(),
        panel.background = element_rect("#D3D3D3"),
        
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
  }
  
  
  else {
    df %>%
      ggplot(aes(Matchday, Rank, group = Team, colour = Team)) +
      geom_bump(smooth = 5, size = 2, lineend = "round") + 
      geom_point(size = 3) +
      scale_colour_manual(
        breaks = teams,                             
        values = c("#FDB913", "#630F33", "#670E36", "#6CABDD", "#9C824A",
                   "#D71920", "#241F20", "#A7A5A6", "#00A650", "#AC944D",
                   "#0053A0", "#ffffff", "#fbee23", "#132257", "#e30613", 
                   "#274488", "#7A263A", "#034694", "#D01317", "#B80102")
      ) +
      geom_text(data = df %>% 
                  filter(Matchday == start_md),
                aes(label = Team, start_md - 1), fontface = "bold", size = 4.5) +
      geom_text(data = df %>% 
                  filter(Matchday == end_md),
                aes(label = Team, x = end_md + 1), fontface = "bold", size = 4.5) +
      gghighlight(Team == h_team,
                  use_direct_label = FALSE,
                  unhighlighted_params = list(colour = NULL, alpha = 0.1)) +
      scale_y_reverse() +
      theme_minimal() +
      theme(
        legend.position = "none",
        
        panel.grid = element_blank(),
        panel.background = element_rect("#D3D3D3"),
        
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
  }
}