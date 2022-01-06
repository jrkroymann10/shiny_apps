# load libraries, data -----------------------------------------------------------------
library(readr)
library(worldfootballR)
library(tidyverse)
library(ggbump)
library(readr)
library(gghighlight)
library(ggplot2)

match_data <- get_match_results(country = "ENG", gender = "M", season_end_year = "2022", tier = "1st")

team_values <- c("All", "Arsenal", "Aston Villa", "Brentford", "Brighton", "Burnley", "Chelsea",
                 "Everton", "Leeds", "Leicester", "Liverpool", "Man City", "Man Utd", "Newcastle",
                 "Norwich", "Palace", "Southampton", "Tottenham", "Watford", "West Ham", "Wolves")

md_values <- 1:tail(match_data[!is.na(match_data$Home_xG),]$Wk, 1)
# UI Stuff -----------------------------------------------------------------------------------------------------------------
# page 1 - introduction
intro_panel <- tabPanel(
  "Introduction",
  
  titlePanel("Welcome to Joe's Premier League Dashboards!")
)

# page 2 - table bump plot
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

# Server Functions ---------------------------------------------------------------------------------------------------------
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
# -------------------------------------------------------------------------------------------
