library(worldfootballR)
library(tidyverse)
library(ggbump)

prem_2021 <- get_match_results(country = "ENG", gender = "M", season_end_year = "2022", tier = "1st")

transform_matchResults <- function(fbref_mr) {
  fbref_mr %>%
    mutate(HomePoints = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals < AwayGoals, 0, 1)),
           AwayPoints = ifelse(HomeGoals > AwayGoals, 0, ifelse(HomeGoals < AwayGoals, 3, 1))) %>%
    select(Date, Wk, Home, HomePoints, HomeGoals, Away, AwayPoints, AwayGoals)
}

find_lastWeek <- function(df) {
  for (i in 1:38) {
    if (nrow(df[which(df$Wk == i & is.na(df$HomePoints)),]) == 10) {
      return(i - 1)
    }
  }
  return(38)
}

create_df <- function() {
  df <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(df) <- c("Team", "Matchday", "Games_Played", "Points", "GD", "Total_Points", "Total_GD")
  return(df)
}

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
    mutate(rank = rank(-Total_Points, ties.method = "random")) %>%
    ungroup()
  
  return(df)
}

# bump plot
get_bumpPlot <- function(df, teams) {
  df %>%
    ggplot(aes(Matchday, rank, group = Team, colour = Team)) +
    geom_bump(smooth = 5, size = 1.5, lineend = "round") + 
    geom_point(size = 2.5) +
    scale_y_reverse() + 
    scale_colour_manual(
      breaks = rev(teams),
      values = c("#6CABDD", "#132257", "#7A263A", "#241F20", "#D01317",
                 "#00A650", "#D71920", "#274488", "#1B458F", "#034694", 
                 "#670E36", "#fbee23", "#005DAA", "#630F33", "#FDB913", 
                 "#0053A0", "#AC944D", "#B80102", "#9C824A", "#e30613")) +
    theme_minimal() +
    theme(
      legend.position = "none",
      
      panel.grid = element_blank(),
      
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    )
}

prem_2021 <- transform_matchResults(prem_2021)
last_week <- find_lastWeek(prem_2021)
td <- fill_df(prem_2021, last_week)

# setting data type of multiple columns to numeric
td$Games_Played <- as.numeric(td$Games_Played)
td$Points <- as.numeric(td$Points)
td$Total_Points <- as.numeric(td$Total_Points)
td$Matchday <- as.numeric(td$Matchday)
td$GD <- as.numeric(td$GD)

teams <- unique(td$Team)
td <- fill_pointsAndGd(td, teams)
td <- add_rank(td)
get_bumpPlot(td, teams)
