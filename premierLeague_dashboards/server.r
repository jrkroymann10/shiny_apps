# load libraries, data, functions -----------------------------------------------------------------
library(worldfootballR)
library(tidyverse)
library(ggbump)
library(readr)
library(gghighlight)
library(ggplot2)

match_data <- read_csv("data/pl_matchdata.csv")


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
    ungroup()
  
  return(df)
}

# shortening team names to 3 letter codes
shorten_teamnames <- function(df) {
  df <- df %>%
    mutate(Team = replace(Team, Team == "Arsenal", "ARS"),
           Team = replace(Team, Team == "Aston Villa", "AVL"),
           Team = replace(Team, Team == "Brentford", "BRE"),
           Team = replace(Team, Team == "Brighton", "BRI"),
           Team = replace(Team, Team == "Burnley", "BUR"),
           Team = replace(Team, Team == "Chelsea", "CHE"),
           Team = replace(Team, Team == "Crystal Palace", "CRY"),
           Team = replace(Team, Team == "Everton", "EVE"),
           Team = replace(Team, Team == "Leeds United", "LEE"),
           Team = replace(Team, Team == "Leicester City", "LEI"),
           Team = replace(Team, Team == "Liverpool", "LFC"),
           Team = replace(Team, Team == "Manchester City", "MCI"),
           Team = replace(Team, Team == "Manchester Utd", "MUN"),
           Team = replace(Team, Team == "Newcastle Utd", "NEW"),
           Team = replace(Team, Team == "Norwich City", "NOR"),
           Team = replace(Team, Team == "Southampton", "SOU"),
           Team = replace(Team, Team == "Tottenham", "TOT"),
           Team = replace(Team, Team == "Watford", "WAT"),
           Team = replace(Team, Team == "West Ham", "WHU"),
           Team = replace(Team, Team == "Wolves", "WOL"))
  
  return(df)
}

# bump plot
get_bumpPlot <- function(df, teams) {
  df %>%
    ggplot(aes(Matchday, Rank, group = Team, colour = Team)) +
    geom_bump(smooth = 5, size = 2, lineend = "round") + 
    geom_point(size = 3) +
    scale_colour_manual(
      breaks = teams,                             
      values = c( #FDB913    #630F33    #670E36    #6CABDD    #9C824A
        "#FDB913", "#630F33", "#670E36", "#6CABDD", "#9C824A",
        
        #D71920    #241F20    #1B458F    #00A650    #AC944D
        "#D71920", "#241F20", "#A7A5A6", "#00A650", "#AC944D",
        
        #0053A0    #005DAA    #fbee23    #132257    #e30613
        "#0053A0", "#ffffff", "#fbee23", "#132257", "#e30613", 
        
        #274488    #7A263A    #034694    #D01317    #B80102
        "#274488", "#7A263A", "#034694", "#D01317", "#B80102")) +
    geom_text(data = df %>% 
                filter(Matchday == 1),
              aes(label = Team, x = 0), hjust = 0.5, fontface = "bold", size = 4.5) +
    geom_text(data = df %>% 
                filter(Matchday == max(Matchday)),
              aes(label = Team, x = max(Matchday) + 1), hjust = 0.5, fontface = "bold", size = 4.5) +
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

# ----------------------------------------------------------------------------------

server <- function(input, output) {
  output$plot <- renderPlot({
    prem_2021 <- transform_matchResults(match_data)
    last_week <- find_lastWeek(prem_2021)
    bump_df <- fill_df(prem_2021, last_week)
    
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
    
    teams <- unique(bump_df$Team)
    get_bumpPlot(bump_df, teams)
    
  })
}