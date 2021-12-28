library(worldfootballR)
library(tidyverse)
library(ggbump)

prem_2021 <- get_match_results(country = "ENG", gender = "M", season_end_year = "2022", tier = "1st")

prem_2021 <- prem_2021 %>%
  mutate(HomePoints = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals < AwayGoals, 0, 1)),
         AwayPoints = ifelse(HomeGoals > AwayGoals, 0, ifelse(HomeGoals < AwayGoals, 3, 1))) %>%
  select(Date, Wk, Home, HomePoints, HomeGoals, Away, AwayPoints, AwayGoals)

find_lastWeek <- function(df) {
  for (i in 1:38) {
    if (nrow(df[which(df$Wk == i & is.na(df$HomePoints)),]) == 10) {
      return(i - 1)
    }
  }
  
  return(38)
}

last_week <- find_lastWeek(prem_2021)

# creating dataframe to be filled
t_d <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(t_d) <- c("Team", "Matchday", "Games_Played", "Points", "GD", "Total_Points", "Total_GD")

for (i in 1:last_week) {
  temp_data <- prem_2021[prem_2021$Wk == i,]
  
  for (j in 1:nrow(temp_data)) {
    if (!is.na(temp_data[j,]$HomePoints)) {
      t_d[nrow(t_d) + 1,] = c(temp_data[j,]$Home, temp_data[j,]$Wk, 
                              ifelse(i > 1, as.numeric(tail(t_d[which(t_d$Team == temp_data[j,]$Home & !is.na(t_d$Games_Played)),]$Games_Played, 1)) + 1,1), 
                              temp_data[j,]$HomePoints, temp_data[j,]$HomeGoals - temp_data[j,]$AwayGoals, NA, NA)
      
      t_d[nrow(t_d) + 1,] = c(temp_data[j,]$Away, temp_data[j,]$Wk, 
                              ifelse(i > 1, as.numeric(tail(t_d[which(t_d$Team == temp_data[j,]$Away & !is.na(t_d$Games_Played)),]$Games_Played, 1)) + 1,1), 
                              temp_data[j,]$AwayPoints, temp_data[j,]$AwayGoals - temp_data[j,]$HomeGoals, NA, NA)
    }
    
    else {
      t_d[nrow(t_d) + 1,] = c(temp_data[j,]$Home, temp_data[j,]$Wk,
                              as.numeric(tail(t_d[which(t_d$Team == temp_data[j,]$Home & !is.na(t_d$Games_Played)),]$Games_Played, 1)),
                              0, 0, NA, NA)
      
      t_d[nrow(t_d) + 1,] = c(temp_data[j,]$Away, temp_data[j,]$Wk, 
                              as.numeric(tail(t_d[which(t_d$Team == temp_data[j,]$Away & !is.na(t_d$Games_Played)),]$Games_Played, 1)), 
                              0, 0, NA, NA)
    }
  }
}
# setting data type of multiple columns to numeric
t_d$Games_Played <- as.numeric(t_d$Games_Played)
t_d$Points <- as.numeric(t_d$Points)
t_d$Total_Points <- as.numeric(t_d$Total_Points)
t_d$Matchday <- as.numeric(t_d$Matchday)
t_d$GD <- as.numeric(t_d$GD)

teams <- unique(t_d$Team)

# filling in total_points and total_gd based on cumulative sum of rows
for (i in 1:20) {
  t_d[t_d$Team == teams[i],]$Total_Points = cumsum(t_d[which(t_d$Team == teams[i] & t_d$Games_Played <= 38),]$Points)
  t_d[t_d$Team == teams[i],]$Total_GD = cumsum(t_d[which(t_d$Team == teams[i] & t_d$Games_Played <= 38),]$GD)
}

# group rows matchday + rank teams by total points
t_d <- t_d %>%
  group_by(Matchday) %>%
  mutate(rank = rank(-Total_Points, ties.method = "random")) %>%
  ungroup()

#function for transparent colors
t_col <- function(color, percent, name = NULL) {
  rgb(color[1], color[2], color[3], 
      max = 255,
      alpha = (100 - percent) * 255/100,
      names = name)
}

# bump plot
t_d %>%
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
    
    axis.title.y = element_blank()
  )



