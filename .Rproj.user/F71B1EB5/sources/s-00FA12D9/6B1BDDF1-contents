library(worldfootballR)
library(tidyverse)
library(ggbump)

prem_2021 <- get_match_results(country = "ENG", gender = "M", season_end_year = "2022", tier = "1st")

prem_2021 <- prem_2021 %>%
  mutate(HomePoints = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals < AwayGoals, 0, 1)),
         AwayPoints = ifelse(HomeGoals > AwayGoals, 0, ifelse(HomeGoals < AwayGoals, 3, 1))) %>%
  select(Wk, Home, HomePoints, HomeGoals, Away, AwayPoints, AwayGoals)

find_lastWeek <- function(df) {
  for (i in 1:38) {
    if (nrow(df[which(df$Wk == i & is.na(df$HomePoints)),]) == 10) {
      return(i - 1)
    }
  }
  
  return(38)
}

last_week <- find_lastWeek(prem_2021)

t_d <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(t_d) <- c("Team", "Week", "Games_Played", "Points", "GD", "Total_Points", "Total_GD")

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


t_d$Games_Played <- as.numeric(t_d$Games_Played)
t_d$Points <- as.numeric(t_d$Points)
t_d$Total_Points <- as.numeric(t_d$Total_Points)
t_d$Week <- as.numeric(t_d$Week)
t_d$GD <- as.numeric(t_d$GD)

teams <- unique(t_d$Team)

for (i in 1:20) {
  t_d[t_d$Team == teams[i],]$Total_Points = cumsum(t_d[which(t_d$Team == teams[i] & t_d$Games_Played <= 38),]$Points)
  t_d[t_d$Team == teams[i],]$Total_GD = cumsum(t_d[which(t_d$Team == teams[i] & t_d$Games_Played <= 38),]$GD)
}

t_d <- t_d %>%
  group_by(Week) %>%
  mutate(rank = rank(-Total_Points, ties.method = "random")) %>%
  ungroup()

t_d %>%
  ggplot(aes(Week, rank, group = Team, color = Team)) +
  geom_bump(aes(smooth = 10), size = 1.5, lineend = "round")



