# [loading] - Libraries -----------------------------------------------------------------
library(readr)
library(worldfootballR)
library(tidyverse)
library(ggbump)
library(readr)
library(gghighlight)
library(ggplot2)
library(ggiraph)
library(MetBrewer)

# [Loading] - Data ----
big5_match_data <- get_match_results(country = c("ENG", "ESP", "ITA", "GER", "FRA"), gender = "M", season_end_year = "2022", tier = "1st") %>%
  mutate(Competition_Name = ifelse(Competition_Name == "FuÃŸball-Bundesliga", "Bundesliga", Competition_Name),
         Home = ifelse(Home == "M'Gladbach", "Gladbach", Home),
         Away = ifelse(Away == "M'Gladbach", "Gladbach", Away))


gk_data <- fb_big5_advanced_season_stats(season_end_year = 2022,
                                         stat_type = "keepers",
                                         team_or_player = "player")

gk_data_adv <- fb_big5_advanced_season_stats(season_end_year = 2022,
                                             stat_type = "keepers_adv",
                                             team_or_player = "player")

pl_gks <- gk_data %>%
  filter(Comp == "Premier League", Min_Playing >= 900) %>%
  select(Player)


# ----------------------------------------------------------------
# {Bump Plot] - Teams ----
bund_teams <- c("Arminia", "Augsburg", "Bayern", "Bochum", "Dortmund", "Frankfurt",
                "Freiburg", "Gladbach", "Fürth", "Hertha BSC", "Hoffenheim", "Köln", 
                "Leverkusen", "Mainz 05", "RB Leipzig", "Stuttgart", "Union Berlin", "Wolfsburg")

laLiga_teams <- c("Alavés", "Athletic Club", "Atlético", "Barcelona", "Betis",
                  "Cádiz", "Celta Vigo", "Elche", "Espanyol", "Getafe",
                  "Granada", "Levante", "Mallorca", "Osasuna", "Rayo",
                  "Real Madrid", "Sociedad", "Sevilla", "Valencia", "Villarreal")

ligue1_teams <- c("Angers", "Bordeaux", "Brest", "Clermont", "Lens", "Lille",
                  "Lorient", "Lyon", "Marseille", "Metz", "Monaco", "Montpellier",
                  "Nantes", "Nice", "Paris S-G", "Reims", "Rennes", "Étienne",
                  "Strasbourg", "Troyes")

pl_teams <- c("Arsenal", "Aston Villa", "Brentford", "Brighton", "Burnley", "Chelsea",
              "Everton", "Leeds", "Leicester", "Liverpool", "Man City", "Man Utd", "Newcastle",
              "Norwich", "Palace", "Sthampton", "Tottenham", "Watford", "West Ham", "Wolves")

serieA_teams <- c("Atalanta", "Bologna", "Cagliari", "Empoli", "Fiorentina", 
                  "Genoa", "Inter", "Juventus", "Lazio", "Milan", 
                  "Napoli", "Roma", "Salernitana", "Sampdoria", "Sassuolo", 
                  "Spezia", "Torino", "Udinese", "Venezia", "Verona")

get_team_choices <- function(comp) {
  if (comp == "Bundesliga") {
    return(bund_teams)
  } else if (comp == "La Liga") {
    return(laLiga_teams)
  } else if (comp == "Ligue 1") {
    return(ligue1_teams)
  } else if (comp == "Premier League") {
    return(pl_teams)
  } else if (comp == "Serie A") {
    return(serieA_teams)
  }
}


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

get_leaguePalette <- function(comp) {
  if (comp == "Bundesliga") {
    return(bund_hex)
  } else if (comp == "La Liga") {
    return(laLiga_hex)
  } else if (comp == "Ligue 1") {
    return(ligue1_hex)
  } else if (comp == "Premier League") {
    return(pl_hex)
  } else if (comp == "Serie A") {
    return(serieA_hex)
  }
}
# [Bump Plot] - Data Transformation Functions ----
get_leagueSpecfic <- function(df, comp) {
  if (comp == "Premier League") {
    return(
      df %>% filter(Competition_Name == "Premier League")
    )
  } else if (comp == "La Liga") {
    return(
      df %>% filter(Competition_Name == "La Liga")
    )
  } else if (comp == "Serie A") {
    return(
      df %>% filter(Competition_Name == "Serie A")
    )
  } else if (comp == "Bundesliga") {
    return(
      df %>% filter(Competition_Name == "Bundesliga")
    )
  } else if (comp == "Ligue 1") {
    return(
      df %>% filter(Competition_Name == "Ligue 1")
    )
  }
}

transform_matchResults <- function(fbref_mr) {
  fbref_mr %>%
    mutate(HomePoints = ifelse(HomeGoals > AwayGoals, 3, ifelse(HomeGoals < AwayGoals, 0, 1)),
           AwayPoints = ifelse(HomeGoals > AwayGoals, 0, ifelse(HomeGoals < AwayGoals, 3, 1))) %>%
    select(Date, Wk, Home, HomePoints, HomeGoals, Away, AwayPoints, AwayGoals)
}

# finding latest md in which a match was played
find_lastWeek <- function(df) {
  
  end_wk <- tail(df$Wk, 1)
  match_count <- length(unique(df$Home)) / 2
  
  for (i in 1:end_wk) {
    if (nrow(df[which(df$Wk == i & is.na(df$HomePoints)),]) == match_count) {
      return(i - 1)
    }
  }
  return(end_wk)
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
get_bumpData <- function(match_data) {
  match_data <- transform_matchResults(match_data)
  last_week <- find_lastWeek(match_data)
  bump_df <- fill_df(match_data, last_week)
  
  # setting data type of multiple columns to numeric
  bump_df$Games_Played <- as.numeric(bump_df$Games_Played)
  bump_df$Points <- as.numeric(bump_df$Points)
  bump_df$Total_Points <- as.numeric(bump_df$Total_Points)
  bump_df$Matchday <- as.numeric(bump_df$Matchday)
  bump_df$GD <- as.numeric(bump_df$GD)
  
  teams <- unique(bump_df$Team)
  bump_df <- fill_pointsAndGd(bump_df, teams)
  bump_df <- add_rank(bump_df, length(unique(bump_df$Team)))
}

# [Bump Plot] - Plot Output ----
get_bumpPlot <- function(df, teams, h_team, start_md, end_md, rank_option, back_color, palette) {
  start_md <- as.numeric(start_md)
  end_md <- as.numeric(end_md) 
  
  df <- df %>%
    filter(Matchday <= end_md & Matchday >= start_md)
  
  if (any(df$Team == h_team)) {
    df %>%
      ggplot(aes(Matchday, Rank, group = Team, colour = Team)) +
      geom_bump(smooth = 5, size = 3, lineend = "round") + 
      geom_point(size = 5) +
      scale_colour_manual(
        breaks = teams,
        values = palette
      ) +
      geom_text(data = df %>% 
                  filter(Matchday == start_md),
                aes(label = Team, start_md - 1), fontface = "bold", hjust = 0.5, size = 11) +
      geom_text(data = df %>% 
                  filter(Matchday == end_md),
                aes(label = Team, x = end_md + 1), fontface = "bold", hjust = 0.5, size = 11) +
      gghighlight(any(Team == h_team),
                  use_direct_label = rank_option,
                  label_key = Rank,
                  label_params = list(colour = "black", size = 10),
                  unhighlighted_params = list(colour = NULL, alpha = 0.1)) +
      scale_y_reverse() +
      # labs(title = paste("Premier League Table Progression (Matchday ", start_md, " - ", end_md, ")",
      #                    sep = "")) +
      theme_minimal() +
      theme(
        legend.position = "none",
        
        panel.grid = element_blank(),
        panel.background = element_rect(back_color),
        
        plot.title = element_text(size = 45, face = "bold", hjust = 0.5),
        
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
  }
  
  
  else {
    df %>%
      ggplot(aes(Matchday, Rank, group = Team, colour = Team, data_id = Team)) +
      geom_path_interactive(size = 3, lineend = "round") + 
      geom_point_interactive(size = 5) + 
      scale_colour_manual(
        breaks = teams,
        values = palette
      ) +
      geom_text_interactive(data = df %>% 
                              filter(Matchday == start_md),
                            aes(label = Team, start_md - 1.2), hjust = 0.5, fontface = "bold", size = 11) +
      geom_text_interactive(data = df %>% 
                              filter(Matchday == end_md),
                            aes(label = Team, x = end_md + 1.2), hjust = 0.5, fontface = "bold", size = 11) +
      scale_y_reverse(breaks = unique(df$Rank)) +
      expand_limits(x = 1, y = 1) +
      # labs(title = paste("Premier League Table Progression (Matchday ", start_md, " - ", end_md, ")",
      #                    sep = "")) +
      theme_minimal() +
      theme(
        legend.position = "none",
        
        panel.grid = element_blank(),
        panel.background = element_rect(back_color),
        
        plot.title = element_text(size = 45, face = "bold", hjust = 0.5),
        
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
      )
  }
}



# script for testing updated bump functions 
bund_match_data <- get_leagueSpecfic(big5_match_data, "Bundesliga")
bund_match_data_transformed <- transform_matchResults(bund_match_data)
bund_last_week <- find_lastWeek(bund_match_data_transformed)

bund_bump_data_filled <- fill_df(bund_match_data_transformed, bund_last_week)

bund_bump_data_filled$Games_Played <- as.numeric(bund_bump_data_filled$Games_Played)
bund_bump_data_filled$Points <- as.numeric(bund_bump_data_filled$Points)
bund_bump_data_filled$Total_Points <- as.numeric(bund_bump_data_filled$Total_Points)
bund_bump_data_filled$Matchday <- as.numeric(bund_bump_data_filled$Matchday)
bund_bump_data_filled$GD <- as.numeric(bund_bump_data_filled$GD)

bund_bump_data_pointsAndGD <- fill_pointsAndGd(bund_bump_data_filled, unique(bund_bump_data_filled$Team))

bund_bump_data_rank <- add_rank(bund_bump_data_pointsAndGD, 20)

unique(bund_bump_data_rank$Team)

get_bumpPlot(df = bund_bump_data_rank, teams = unique(bund_bump_data_rank$Team), 
             h_team = "", start_md = 1, end_md = 22, rank_option = FALSE, 
             back_color = "#D3D3D3", palette = serieA_hex
)

# ----------------------------------------------------------------
# [GK Zone] - Plot Outputs ----
get_plKeeper_adv <- function(gk_data, gk_data_adv) {
  pl_keepers <- gk_data %>%
    filter(Comp == "Premier League", Min_Playing > 900)
  
  pl_keepers_adv <- merge(gk_data_adv, pl_keepers)
  colnames(pl_keepers_adv)[colnames(pl_keepers_adv) == "#OPA_Sweeper"] <- "OPA_Sweeper"
  colnames(pl_keepers_adv)[colnames(pl_keepers_adv) == "#OPA_per_90_Sweeper"] <- "OPA_Sweeper_per_90"
  colnames(pl_keepers_adv)[colnames(pl_keepers_adv) == "_per_90_Expected"] <- "per_90"
  colnames(pl_keepers_adv)[colnames(pl_keepers_adv) == "PSxG+_per__minus__Expected"] <- "PSxG_minus_GA"
  
  return(pl_keepers_adv)
}

gk_model_plot <- function(data) {
  ggplot(data = data, aes(x = SoTA, y = PSxG_minus_GA, colour = Save_percent)) +
    xlim(40, 100) +
    geom_vline(xintercept = 70, colour = "white", linetype = "dashed") +
    geom_hline(yintercept = 0, colour = "white", linetype = "dashed") +
    geom_point_interactive(aes(size = 3, tooltip = paste(Player, " - ", Squad, "\n",
                                                         "PSxG - GA: ", PSxG_minus_GA, "\n",
                                                         "SoTA: ", SoTA, "\n",
                                                         "GA: ", GA,
                                                         sep = ),
                               data_id = Player)) + 
    scale_colour_gradientn(colours = met.brewer("OKeeffe2", n = 5)) +
    scale_y_continuous(limits = c(-8, 8),
                       breaks = c(-8, -6, -4, -2, 0, 2, 4, 6, 8)) +
    labs(title = "Who's Beating the Model? (And Does it Matter?)",
         subtitle = "Shot Stopping Ability by Shots on Target for Premier League GK's (> 900 minutes played)") +
    xlab("Shots on Target Against") +
    ylab("Post-Shot Expected Goals - Goals Allowed per 90") +
    guides(size = FALSE,
           colour = guide_colourbar(label.position = "bottom",
                                    title.position = "top",
                                    title = "Save %",
           )
    ) +
    theme(
      text = element_text(colour = "white"),
      title = element_text(size = 14, margin = margin(2.5, 0, 0, 0)),
      
      plot.background = element_rect(fill = "black"),
      
      axis.title = element_text(colour = "white", size = 12),
      axis.title.y = element_text(margin = margin(0, 8, 0, 6)),
      axis.title.x = element_text(margin = margin(3, 0, 6, 0)),
      axis.text = element_text(colour = "white"),
      axis.ticks = element_line(colour = "white", linetype = "longdash"),
      axis.line = element_line(colour = "white"),
      
      panel.background = element_rect(colour = "black", fill = "black"),
      panel.grid = element_blank(),
      
      legend.position = c(0.175, 0.8),
      legend.direction = "horizontal",
      legend.background = element_rect(colour = "white", fill = "black", linetype = "dashed"),
      legend.text = element_text(colour = "white"),
      legend.title = element_text(colour = "white"),
      legend.title.align = 0.5,
      legend.key.size = unit(0.75, "cm")
    )}

gk_sweeper_plot <- function(data) {
  ggplot(data = data, aes(x = AvgDist_Sweeper, y = OPA_Sweeper_per_90)) +
    geom_text(aes(x = 18, y = 0.25, label = "Outside of Box", hjust = 1.25), colour = "white") +
    geom_text(aes(x = 12, y = 1.41, label = "Penalty Spot", hjust = -.25), colour = "white") +
    geom_vline(xintercept = 12, colour = "white", linetype = "dashed") +
    geom_vline(xintercept = 18, colour = "white", linetype = "dashed") +
    
    geom_point_interactive(size = 4, colour = met.brewer("Lakota", n = 1), 
                           aes(tooltip = paste(Player, " - ", Squad, "\n",
                                               "Avg Distance: ", AvgDist_Sweeper, "\n",
                                               "Def Actions per 90: ", OPA_Sweeper_per_90, "\n",
                                               sep = ""),
                               data_id = Player)) +
    scale_x_continuous(expand = c(0,0), limits = c(11.5, 18.5)) +
    scale_y_continuous(expand = c(0,0), limits = c(0.1, 1.55)) +
    labs(title = "Sweeper or Nah?",
         subtitle = "Distance of Defensive Actions from Goal by Activity Outside of the Penalty Area") +
    
    ylab("Defensive Actions Outside of Penalty Area per 90") +
    xlab("Average Distance from Goal of All Defensive Actions (Yards)") +
    
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "black"),
      text = element_text(colour = "white"),
      
      plot.background = element_rect(fill = "black"),
      
      axis.line = element_line(colour = "white"),
      axis.text = element_text(colour = "white"),
      axis.title = element_text(colour = "white", size = 12),
      axis.ticks = element_line(colour = "white"),
      axis.title.y = element_text(margin = margin(0, 7.5, 0, 5)),
      axis.title.x = element_text(margin = margin(4, 0, 5, 0)),
      
      title = element_text(size = 14, margin = margin(5, 0, 0, 0))
    )
}

gk_cross_plot <- function(data) {
  fill_gradient <- colorRampPalette(c("#6ca3a0", "#002a3a"))
  
  ggplot(data = data, aes(x = reorder(Player, +Stp_percent_Crosses), y = Stp_percent_Crosses, fill = CK_Goals,)) +
    geom_bar_interactive(stat = "identity", width = .8, 
                         aes(tooltip = paste0(Player, " - ", Squad, "\n",
                                              "# of Attempted Crosses: ", Opp_Crosses, "\n",
                                              "# of Stopped Crosses: ", Stp_Crosses, "\n",
                                              "% of Crosses Stopped: ", Stp_percent_Crosses, "\n",
                                              "Goals from Corners: ", CK_Goals, 
                                              sep = ""),
                             data_id = Player)) +
    coord_flip() +
    scale_y_continuous(expand = c(0,0), limits = c(0, 15.5), breaks = c(5, 10, 15)) +
    scale_fill_gradientn(colours = fill_gradient(10)) +
    
    ylab("Percentage of Penalty Area Crosses Successfully Stopped") +
    guides(fill = guide_colorbar(label.position = "bottom",
                                 title.position = "top",
                                 title = "Goals from Corners")) +
    
    labs(title = "Controlling the Box - Cross Stopping Ability of Premier League GK's (> 900 Minutes Played)") +
    
    theme(
      title = element_text(colour = "white", hjust = 1),
      
      axis.title.y = element_blank(),
      axis.title.x = element_text(colour = "white"),
      axis.text = element_text(colour = "white"),
      axis.line = element_line(colour = "white"),
      axis.ticks = element_line(colour = "white"),
      
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "black"),
      
      plot.background = element_rect(fill = "black"),
      
      legend.direction = "horizontal",
      legend.background = element_rect(fill = "black", colour = "white", linetype = "dashed"),
      legend.title.align = 0.5,
      legend.text = element_text(colour = "white"),
      legend.title = element_text(colour = "white"),
      legend.key.width = unit(20, "pt"),
      legend.position = c(0.8, 0.25)
    )
}

# [GK Zone] - Viz Text(s) ----
gk_model_text <- paste0("This plot, inspired by this wonderful (article) by John Muller, attempts to ")
