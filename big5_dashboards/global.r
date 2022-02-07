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
# 
# [Loading] - Data ----
big5 <- data.frame(get_match_results(country = c("ENG", "ESP", "ITA", "GER", "FRA"), gender = "M", season_end_year = "2022", tier = "1st") %>%
                     mutate(Home = ifelse(Home == "M'Gladbach", "Gladbach",
                                          ifelse(Home == "Manchester Utd", "Man Utd",
                                                 ifelse(Home == "Manchester City", "Man City",
                                                        ifelse(Home == "Rayo Vallecano", "Rayo",
                                                                      Home)))),
                            Away = ifelse(Away == "M'Gladbach", "Gladbach",
                                          ifelse(Away == "Manchester Utd", "Man Utd",
                                                 ifelse(Away == "Manchester City", "Man City",
                                                               ifelse(Away == "Rayo Vallecano", "Rayo",
                                                                      Away)))),
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
# {Bump Plot] - Teams ----
bund_teams <- c("Arminia", "Augsburg", "Bayern Munich", "Bochum", "Dortmund", "Eint Frankfurt",
                "Freiburg", "Gladbach", "Greuther Fürth", "Hertha BSC", "Hoffenheim", "Köln",
                "Leverkusen", "Mainz 05", "RB Leipzig", "Stuttgart", "Union Berlin", "Wolfsburg")

laLiga_teams <- c("Alavés", "Athletic Club", "Atlético Madrid", "Barcelona", "Betis",
                  "Cádiz", "Celta Vigo", "Elche", "Espanyol", "Getafe",
                  "Granada", "Levante", "Mallorca", "Osasuna", "Rayo",
                  "Real Madrid", "Real Sociedad", "Sevilla", "Valencia", "Villarreal")

ligue1_teams <- c("Angers", "Bordeaux", "Brest", "Clermont Foot", "Lens", "Lille",
                  "Lorient", "Lyon", "Marseille", "Metz", "Monaco", "Montpellier",
                  "Nantes", "Nice", "Paris S-G", "Reims", "Rennes", "Saint-Étienne",
                  "Strasbourg", "Troyes")

pl_teams <- c("Arsenal", "Aston Villa", "Brentford", "Brighton", "Burnley", 
              "Chelsea", "Crystal Palace", "Everton", "Leeds United", "Leicester City", 
              "Liverpool", "Man City", "Man Utd", "Newcastle Utd", "Norwich City", 
              "Southampton", "Tottenham", "Watford", "West Ham", "Wolves")

serieA_teams <- c("Atalanta", "Bologna", "Cagliari", "Empoli", "Fiorentina",
                  "Genoa", "Hellas Verona", "Inter", "Juventus", "Lazio", 
                  "Milan", "Napoli", "Roma", "Salernitana", "Sampdoria", 
                  "Sassuolo", "Spezia", "Torino", "Udinese", "Venezia", "Verona")
#
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
  cat(file=stderr(), nrow(df), "\n")
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
get_leaguePalette <- function(comp = "Fußball-Bundesliga") {
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

get_team_choices <- function(comp = "Fußball-Bundesliga") {
  if (comp == "La Liga") {
    return(laLiga_teams)
  } else if (comp == "Ligue 1") {
    return(ligue1_teams)
  } else if (comp == "Premier League") {
    return(pl_teams)
  } else if (comp == "Serie A") {
    return(serieA_teams)
  } else {
    return(bund_teams)
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
                  dplyr::filter(Matchday == md_start),
                aes(label = Team, md_start - 1), fontface = "bold", hjust = 0.45, size = 11) +
      geom_text(data = df %>%
                  dplyr::filter(Matchday == md_end),
                aes(label = Team, x = md_end + 1), fontface = "bold", hjust = 0.35, size = 11) +
      gghighlight(any(Team == sel_teams),
                  use_direct_label = bump_rank,
                  label_key = Rank,
                  label_params = list(colour = "black", size = 10),
                  unhighlighted_params = list(alpha = 0.1)) +
      scale_y_reverse() +
      # labs(title = paste("Premier League Table Progression (Matchday ", start_md, " - ", end_md, ")",
      #                    sep = "")) +
      theme_minimal() +
      theme(
        legend.position = "none",
        
        panel.grid = element_blank(),
        panel.background = element_rect(substring(background, 1, 7)),
        
        plot.title = element_text(size = 45, face = "bold", hjust = 0.5),
        
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
  }
  else {
    df %>%
      ggplot(aes(x = Matchday, y = Rank, colour = Team, data_id = Team)) +
      geom_path_interactive(size = 3, lineend = "round") + 
      geom_point_interactive(size = 5) + 
      #xlim(input$md_range[1] - 1, input$md_range[2] + 1) +
      scale_colour_manual(
        breaks = teams,
        values = league_palette
      ) +
      geom_text_interactive(data = df %>%
                              dplyr::filter(Matchday == md_start),
                            aes(label = Team, x = md_start - 1), fontface = "bold", hjust = 0.45, size = 11) +
      geom_text_interactive(data = df %>%
                              dplyr::filter(Matchday == md_end),
                            aes(label = Team, x = md_end + 1), fontface = "bold", hjust = 0.35, size = 11) +
      scale_y_reverse() +
      # labs(title = paste("Premier League Table Progression (Matchday ", start_md, " - ", end_md, ")",
      #                    sep = "")) +
      theme_minimal() +
      theme(
        legend.position = "none",
        
        panel.grid = element_blank(),
        panel.background = element_rect(substring(background, 1, 7)),
        
        plot.title = element_text(size = 45, face = "bold", hjust = 0.5),
        
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
  }
}
# ----------------------------------------------------------------
# [GK Zone] - Plot Outputs ----
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
    # xlim(min(data$SoTa) - 1.5, max(data$SoTa + 1.5)) +
    geom_vline(xintercept = mean(data$SoTA), colour = "white", linetype = "dashed") +
    geom_hline(yintercept = mean(data$PSxG_minus_GA), colour = "white", linetype = "dashed") +
    geom_point_interactive(aes(size = 3, tooltip = paste(Player, " - ", Squad, "\n",
                                                         "PSxG - GA: ", PSxG_minus_GA, "\n",
                                                         "SoTA: ", SoTA, "\n",
                                                         "GA: ", GA,
                                                         sep = ),
                               data_id = Player)) +
    # scale_colour_gradientn(colours = met.brewer("OKeeffe2", n = 5)) +
    # scale_y_continuous(limits = c(min(data$PSxG_minus_GA) - 1.5, max(data$PSxG_minus_GA) + 1.5)) +
    labs(title = "Who's Beating the Model? (And Does it Matter?)",
         subtitle = "Shot Stopping Ability by Shots on Target for Big 5 GK's (> 900 minutes played)") +
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
gkZoneSweeperPlot <- function(data) {
  ggplot(data = data, aes(x = AvgDist_Sweeper, y = OPA_Sweeper_per_90, colour = Comp)) +
    geom_text(aes(x = 18, y = 0.25, label = "Outside of Box", hjust = 1.25), colour = "white") +
    geom_text(aes(x = 12, y = 1.41, label = "Penalty Spot", hjust = -.25), colour = "white") +
    geom_vline(xintercept = 12, colour = "white", linetype = "dashed") +
    geom_vline(xintercept = 18, colour = "white", linetype = "dashed") +

    geom_point_interactive(size = 4, aes(tooltip = paste(Player, " - ", Squad, "\n",
                                                         "Avg Distance: ", AvgDist_Sweeper, "\n",
                                                         "Def Actions per 90: ", OPA_Sweeper_per_90, "\n",
                                                         sep = ""),
                               data_id = Player)) +
    scale_x_continuous(expand = c(0,0), limits = c(11, 18.5)) +
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
gkZoneCrossPlot <- function(data) {
  # fill_gradient <- colorRampPalette(c("#6ca3a0", "#002a3a"))

  ggplot(data = data, aes(x = reorder(Player, +Stp_percent_Crosses), y = Stp_percent_Crosses, fill = Comp)) +
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
    # scale_fill_gradientn(colours = fill_gradient(10)) +

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
# # gk_model_text <- paste0("This plot, inspired by this wonderful (article) by John Muller, attempts to ")

# ----------------------------------------------------------------