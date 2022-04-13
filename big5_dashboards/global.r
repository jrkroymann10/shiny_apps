# [Loading] - Libraries, Pitches -----------------------------------------------------------------
library(shinyWidgets)
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
library(ggrepel)
library(reactable)
library(reactablefmtr)

font_add_google("Roboto Mono", "Roboto")
showtext_auto()

colGrid <- rgb(235, 235, 235, 225, maxColorValue = 255)

t_col <- function(color, percent, name = NULL) {
  rgb(color[1], color[2], color[3],
      max = 255,
      alpha = (100 - percent) * 255/100,
      names = name)
}

statsBomb_halfPitch <- function(grass_colour, line_colour, background_colour, goal_colour) {

  shotmapxgcolors <- c("#192780", "#2a5d9f", "#40a7d0", "#87cdcf", "#e7f8e6", "#f4ef95", "#FDE960",
                       "#FCDC5F", "#F5B94D", "#F0983E", "#ED8A37", "#E66424", "#D54F1B", "#DC2608",
                       "#BF0000", "#7F0000", "#5F0000")

  theme_blankPitch <- function(size=12) {
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.length = unit(0, "lines"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.background = element_rect(fill = background_colour, colour = NA),
          legend.key = element_rect(colour = background_colour, fill = background_colour),
          legend.key.size = unit(1.2, "lines"),
          legend.text = element_text(size = size),
          legend.title = element_text(size = size, face = "bold", hjust = 0),
          strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
          panel.background = element_rect(fill = background_colour, colour = background_colour),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = element_blank(),
          plot.background = element_blank(),
          plot.margin = unit(c(0,0,0,0), "lines"),
          plot.title = element_text(size = size*1.2),
          strip.text.y = element_text(colour = background_colour, size = size, angle = 270),
          strip.text.x = element_text(size = size*1))}

  ## defining variables related to dimensions of a statsbomb pitch
  ymin <- 0
  ymax <- 80
  xmin <- 60
  xmax <- 120

  ## Defining features along the length
  boxEdgeOff <- 102
  sixYardOff <- 114
  penSpotOff <- 108
  halfwayline <- 60

  ## Defining features along the width
  boxEdgeLeft <- 18
  boxEdgeRight <- 62
  sixYardLeft <- 30
  sixYardRight <- 50
  goalPostLeft <- 36
  goalPostRight <- 44
  CentreSpot <- 40

  ## other dimensions
  centreCirle_d <- 20

  ## creating circle function for arc at the top of the box
  circle_fun <- function(center = c(0,0), diameter = 1, npoints = 100) {
    r <- diameter/2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }

  ## create leftD arc
  dArc <- circle_fun(c((40),(penSpotOff)),centreCirle_d,npoints = 1000)

  ## remove part in box
  dArc <- dArc[which(dArc$y <= (boxEdgeOff)),]

  p <- ## mix matched limits due to rotating og dimensions to half plot
    ggplot() + xlim(c(ymin, ymax)) + ylim(c(xmin, xmax)) +

    ## adding the theme
    theme_blankPitch() +

    ## adding base rectangle of the pitch
    geom_rect(aes(xmin = ymin, xmax = ymax, ymin = xmin, ymax = xmax), fill = grass_colour, colour = line_colour) +

    ## adding 18 yard box offensive
    geom_rect(aes(xmin = boxEdgeLeft, xmax = boxEdgeRight, ymin = boxEdgeOff, ymax = xmax), fill = grass_colour,
              colour = line_colour) +

    ## adding goal offensive
    geom_segment(aes(x = goalPostLeft, y = xmax, xend = goalPostRight, yend = xmax), colour = goal_colour, size = 1) +

    ## adding six yard box offensive
    geom_rect(aes(xmin = sixYardLeft, xmax = sixYardRight, ymin = sixYardOff, ymax = xmax), fill = grass_colour,
              colour = line_colour) +

    ## adding the arc circle
    geom_path(data = dArc, aes(x = x, y = y), colour = line_colour) +

    ## adding penalty spot
    geom_point(aes(x = CentreSpot, y = penSpotOff), colour = line_colour) +

    ## adding goal
    geom_segment(aes(x = goalPostLeft, y = xmax, xend = goalPostRight, yend = xmax),colour = goal_colour, size = 1)

  return(p)
}

# [Loading] - Reading in Data ----
big5 <- read.csv('data/big5.csv', encoding = "UTF-8")
big5_table <- read.csv('data/big5_table.csv', encoding = "UTF-8")
gkDataCombined <- read.csv('data/big5_GK.csv', encoding = "UTF-8")
big5_shots <- read.csv('data/big5_shots.csv', encoding = "UTF-8") %>%
  filter(result != "OwnGoal")
# ---------------------------------------------------------------------------------------------------
# [Table] - Image Helper (unicode workaround) ----
value_helper <- function(value) {
  if (value == "Alavés") {
    return("Alaves")
  }
  else if (value == "Atlético Madrid") {
    return("Athletico Madrid")
  }
  else if (value == "Cádiz") {
    return("Cadiz")
  }
  else if (value == "Greuther Fürth") {
    return("Greuther Furth")
  }
  else if (value == "Köln") {
    return("Koln")
  }
  else if (value == "Saint-Étienne") {
    return("Saint-Etienne")
  }
  else {
    return(value)
  }
}
# [Table] - Table Theme ----
standTheme <- reactableTheme(
  backgroundColor = "#202124",
  color = "white",
  style = list(fontSize = 16),
  cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")
)
# [Table] - Table Output ----
redgreen_pal <- function(x) rgb(colorRamp(c("#ea4335", "#3aa757"))(x), maxColorValue = 255)

form_unicode <- function(val) {
  if (val == "W") {
    return("\uD83D\uDFE2")
  } else if (val == "L") {
    return("\uD83D\uDD34") 
  } else {
    return("\u25EF")
  }
}

standTable <- function(data) {
  reactable(
    data %>%
      mutate(squad_copy = Squad) %>%
      select(Rk, Squad, squad_copy, MP, W, D, L, GF, GA, GD, Pts, Last.5),
    
    defaultSorted = "Rk",
    
    defaultColDef = colDef(
      width = 50, 
      align = "center",
      
      # vAlign = "center" - hopefully will be in next version of package
    ),
    
    columns = list(
      Squad = colDef(name = "", width = 52.5, align = "left", 
                     cell = function(value) {
                       val_mod <- value_helper(value)
                       imgSrc <- sprintf("%s.png", val_mod)
                       image <- img(src = imgSrc, height = "32px", width = "32px", alt = value)
                       tagList(
                         div(style = list(display = "inline-block", width = "45px"), image)
                       )
                     }),
      squad_copy = colDef(name = "Club", width = 197.5, align = "left",
                          style = list(borderRight = "1px solid rgba(255, 255, 255, 1)"),),
      Rk = colDef(name = "RK", width = 50, align = "center"),
      Last.5 = colDef(name = "Form", width = 175, align = "center", sortable = FALSE,
                      style = list(borderLeft = "1px solid rgba(255, 255, 255, 1)"),
                      cell = function(value) {
                        paste(form_unicode(substr(value, 1, 1)), form_unicode(substr(value, 3, 3)), form_unicode(substr(value, 5, 5)), form_unicode(substr(value, 7, 7)), form_unicode(substr(value, 9, 9)))
                      }),
      Pts = colDef(width = 60),

      GD = colDef(style = function(value) {
        normalized = ((value + (abs(min(data$GD)))) - (min(data$GD) + (abs(min(data$GD))))) / (((max(data$GD)) + abs(min(data$GD))) - ((min(data$GD)) + abs(min(data$GD))))
        color <- redgreen_pal(normalized)
        list(background = color)
      })
    ),
    
    outlined = TRUE,
    highlight = TRUE,
    pagination = FALSE,
    bordered = FALSE,
    theme = standTheme
    )
}

min_max_norm <- function(value, x) {
  (value - min(x)) / (max(x) - min(x))
}
# [Bump Plot] - Team Hex Codes ----
bund_hex <- c("#005CA9", "#DE023F", "#005CA9", "#E1000F", "#46714d", "#009932",
              "#808777", "#DC052D", "#FDDC02", "#E32221", "#004E95", "#FFFFFF",
              "#65B32E", "#918F90", "#ED1C24", "#FDE100", "#E32219", "#1C63B7") 

laLiga_hex <- c("#004fa3", "#8AC3EE", "#0067B1", "#0761AF", "#e53027",
                "#E20613", "#0BB363", "#fde607", "#B4053F", "#AD8F1F",
                "#007FC8", "#FFE667", "#A61B2B", "#05642c", "#EE2523",
                "#D18816", "#CB3524", "#A50044", "#ffffff", "#F43333")

ligue1_hex <- c("#006eb2", "#d87043", "#FFFFFF", "#009fe3", "#e51b22",
                "#1B8F3A", "#14387F", "#ed1c24", "#E13327", "#fff200",
                "#b59a54", "#ee2223", "#008d3f", "#f58113", "#6e0f12",
                "#e01e13", "#004170", "#2faee0", "#c50c46", "#c50c46")

pl_hex <- c("#FDB913", "#630F33", "#670E36", "#6CABDD", "#9C824A",
            "#D71920", "#F9423A", "#A7A5A6", "#00A650", "#AC944D",
            "#0053A0", "#0057B8", "#fbee23", "#FFFFFF", "#e30613",
            "#274488", "#7A263A", "#034694", "#D01317", "#B80102")

serieA_hex <- c("#005395", "#8A1E03", "#742038", "#1B5497", "#00579C",
                "#fd9b00", "#482E92", "#AD1919", "#8B7D37", "#FFFFFF",
                "#AD002A", "#99834a", "#00A752", "#1E71B8", "#A21C26",
                "#FB090B", "#87D8F7", "#12A0D7", "#8E1F2F", "#A39261")
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
    mutate(Rank = row_number(Matchday),
           Norm_Rank = (Total_Points - min(Total_Points)) / (max(Total_Points) - min(Total_Points))) %>%
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
getBumpPlot <- function(df, md_start, md_end, teams, sel_teams, league_palette, bump_rank, background, plotType) {
  if (plotType == "Bump Plot") {
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
      gghighlight(ifelse(length(sel_teams >= 1), any(Team == sel_teams), Rank <= 20),
                  use_direct_label = bump_rank,
                  label_key = Rank,
                  label_params = list(colour = "#202124", size = 10),
                  unhighlighted_params = list(alpha = 0.25)) +
      scale_y_reverse() +
      scale_x_continuous(limits = c(md_start - 2.25, md_end + 2.25)) +
      theme(
        legend.position = "none",
        
        panel.grid = element_blank(),
        panel.background = element_rect(fill = substring(background, 1, 7), colour = substring(background, 1, 7),
                                        size = 5),        
        plot.title = element_text(size = 45, face = "bold", hjust = 0.5, family = "Roboto"),
        plot.background = element_rect(fill = substring(background, 1, 7), colour = "white", size = 2),
        
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
  } else if (plotType == "Bump Plot w/ Point Difference") {
  
    df %>%
      ggplot(aes(x = Matchday, y = Norm_Rank, colour = Team)) +
      geom_bump(smooth = 5, size = 3, lineend = "round") +
      geom_point(size = 5) +
      scale_colour_manual(
        breaks = teams,
        values = league_palette
      ) +
      geom_text(data = df %>%
                  dplyr::filter(Matchday == md_end), aes(label = Team, x = md_end + .4),
                fontface = "bold", size = 11, hjust = 0, family = "Roboto") +
      gghighlight(ifelse(length(sel_teams >= 1), any(Team == sel_teams), Rank <= 20),
                  use_direct_label = bump_rank,
                  label_key = Rank,
                  label_params = list(colour = "#202124", size = 10),
                  unhighlighted_params = list(alpha = 0.25)) +
      scale_x_continuous(limits = c(md_start, md_end + 2.25)) +
      theme(
        legend.position = "none",
        
        panel.grid = element_blank(),
        panel.background = element_rect(fill = substring(background, 1, 7), colour = substring(background, 1, 7),
                                        size = 5),        
        plot.title = element_text(size = 45, face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = substring(background, 1, 7), colour = "white", size = 2),
        
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
    
    
  } else {
    
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
      scale_x_continuous(limits = c(md_start - 2.25, md_end + 2.25), expand = c(0.01, 1)) +
      theme(
        legend.position = "none",
        
        panel.grid = element_blank(),
        panel.background = element_rect(fill = substring(background, 1, 7), colour = substring(background, 1, 7)),
        
        plot.background = element_rect(fill = substring(background, 1, 7), colour = "white", size = 2),
        
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
getGkZonePlot <- function(vizSelected, gkData, playerSel) {
  if (vizSelected == "Who's Beating the Model?") {
    gkZoneModelPlot(gkData, playerSel)
  }
  else if (vizSelected == "Getting Out of the Box") {
    gkZoneSweeperPlot(gkData, playerSel)
  }
  else if (vizSelected == "Distribution Drop-off") {
    gkZonePassingPlot(gkData, playerSel)
  }
}

gkZoneModelPlot <- function(data, playerSel) {
  if (is.null(playerSel)) {
    label = NULL
  } else {
    label = geom_label_repel(data = . %>% 
                               mutate(label = if_else(Player %in% playerSel, Player, NULL)),
                             aes(label = label, fill = Comp),
                             color = "white", fontface = "bold", direction = "both", show.legend = FALSE,
                             box.padding = 2, max.overlaps = 10, min.segment.length = 0)
  }
  
  ggplot(data = data, aes(x = SoTA, y = PSxG_minus_GA, colour = Comp)) +
    geom_vline(xintercept = median(data$SoTA), colour = "white", linetype = "dashed") +
    geom_hline(yintercept = 0, colour = "white", linetype = "dashed") +
    geom_point_interactive(aes(size = PSxG_per_SoT_Expected, tooltip = paste(Player, " - ", Squad, "\n",
                                                         "PSxG-GA: ", PSxG_minus_GA, "\n",
                                                         "PSxG: ", PSxG_Expected, "\n",
                                                         "GA: ", GA, "\n",
                                                         "SoTA: ", SoTA, "\n",
                                                         "PSxG/SoTA: ", PSxG_per_SoT_Expected,
                                                         sep = ""),
                               data_id = Player)) +
    label +
    scale_x_continuous(limits = c(min(data$SoTA) - 10, max(data$SoTA) + 10)) +
    scale_y_continuous(limits = c(min(data$PSxG_minus_GA) - 1, max(data$PSxG_minus_GA) + 1)) +
    scale_size_continuous(range = c(2, 5)) +
    scale_colour_manual(breaks = c("Bundesliga", "La Liga", "Ligue 1", "Premier League", "Serie A"),
                        values = c(met.brewer("Isfahan2")[1], met.brewer("Isfahan2")[2], met.brewer("Isfahan2")[3], met.brewer("Isfahan2")[4], met.brewer("Isfahan2")[5])) +
    scale_fill_manual(breaks = c("Bundesliga", "La Liga", "Ligue 1", "Premier League", "Serie A"),
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
      title = element_text(size = 14, margin = ggplot2::margin(2.5, 0, 0, 0, unit = "pt"), face = "bold"),

      plot.background = element_rect(fill = "#202124", colour = "white"),
      plot.title = element_text(margin = ggplot2::margin(6.25, 0, 3.75, 0, unit = "pt")),
      plot.subtitle = element_text(face = "plain", margin = ggplot2::margin(0, 0, 10, 0, unit = "pt")),
      plot.tag = element_text(face = "plain", size = 10),
      plot.tag.position = c(0.325, 0.00875),

      axis.title = element_text(colour = "white", size = 12, hjust = 0.5),
      axis.title.y = element_text(margin = ggplot2::margin(0, 15, 0, 15, unit = "pt")),
      axis.title.x = element_text(margin = ggplot2::margin(15, 0, 25, 0, unit = "pt")),
      axis.text = element_text(colour = "white"),
      axis.ticks = element_line(colour = "white", linetype = "longdash"),
      axis.line = element_line(colour = "white"),

      panel.background = element_rect(colour = "#202124", fill = "#202124"),
      panel.grid = element_blank(),

      legend.background = element_rect(colour = "white", fill = "#202124"),
      legend.text = element_text(colour = "white", margin = ggplot2::margin(0, 1, 0, 0, unit = "pt")),
      legend.key = element_rect(fill = "#202124"),
      legend.title.align = 0.5)
}
gkZoneSweeperPlot <- function(data, playerSel) {
  if (is.null(playerSel)) {
    label = NULL
  } else {
    label = geom_label_repel(data = . %>% 
                               mutate(label = if_else(Player %in% playerSel, Player, NULL)),
                             aes(label = label, fill = Comp),
                             color = "white", fontface = "bold", direction = "both", show.legend = FALSE,
                             box.padding = 2, max.overlaps = 10, min.segment.length = 0)
  }
  
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
    label +
    scale_y_continuous(expand = c(0,0), limits = c(0, 2), breaks = c(0, 0.5, 1, 1.5, 2)) +
    scale_x_continuous(breaks = c(12, 14, 16, 18, 20)) +
  
    scale_colour_manual(breaks = c("Bundesliga", "La Liga", "Ligue 1", "Premier League", "Serie A"),
                        values = c(met.brewer("Isfahan2")[1], met.brewer("Isfahan2")[2], met.brewer("Isfahan2")[3], met.brewer("Isfahan2")[4], met.brewer("Isfahan2")[5])) +
    scale_fill_manual(breaks = c("Bundesliga", "La Liga", "Ligue 1", "Premier League", "Serie A"),
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
      panel.background = element_rect(fill = "#202124"),

      plot.background = element_rect(fill = "#202124", colour = "white"),
      plot.title = element_text(margin = ggplot2::margin(6.25,0,3.75,0), face = "bold", size = 18),
      plot.subtitle = element_text(margin = ggplot2::margin(0,0,10,0), face = "plain", size = 14),
      plot.tag = element_text(face = "plain", size = 10),
      plot.tag.position = c(0.325, 0.00875),
      
      legend.background = element_rect(colour = "white", fill = "#202124"),
      legend.key = element_rect(fill = "#202124"),
      legend.title.align = 0.5,
      legend.position = c(.875, .3),

      axis.text = element_text(colour = "white"),
      axis.line = element_line(colour = "white"),
      axis.title = element_text(colour = "white", size = 12, face = "bold"),
      axis.ticks = element_line(colour = "white"),
      axis.title.y = element_text(margin = ggplot2::margin(0, 16.25, 0, 15)),
      axis.title.x = element_text(margin = ggplot2::margin(15, 0, 25, 0)),
    )
}
gkZonePassingPlot <- function(data, playerSel) {
  if (is.null(playerSel)) {
    label = NULL
  } else {
    label = geom_label_repel(data = . %>% 
                               mutate(label = if_else(Player %in% playerSel, Player, NULL)),
                             aes(label = label, fill = Comp),
                             color = "white", fontface = "bold", direction = "both", show.legend = FALSE,
                             box.padding = 2, max.overlaps = 10, min.segment.length = 0)
  } 
  
  ggplot(data = data, aes(AvgLen_Passes, Cmp_percent_Medium - Cmp_percent_Long, colour = Comp)) +
  
  geom_vline(aes(xintercept = mean(AvgLen_Passes)), colour = "white", linetype = "dashed") +
  geom_hline(aes(yintercept = mean(Cmp_percent_Medium - Cmp_percent_Long)), colour = "white", linetype = "dashed") +
  geom_point_interactive(aes(data_id = Player, tooltip = paste0(Player, " - ", Squad, "\n",
                                                                "Medium: ", Cmp_percent_Medium, "%", "\n",
                                                                "Long: ", Cmp_percent_Long, "%", "\n",
                                                                "Avg Length: ", round(mean(AvgLen_Passes), 2), " yards")),
                         size = 3) +
  label +
  scale_x_continuous(limits = c(min(data$AvgLen_Passes) - 2.5, max(data$AvgLen_Passes) + 2.5)) +
  scale_y_continuous(limits = c(min(data$Cmp_percent_Medium - data$Cmp_percent_Long) - 5, max(data$Cmp_percent_Medium - data$Cmp_percent_Long) + 5)) +
  
  scale_colour_manual(breaks = c("Bundesliga", "La Liga", "Ligue 1", "Premier League", "Serie A"),
                      values = c(met.brewer("Isfahan2")[1], met.brewer("Isfahan2")[2], met.brewer("Isfahan2")[3],
                                 met.brewer("Isfahan2")[4], met.brewer("Isfahan2")[5])) +
  scale_fill_manual(breaks = c("Bundesliga", "La Liga", "Ligue 1", "Premier League", "Serie A"),
                                values = c(met.brewer("Isfahan2")[1], met.brewer("Isfahan2")[2], met.brewer("Isfahan2")[3],
                                           met.brewer("Isfahan2")[4], met.brewer("Isfahan2")[5])) +
  
  guides(colour = guide_legend(
    title = "Competition",
    title.theme = element_text(size = 10, colour = 
                                 "white", hjust = 0.5,
                               face = "bold"),
    override.aes = list(size = 5)
    )
  ) +
  
  labs(title = "Distribution Drop-off At A Distance",
       subtitle = "Pass Completion Percentage Drop-off by Average Length of Passes",
       x = "Average Length of All Passes (yards)", y = "Medium Completion % - Long Completion %",
       tag = "Data: StatsBomb via fbref.com (GK's with > 900 minutes played in 21/22 season) - Medium = 15-30 yards, Long = 30+ yards") +
  
  theme(
    text = element_text(family = "Roboto", colour = "white"),
    
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#202124"),
    
    plot.background = element_rect(fill = "#202124", colour = "white"),
    plot.title = element_text(margin = ggplot2::margin(6.25,0,3.75,0), face = "bold", size = 18),
    plot.subtitle = element_text(margin = ggplot2::margin(0,0,10,0), face = "plain", size = 14),
    plot.tag = element_text(face = "plain", size = 10),
    plot.tag.position = c(0.5, 0.00875),
    
    legend.background = element_rect(colour = "white", fill = "#202124"),
    legend.key = element_rect(fill = "#202124"),
    legend.title.align = 0.5,
    
    axis.line = element_line(colour = "white"),
    axis.text = element_text(colour = "white"),
    axis.title = element_text(colour = "white", size = 12, face = "bold"),
    axis.ticks = element_line(colour = "white"),
    axis.title.y = element_text(margin = ggplot2::margin(0, 16.25, 0, 15), face = "bold"),
    axis.title.x = element_text(margin = ggplot2::margin(15, 0, 25, 0), face = "bold"),
  )
}

# [GK Zone] - Viz Text(s) ----
getGkZoneText <- function(vizSelected) {
  if (vizSelected == "Who's Beating the Model?") {
    gkModelText
  }
  else if (vizSelected == "Getting Out of the Box") {
    gkSweeperText
  }
  else if (vizSelected == "Distribution Drop-off") {
    gkPassingText
  }
  else {
    gkLandingText 
  }
}

gkModelText <- HTML("<p>This plot, inspired by <a href = 'https://fivethirtyeight.com/features/the-most-valuable-soccer-player-in-america-is-a-goalkeeper/'> John Muller</a>,
                    attempts to showcase how goalkeepers have performed against Statsbomb's <a href = 'https://statsbomb.com/2018/11/a-new-way-to-measure-keepers-s
                    hot-stopping-post-shot-expected-goals/'> Post-Shot Expected Goals Model</a> (PSxG). The PSxG Model is similar to Statsbomb's Expected Goals Model, but 
                    is designed to evaluate goalkeepers rather than shooters. This change in focus is accomplished by restricting the shot sample to those on target, and using 
                    post-shot information, such as shot speed and trajectory, to train the model. This plot's main statistic, PSxG - Goals Allowed, allows us to evaluate goalkeeper's 
                    shot-stopping ability. Positive values suggest an above average shot-stopping ability (or better luck). Another useful statistic for analysis, PSxG
                    per Shot on Target (PSxG/SoTa), is represented in the size of each point, and can be used to compare the quality of shots keepers have faced.</p>")

gkSweeperText <- HTML("<p>This visualization is the place for determining whether or not a goalkeeper is a sweeper keeper or not. Not only does the plot show us how often big 5 keepers
                       are defensively active beyond the penalty area per 90, but it also gives us an idea of how far away from goal their defensive actions usually take place. This plot allows
                       us to appreciate the unique behavior of players such as Manuel Neuer, but also informs us on an important facet of goalkeeping play. How does one respond to breakaway one-on-one
                       situations? How does one suppress attacks right before they're about to become dangerous? These are the sorts of questions that can start to be answered here. The plot tells us nothing
                       about how goalkeepers perform in defensive situations further away from goal, but it does give us an idea of how often they get themselves into these situations, or if they 
                       even care to in the first place.</p>")

gkPassingText <- HTML("<p>There aren't a lot of statistics that can be used to distinctly show differences in passing ability and performance at a group level. Most goalkeepers 
                       complete 90-100% of their short and medium passes, have similar numbers for progessive pass distance on a per 90 basis, and random stats, such as throws attempted,
                       tell us very little. The passing stats start to become useful, in terms of learning more about a keeper's passing ability or the type
                       of style their team plays, with the long stuff. Differences in long pass (> 30 yards) completion rates and launch rates (% of passes over 40 yards)
                       between keepers are much larger than any passing stats dealing with shorter distances. Unfortunately, we can't be sure if the differences in drop-off levels displayed in this plot accurately captures differences in passing
                       ability between keepers, as long pass completiton rates depend on so many outside factors (team play style, ability of teammates to control such passes, etc.) Despite this,
                       we can still use this plot as a launching point for further analysis. For example, one might want to further investigate a keeper with an above average average
                       pass length and a below average drop-off, and see if that unique combo is due to the keeper's passing ability, and how that ability might be utilized more
                       effectively in a team with a different style of play.</p>")

gkLandingText <- HTML("<p>Welcome to the Goalkeeper Zone! This panel is comprised of four different vizualizations, all of which inform us on how Big 5 goalkeepers have performed in
                       the main facets of their game (Shot-Stopping, Sweeper Activity, Distribution, and Corner/Cross Handling). These vizualizations are not intended to declare who the best
                       goalkeepers are, but rather inform us on the differences in performance and style between keepers individually and across competitions. With that in mind, this panel can 
                       be of use in a number of situations. Come here to confirm what your eyes see week in and out. Come here to determine which competition has the most adventureous sweeper
                       keepers. Do what you want with the plots, but please remember that although a certain plot may help with the story you're trying to tell, it's most likely only a small part of the
                       analyis necessary to deliver that story in an accurate and convincing manner. Cheers!</p>")

# ----------------------------------------------------------------
# [XG Time (Team)] - Data Transformation ----
# transforming big 5 data (calculating rolling xG averages for and against + adding gameNum column)
big5ToXG <- function(df, team, rollN = 6) {
  df_trans <- df %>%
    filter(Home == team | Away == team) %>%
    select(Wk, Date, Home, HomeGoals, Home_xG, Away, AwayGoals, Away_xG) %>%
    drop_na() %>%
    mutate(HomeAway = if_else(Home == team, "Home", "Away"),
           xgFor = if_else(Home == team, Home_xG, Away_xG),
           xgAgainst = if_else(Home == team, Away_xG, Home_xG),
           goalsFor = if_else(Home == team, HomeGoals, AwayGoals),
           goalsAgainst = if_else(Home == team, AwayGoals, HomeGoals),
           outcome = if_else(goalsFor > goalsAgainst, "win",
                             if_else(goalsFor < goalsAgainst, "loss",
                                     "tie")),
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
    map_df(~data.frame(selRoll = approx(.x$gameNum, .x$rollFor, n = 100),
                       oppRoll = approx(.x$gameNum, .x$rollAgainst, n = 100),
                       selXG = approx(.x$gameNum, .x$xgFor, n = 100),
                       oppXG = approx(.x$gameNum, .x$xgAgainst, n = 100),
                       team = .x$selOpp[1]))
}

# [XG Time (Team)] - Plot Output(s) ----
getXGPlot <- function(viz, df_int, df, team, comp, bund, the, pal) {
  if (viz == "6 Game Rolling Avg") {
    xgRollPlot(df_int, team, comp, bund, the, pal)
  }
  else if (viz == "Game By Game") {
    gbgXGPlot(df, team, comp, bund, the, pal)
  }
}

xgRollPlot <- function(df, team, comp, bund, the, pal) {
  if (the == TRUE) {
    phrase = "in the"
  }
  else {
    phrase = "in"
  }
  
  ggplot(data = df, aes(x = selRoll.x, y = max(selRoll.y, oppRoll.y))) +
      geom_ribbon(aes(ymin = selRoll.y, ymax = pmin(selRoll.y, oppRoll.y)), fill = pal[1], alpha = 0.5) + 
      geom_ribbon(aes(ymin = oppRoll.y, ymax = pmin(selRoll.y, oppRoll.y)), fill = pal[2], alpha = 0.5) +
      geom_line(aes(y = selRoll.y), colour = pal[1], size = 1.5) +
      geom_line(aes(y = oppRoll.y), colour = pal[2], size = 1.5) +
      scale_x_continuous(
        expand = c(0,0), 
        limits = c(1, if_else(bund == TRUE, 36, 38)),
        breaks = seq(5, 35, by = 5)) +
      scale_y_continuous(
        expand = c(0,0),
        limits = c(0.05, 3.45), 
        breaks = seq(0, 3, by = 0.5)) +
      
      labs(title = paste(team, "'s Underlying Expected Performance", sep = ""),
           subtitle = glue("<span>6 game rolling average of {team}'s
                            <span style = 'color:{pal[1]}'>**expected goals for**</span> and 
                            <span style = 'color:{pal[2]}'>**expected goals against**</span>
                            {phrase} {comp} for 2021/2022</span>"),
           caption = "Data from Statsbomb via fbref.com. This data includes penalties. The first six games shown are a partial average. Recreation of a @petermckeever viz using R by @biglake402.") +
      xlab("Matchday") + ylab("Rolling xG") +
    theme(
        panel.background = element_rect(fill = "#202124", colour = "#202124"),
        panel.grid = element_blank(),
        
        text = element_text(family = "Roboto", colour = "white"),
        
        title = element_text(colour = "white"),
        plot.title = element_text(face = "bold", size = 20, margin = ggplot2::margin(6.25, 0, 3.75, 0, unit = "pt")),
        plot.subtitle = element_markdown(family = "Roboto", face = "plain", size = 16, lineheight = 0.625, ),
        plot.caption = element_text(hjust = 0, size = 12.5, margin = ggplot2::margin(0,0,0,0, unit = "pt")),
        plot.background = element_rect(colour = "white", fill = "#202124"),

        axis.ticks = element_line(colour = "white",),
        axis.ticks.length = unit(c(-3, 3), "pt"),
        axis.text = element_text(colour = "white", size = 12.5),
        axis.text.y = element_text(margin = ggplot2::margin(0, 7.5, 0, 0, unit = "pt")),
        axis.title = element_text(face = "bold", size = 18),
        axis.title.y = element_text(margin = ggplot2::margin(0, 20, 0, 20, unit = "pt")),
        axis.title.x = element_text(margin = ggplot2::margin(20, 0, 20, 0, unit = "pt")),
        axis.line = element_line(colour = "white"),
        
        legend.position = "none"
      )
}
gbgXGPlot <- function(df, team, comp, bund, the, pal) {
  if (the == TRUE) {
    phrase = "in the"
  }
  else {
    phrase = "in"
  }
  
  ggplot(df, aes(gameNum, xgFor - xgAgainst)) +
    geom_hline(aes(yintercept = 0), colour = "white") +
    geom_segment_interactive(aes(x = gameNum, xend = gameNum, y = 0, yend = xgFor - xgAgainst, data_id = gameNum,
                                 tooltip = paste0(team, if_else(HomeAway == "Home", " vs. ", " at "), if_else(HomeAway == "Home", Away, Home),
                                                  "\n", "Actual (xG): ", ifelse(HomeAway == "Home", HomeGoals, AwayGoals), "(",
                                                  ifelse(HomeAway == "Home", Home_xG, Away_xG), ") - ", ifelse(HomeAway == "Home", AwayGoals, HomeGoals),
                                                  "(", ifelse(HomeAway == "Home", Away_xG, Home_xG), ")")), 
                 linetype = "dashed", size = 0.625, colour = "white") +
    geom_point_interactive(aes(shape = HomeAway, fill = outcome, colour = outcome, data_id = gameNum,
                               tooltip = paste0(team, if_else(HomeAway == "Home", " vs. ", " at "), if_else(HomeAway == "Home", Away, Home),
                                                "\n", "Actual (xG): ", ifelse(HomeAway == "Home", HomeGoals, AwayGoals), "(",
                                                ifelse(HomeAway == "Home", Home_xG, Away_xG), ") - ", ifelse(HomeAway == "Home", AwayGoals, HomeGoals),
                                                "(", ifelse(HomeAway == "Home", Away_xG, Home_xG), ")")
                               ), 
                           size = 5, stroke = 1) +
    labs(title = paste0(team, "'s Match By Match Expected and Actual Performance"),
         subtitle = glue("<span>Expected Goal Difference + Outcome (<span style = 'color:{pal[1]}'>**win**</span>,
                         <span style = 'color:{pal[2]}'>**loss**</span>,
                         <span style = 'color:{pal[3]}'>**draw**</span>)
                         of {team}'s matches {phrase} {comp} for 2021/2022</span>"),
         caption = "Data from Statsbomb via fbref.com (xG includes penalites)",
         x = "Matchday", y = "xG For - xG Against") +
    scale_shape_manual(
      values = c(24, 21)
    ) +
    scale_x_continuous(
      limits = c(1, if_else(bund == FALSE, 38, 36)), 
      expand = c(0,.9),
      breaks = seq(5, 35, by = 5)
    ) +
    scale_y_continuous(
      limits = c(-5.75, 5.75),
      expand = c(0,0),
      breaks = seq(-5, 5, by = 1)
    ) +
    scale_fill_manual(
      values = c(pal[2],pal[3],pal[1])
    ) +
    scale_colour_manual(
      values = c(pal[2],pal[3],pal[1])
    ) +
    guides(
      fill = "none",
      color = "none",
      shape = guide_legend(
        override.aes = list(fill = "white")
      )
    ) +
    theme(
      text = element_text(family = "Roboto", colour = "white"),
      
      panel.background = element_rect(colour = "#202124", fill = "#202124"),
      panel.grid = element_blank(),
      panel.ontop = FALSE,
      
      plot.background = element_rect(fill = "#202124"),
      plot.title = element_text(face = "bold", size = 20, margin = ggplot2::margin(6.25, 0, 3.75, 0, unit = "pt")),
      plot.subtitle = element_markdown(size = 16),
      plot.caption = element_text(hjust = 0, size = 12.5, margin = ggplot2::margin(0,0,0,0, unit = "pt")),
      
      legend.position = c(0.9525, 1.05), 
      legend.direction = "horizontal", 
      legend.title = element_blank(),
      legend.key = element_rect(fill = "#202124", colour = "#202124"),
      legend.background = element_rect(colour = "white", fill = "#202124", linetype = "dashed"),
      legend.text = element_text(size = 12.5),
      
      axis.line = element_line(color = "white"),
      axis.ticks = element_line(colour = "white"),
      axis.ticks.length = unit(c(-3,3), "pt"),
      axis.title = element_text(face = "bold", size = 18),
      axis.title.y = element_text(margin = ggplot2::margin(0, 20, 0, 20, unit = "pt")),
      axis.title.x = element_text(margin = ggplot2::margin(20, 0, 20, 0, unit = "pt")),
      axis.text = element_text(colour = "white", size = 12.5)
    )
}
# [XG Time (Team)] - Palettes ----
getXGPalette <- function(input) {
  if (input == "Liverpool") {
    c("#D01317", "#009782", "#FFEE46") # lava, paolo veronese green, lemon yellow
  }
  else if (input == "Venezia") {
    c("#D8703D", "#3C6131", "#94CFDF")
  }
  else if (input == "Real Betis") {
    c("#0BB363", "#E7A614", "#D62550")
  }
}
# [XG Time (Player)] - Shot Map Output ----
get_shotMap <- function(df, palette) {
  statsBomb_halfPitch("#202124", "#ffffff", "#202124", "#131313") +
    geom_point_interactive(data = df, aes(x = Y * 80, y = X * 120, fill = result,
                              size = xG, colour = result, data_id = id,
                              tooltip = paste0("xG: ", round(xG, 3))), shape = 21) +
    scale_fill_manual(values = c(t_col(col2rgb(met.brewer(palette, 5)[1]), 10),
                                 t_col(col2rgb(met.brewer(palette, 5)[2]), 10),
                                 t_col(col2rgb(met.brewer(palette, 5)[3]), 10),
                                 t_col(col2rgb(met.brewer(palette, 5)[4]), 10),
                                 t_col(col2rgb(met.brewer(palette, 5)[5]), 10)),
                      labels = c(" Goal", " Missed", " Saved", " On Post", " Blocked"),
                      breaks = c("Goal", "MissedShots", "SavedShot", "ShotOnPost", "BlockedShot")
    ) +
    scale_colour_manual(values = c(t_col(col2rgb(met.brewer(palette, 5)[1]), 10),
                                   t_col(col2rgb(met.brewer(palette, 5)[2]), 10),
                                   t_col(col2rgb(met.brewer(palette, 5)[3]), 10),
                                   t_col(col2rgb(met.brewer(palette, 5)[4]), 10),
                                   t_col(col2rgb(met.brewer(palette, 5)[5]), 10)),
                        breaks = c("Goal", "MissedShots", "SavedShot", "ShotOnPost", "BlockedShot")
    ) +
    scale_x_reverse() +
    guides(
      colour = "none",
      size = "none",
      fill = guide_legend(
        title = NULL,
        label.theme = element_text(family = "Roboto", colour = "white", hjust = 0),
        override.aes = list(
          size = 5, colour = "white"
        )
      )
    ) +
    
    geom_text(aes(x = 78, y = 73, label = paste0(unique(df$player), " (21-22)")),
              colour = "white", size = 6, family = "Roboto", fontface = "bold", hjust = 0) +
    
    geom_text(aes(x = 78, y = 69, label = paste0("Non-Penalty xG: ", round(sum(df$xG), 2))),
              colour = "white", size = 5, family = "Roboto", hjust = 0) +
    
    geom_text(aes(x = 78, y = 66, label = paste0("Non-Penalty Goals: ", round(sum(df$goal), 2))),
              colour = "white", size = 5, family = "Roboto", hjust = 0) +
    
    geom_text(aes(x = 78, y = 62, label = "Data: Understat (Penalties + Own Goals Not Included)"),
              colour = "white", size = 3, family = "Roboto", hjust = 0) +
    
    theme(
      legend.position = c(0.8375, 0.2075),
      legend.background = element_rect(colour = "#ffffff", linetype = "dashed"),
      legend.title = element_blank(),
      legend.spacing.x = unit(0, "mm"),
      legend.spacing.y = unit(0, "mm")
    )
}
# {XG Time (Player)} - XG Diff Output ----
get_xgDiff <- function(df, palette = "Egypt") {
  
  df <- df %>%
    mutate(shot = 1:nrow(df),
           cum_xg = cumsum(xG),
           cum_goals = cumsum(goal)) %>%
    mutate(diff = cum_goals - cum_xg,
           roll_diff = round(rollmean(diff, k = 10, fill = NA, align = "right"), 2))
  
  for (i in 1:9) {
    df[i,]$roll_diff <- round(mean(df[1:i,]$diff), 2)
  }
  
  ggplot(df, aes(x = shot, y = roll_diff)) +
    geom_hline(aes(yintercept = 0), colour = "white", size = 0.25) +
    geom_line(colour = "white", size = 1.5) +
    geom_line(linetype = "dashed", colour = "white", size = 1.5) +
    
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(1,max(df$shot) + 3),
      breaks = seq(0, max(df$shot), by = 10)) +
    
    scale_y_continuous(
      limits = c(-5.25, 5.25),
      breaks = seq(-5, 5, 1)
    ) +
    
    labs(
      title = paste0(df$player, "'s Underlying Expected Goal-Scoring Performance"),
      subtitle = paste0("10 Shot Rolling Average of ", df$player, "'s Expected Goal Difference in the Premier League for 2021/2022"),
      x = "Shot", y = "Rolling xG Difference"
    ) +
    
    theme(
      panel.background = element_rect(fill = "#202124", colour = "white"),
      panel.grid = element_blank(),
      
      plot.background = element_rect(fill = "#202124", colour = "#202124"),
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 14),
      plot.margin = unit(c(2.5, 5, 2.5, 3.75), "mm"),
      
      text = element_text(colour = "white", family = "Roboto"),
      
      axis.title = element_text(face = "bold", size = 14),
      axis.title.y = element_text(margin = unit(c(0,2.5,0,0), "mm")),
      axis.title.x = element_text(margin = unit(c(2.5, 0, 0, 0), "mm")),
      axis.text = element_text(colour = "white", family = "Roboto", size = 12),
      axis.ticks.x = element_line(colour = "white"),
      axis.ticks.y = element_blank()
    )
}