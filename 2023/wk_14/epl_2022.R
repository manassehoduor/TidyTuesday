
rm(list = ls())

# libraries
pacman::p_load(tidyverse, ggtext, magick, htmltools, ggplot2, scales, ggimage, grid, png, patchwork,
               knitr, gt, gtExtras, extrafont, showtext, ggpath, webshot2) 

# Import fonts
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# load data
epl_2022 <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv")

# data Wrangling
epl_data <- epl_2022 |>
  select(HomeTeam, AwayTeam, FTHG, FTAG, FTR, HST, AST, HS, AS)

# create a table of unique team names
teams <- unique(c(epl_data$HomeTeam, epl_data$AwayTeam))

# create an empty data frame to store the results
standings <- data.frame(team_name = character(),
                        games_played = integer(),
                        goal_difference = integer(),
                        points = integer(),
                        total_wins = integer(),
                        home_wins = integer(),
                        away_wins = integer(),
                        shots_on_target = integer(),
                        total_goals = integer(),
                        total_shots = integer(), 
                        stringsAsFactors = FALSE)

# loop through each team and calculate their stats
for (i in 1:length(teams)) {
  team <- teams[i]
  home_epl_data <- subset(epl_data, HomeTeam == team)
  away_epl_data <- subset(epl_data, AwayTeam == team)
  games_played <- nrow(home_epl_data) + nrow(away_epl_data)
  goals_for <- sum(home_epl_data$FTHG) + sum(away_epl_data$FTAG)
  goals_against <- sum(home_epl_data$FTAG) + sum(away_epl_data$FTHG)
  goal_difference <- goals_for - goals_against
  wins <- sum(home_epl_data$FTR == "H") + sum(away_epl_data$FTR == "A")
  home_wins <- sum(home_epl_data$FTR == "H")
  away_wins <- sum(away_epl_data$FTR == "A")
  shots_on_target <- sum(home_epl_data$HST) + sum(away_epl_data$AST)
  total_goals <- sum(home_epl_data$FTHG) + sum(away_epl_data$FTAG)
  total_shots <- sum(home_epl_data$HS) + sum(away_epl_data$AS)
  points <- 3 * wins + sum(home_epl_data$FTR == "D") + sum(away_epl_data$FTR == "D")
  standings[i,] <- c(team, games_played, goal_difference, points, wins, home_wins, away_wins, shots_on_target, total_goals, total_shots)
}

# sort the standings by points and goal difference
standings <- standings[order(standings$points, standings$goal_difference, decreasing = TRUE),]

# add row numbers
standings$position <- 1:nrow(standings)

standings$total_goals = as.integer(standings$total_goals)
standings$shots_on_target = as.integer(standings$shots_on_target)
standings$total_shots = as.integer(standings$total_shots)

standings <- standings |>
  mutate(shots_on_target_Per_Match = round(shots_on_target/38, 1))


# Shot conversion for goals
standings <- standings |>
  select(position, team_name, games_played, total_wins, home_wins, away_wins, shots_on_target,
         shots_on_target_Per_Match, total_shots, total_goals, goal_difference, points) |>
  mutate(Conversion_rate_of_total_shots = round(total_goals/total_shots*100,1),
         Conversion_rate_of_shots_on_target = round(total_goals/shots_on_target*100, 1),
         shooting_times_to_score_a_goal = round(100/Conversion_rate_of_total_shots,0),
         On_target_shooting_times_to_score_a_goal = round(100/Conversion_rate_of_shots_on_target,0))

standings <- standings |>
  select(position, team_name, games_played, total_wins, home_wins, away_wins, shots_on_target,
         shots_on_target_Per_Match, total_shots, total_goals, 
         Conversion_rate_of_total_shots, Conversion_rate_of_shots_on_target,
         shooting_times_to_score_a_goal, On_target_shooting_times_to_score_a_goal,
         goal_difference, points) |>
  arrange(position)

# EPL logos scrapped from ESPN website
logo <- read.csv(file = "logo.csv")

# Join data
standings <- full_join(standings, logo, by = "team_name")
standings <- standings  |> 
  mutate(image_epl = paste0("<img src ='", url_logo_espn, "' width='20'>"))  

# Read team logos from local disk
logo_images <- lapply(standings$local_logo_path, png::readPNG)

# Add team logos as raster grobs
team_names <- fct_inorder(standings$team_name)
standings$logo_grob <- vector("list", length(team_names))
for (i in seq_along(team_names)) {
  logo_grob <- rasterGrob(logo_images[[i]], interpolate = TRUE)
  standings$logo_grob[[i]] <- annotation_custom(logo_grob, xmin = i - 0.5, xmax = i + 0.5, ymin = -Inf, ymax = -1)
}

# Plot Cleveland dot plot with team logos
title = "\nENGLISH PREMIER LEAGUE ANALYSIS"
subtitle = "Who were the most effective Premier League teams at converting chances in the 2022 season? \n Manchester City netted a total of 99 goals in the 2022 season, \n yielding a conversion rate of 39.0% for shots on target and 13.8% for all shots taken \n They had an impressive scoring efficiency in the 2022 season, requiring only 3 shots on target \n or 7 total shots per game to score a goal"

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'>.</span>",
               "<span style='font-family:sans;'> Manasseh Oduor </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | #TidyTuesday: week 14 | Source: @EvanJGower")

ggplot(standings) +
  geom_segment(aes(x=team_names, xend=team_names, 
                   y=On_target_shooting_times_to_score_a_goal, 
                   yend=shooting_times_to_score_a_goal), color="#f0c808") +
  geom_point(aes(x=team_names, y=On_target_shooting_times_to_score_a_goal), color= "white", 
             size = 3.5, shape = 21, fill = "white", stroke = 1, show.legend = FALSE) +
  geom_point(aes(x=team_names, y=shooting_times_to_score_a_goal), color= "#07a0c3",
             size = 3.5, shape = 21, fill = "white", stroke = 1, show.legend = FALSE) +
  geom_text(aes(x = team_names, y = 17, label = team_names),
            family = "Roboto Condensed",
            hjust = 0, size = 4.5, color = "white") +
  
  scale_color_manual(values=c("white","#0e066f")) +
  scale_y_continuous(breaks = c(2,4,6,8,10,12,14,16,18),
                     limits = c(-1, 18)) +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "Roboto Condensed", color= "white"),
        axis.text.x=element_markdown(family = "Roboto Condensed", color= "white", size = 15),
        axis.text.y=element_markdown(family = "Roboto Condensed", color= "white", size = 13),
        panel.grid = element_blank(),
        axis.title.x = element_text(color = "white", family = "Roboto Condensed", size = 15),
        axis.title.y = element_blank(),
        legend.position = "none",
        axis.line.x = element_line(colour = "white", linewidth = 0.3),
        plot.subtitle = element_text(family = "Roboto Condensed", colour = "#ffffff", size = 14,
                                     margin = margin(t = 3, b = 2, unit = "mm"), hjust = 0.5
        ),
        plot.title = element_text(
          face = "bold", colour = "#ffffff", family = "Roboto Condensed", size = 25, hjust = 0.5
        ),
        plot.caption = element_textbox_simple(
          lineheight = 0.4, colour = "#ffffff", size = 9, 
          margin = margin(0,0,0,100,"mm"), family = "Roboto Condensed"
        )
  ) +
  labs(
  x = "", 
  y = "Shooting times to score a goal",
  subtitle = subtitle,
  title = title,
  caption = cap) +
  # Add team logos as annotations
  standings$logo_grob

ggsave("epl.png", height=8, width=12, bg= "black")


# Table 1
# Add logo
standings <- standings |>
  mutate(Logo = team_name)

# Teams Logo
Brighton_logo <- local_image('logo_epl/Brighton.png', height = 20)
Burnley_logo <- local_image('logo_epl/Burnley.png', height = 20)
Chelsea_logo <- local_image('logo_epl/Chelsea.png', height = 20)
Everton_logo <- local_image('logo_epl/Everton.png', height = 20)
Leicester_logo <- local_image('logo_epl/Leicester.png', height = 20)
ManCity_logo <- local_image('logo_epl/Man City.png', height = 20)
ManUnited_logo <- local_image('logo_epl/Man United.png', height = 20)
Newcastle_logo <- local_image('logo_epl/Newcastle.png', height = 20)
Norwich_logo <- local_image('logo_epl/Norwich.png', height = 20)
Southampton_logo <- local_image('logo_epl/Southampton.png', height = 20)
Arsenal_logo <- local_image('logo_epl/Arsenal.png', height = 20)
Liverpool_logo <- local_image('logo_epl/Liverpool.png', height = 20)
Tottenham_logo <- local_image('logo_epl/Tottenham.png', height = 20)
Watford_logo <- local_image('logo_epl/Watford.png', height = 20)
Wolves_logo <- local_image('logo_epl/Wolves.png', height = 20)
WestHam_logo <- local_image('logo_epl/West Ham.png', height = 20)
CrystalPalace_logo <- local_image('logo_epl/Crystal Palace.png', height = 20)
Brentford_logo <- local_image('logo_epl/Brentford.png', height = 20)
AstonVilla_logo <- local_image('logo_epl/Aston Villa.png', height = 20)
Leeds_logo <- local_image('logo_epl/Leeds.png', height = 20)

# select and reorder the columns
standings <- standings |>
  select(position, Logo, team_name, games_played, total_wins, home_wins, away_wins, shots_on_target,
         shots_on_target_Per_Match, total_shots, total_goals, 
         Conversion_rate_of_total_shots, Conversion_rate_of_shots_on_target,
         goal_difference, points) |>
  arrange(position)

  
# Table
standings_tbl = standings %>%
  gt() %>%
  gt_theme_guardian() %>% 
  gt::data_color(
    columns = points, colors = c("white", "orange")) %>% 
  gt::data_color(
    columns = c(total_goals, Conversion_rate_of_total_shots, Conversion_rate_of_shots_on_target), 
    colors = c("white", "#4b7912")) %>% 
  cols_width(
    team_name ~ px(120),
    points ~ px(40),
    Logo ~ px(50)) %>% 
  tab_header(title = md("**ENGLISH PREMIER LEAGUE**"), subtitle = md("2022 Season: *Team Stats*")) %>%
  cols_label(position = "Pos", team_name = "Team", Logo = "", games_played = "P", 
             total_wins = "W", home_wins = "Home Win", away_wins = "Away Win", 
             shots_on_target = "Shots on Target", shots_on_target_Per_Match = "Shots on Target per Match",
             total_shots = "Total Shots", total_goals = "Goals", 
             Conversion_rate_of_total_shots = "Conversion Rate of Total Shots",
             Conversion_rate_of_shots_on_target = "Conversion Rate of Shots on Target",
             goal_difference = "GD", points = "Pts") %>%
  tab_options(
    table.font.size = "13px",
    #table.width = "950px",
    column_labels.font.weight = "bolder",
    #column_labels.font.size = 14,
    column_labels.background.color = "#0e066f",
    column_labels.padding = "8px",
    table.border.top.width = "1px",
    table.border.top.color = "black",
    table.border.bottom.width = "1px",
    table.border.bottom.color = "black",
    table.border.left.width = "1px",
    table.border.left.color = "black",
    table.border.right.width = "1px",
    table.border.right.color = "black",
    data_row.padding = "3px",
    heading.title.font.size = "22px",
    heading.subtitle.font.size = "20px",
    heading.title.font.weight = "bold",
    heading.padding = "7px",
    source_notes.font.size = 10,
    source_notes.background.color = "#0e066f",
  ) %>% 
  text_transform(
    locations = cells_body(columns = Logo),
    fn = function(x) {
      paste0(
        dplyr::case_when(
          x ==  'Brighton' ~ Brighton_logo, 
          x == "Burnley"  ~ Burnley_logo,
          x == 'Chelsea' ~ Chelsea_logo,
          x == 'Everton' ~ Everton_logo,
          x == 'Leicester' ~ Leicester_logo,
          x == 'Man City' ~ ManCity_logo,
          x == 'Man United' ~ ManUnited_logo,
          x == 'Newcastle' ~ Newcastle_logo,
          x == 'Norwich' ~ Norwich_logo,
          x == 'Southampton' ~ Southampton_logo,
          x == 'Arsenal' ~ Arsenal_logo,
          x == 'Liverpool' ~ Liverpool_logo,
          x == 'Tottenham' ~ Tottenham_logo,
          x == 'Watford' ~ Watford_logo,
          x == 'Wolves' ~ Wolves_logo,
          x == 'West Ham' ~ WestHam_logo,
          x == 'Crystal Palace' ~ CrystalPalace_logo,
          x == 'Brentford' ~ Brentford_logo,
          x == 'Aston Villa' ~ AstonVilla_logo,
          x == 'Leeds' ~ Leeds_logo))
    }
  ) %>% 
  cols_align(
    align = "center",
    columns = everything()) %>% 
  tab_source_note(
    source_note = md("**Table:** Manasseh Oduor | **Source:** @EvanJGower | **#TidyTuesday:** week 14"))


new_gt_save <- function (data, filename, path = NULL, ..., zoom = 2, expand = 5) 
{
  filename <- gt:::gtsave_filename(path = path, filename = filename)
  tempfile_ <- tempfile(fileext = ".html")
  tempfile_ <- tempfile_ %>% gt:::tidy_gsub("\\\\", "/")
  gt:::gt_save_html(data = data, filename = tempfile_, path = NULL)
  if (!requireNamespace("webshot2", quietly = TRUE)) {
    stop("The `webshot2` package is required for saving images of gt tables.", 
         call. = FALSE)
  }
  else {
    webshot2::webshot(url = paste0("file:///", tempfile_), 
                      file = filename, selector = "table", zoom = zoom, 
                      expand = expand, ...)
  }
}

# Save table
new_gt_save(standings_tbl, "epl_2022_standings.png")



