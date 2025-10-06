# Load libraries
pacman::p_load(tidyverse, ggimage, ggtext, showtext)

## Load font
font_add_google("Nunito")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# Load data
euroleague_basketball <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-07/euroleague_basketball.csv')

# Data wrangling
team_map <- c(
  "Virtus Olidata Bologna" = "Virtus Bologna",
  "Panathinaikos" = "Panathinaikos AKTOR Athens",
  "Barcelona" = "FC Barcelona",
  "Olympiacos" = "Olympiacos Piraeus",
  "Fenerbahce" = "Fenerbahce Beko Istanbul",
  "Anadolu Efes" = "Anadolu Efes Istanbul"
)

champions_df <- euroleague_basketball |>
  separate_rows(Years_of_Titles_Won, sep = ",\\s*") |>
  filter(
    !is.na(Years_of_Titles_Won),
    !Years_of_Titles_Won %in% c("None", "NA", "")) |>
  mutate(
    year = as.integer(Years_of_Titles_Won),
    year_won = case_when(
      year == 2025 ~ "2024–25",
      TRUE ~ sprintf("%d–%02d", year - 1, year %% 100)),  # e.g. 2000 means 1999–2000 season
    team = recode(Team, !!!team_map)) |>
  filter(year - 1 >= 2000) |>
  select(team, year_won)

# Sequence of EuroLeague seasons ~ 2000–01 to 2024–25
expected_seasons <- sprintf("%d–%02d", 2000:2024, (2001:2025) %% 100)
missing_seasons <- setdiff(expected_seasons, champions_df$year_won)

lookup_df <- tribble(
  ~team,                       ~year_won,
  "Virtus Bologna",             "2000–01",
  "FC Barcelona",               "2002–03",
  "Maccabi Rapyd Tel Aviv",     "2003–04",
  "Maccabi Rapyd Tel Aviv",     "2004–05",
  "CSKA Moscow",                "2005–06",
  "CSKA Moscow",                "2007–08",
  "FC Barcelona",               "2009–10",
  "Maccabi Rapyd Tel Aviv",     "2013–14",
  "CSKA Moscow",                "2015–16",
  "CSKA Moscow",                "2018–19",
  NA,                           "2019–20"   # Covid-19 disruption
)

# Add missing season winners
roll_df <- champions_df |>
  bind_rows(
    lookup_df |> 
      filter(year_won %in% missing_seasons)
  ) |>
  distinct(year_won, .keep_all = TRUE) |>
  arrange(year_won) |> 
  mutate(
    team_logo = ifelse(!is.na(team), paste0("logos/", team, ".png"), NA), 
    index = row_number(),
    col = ((index - 1) %% 5) + 1,
    row = -((index - 1) %/% 5)
  )

# Plot
ggplot(roll_df, aes(col, row)) +
  geom_tile(width = 0.8, height = 0.8, fill = "#1c1c1c", color = "#2d2d2d", linewidth = 0.5) +
  geom_image(
    data = filter(roll_df, !is.na(team_logo)),
    aes(image = team_logo), size = 0.14, by = "width") +
  geom_tile(width = 0.85, height = 0.85,
    data = filter(roll_df, is.na(team_logo)), fill = NA, color = "#555555", linewidth = 0.6) +
  geom_text(
    data = filter(roll_df, is.na(team_logo)), label = "Cancelled\n*COVID-19*", color = "gray80", size = 2.8, lineheight = 1, family = "Nunito") +
  geom_text(aes(label = year_won, y = row - 0.5), color = "gold", size = 3, fontface = "bold", family = "Nunito") +
  coord_equal() +
  theme_void() +
  labs(
    title = "EUROLEAGUE BASKETBALL \n CHAMPIONS", subtitle = "ROLL OF HONOR 2000/01-2024/25 SEASON",
    caption = "Graphic: Manasseh Oduor | Source: EuroleagueBasketball R package \n #TidyTuesday {wk:40-2025}") +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    plot.margin = margin(t=20, b=10, l=10, r=10),
    plot.title = element_text(color = "gold", face = "bold", family = "Nunito", size = 20, hjust = 0.5, margin = margin(b = 15)),
    plot.subtitle = element_text(color = "gray80", hjust = 0.5, family = "Nunito", size = 12),
    plot.caption = element_text(size = 8, family = "Nunito", hjust = 0.5, color = "#FAF9EE", margin = margin(t=5))
  )

ggsave("euroleague_basketball.png", width = 7, height = 7)