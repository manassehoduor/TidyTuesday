# Load Libraries
pacman::p_load(tidyverse, ggforce, forcats, ggtext, showtext)

# Load Fonts
font_add_google(name = "Rosario")
font_add_google(name = "Averia Sans Libre")
font_add_google(name = "Averia Gruesa Libre")
font_add_google(name = "Averia Serif Libre")

showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# Load data
restaurant_and_chef <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-31/restaurant_and_chef.csv')

# Data wrangling
outstanding_chef <- restaurant_and_chef |> 
  filter(subcategory == "Outstanding Chef") |> 
  select(!subcategory) |> 
  distinct()

pers_art <- outstanding_chef |> 
  group_by(name) |> 
  filter(all(c("Nominee", "Winner") %in% rank) | 
           all(c("Semifinalist", "Winner") %in% rank) | 
           all(c("Nominee", "Semifinalist", "Winner") %in% rank)) |> 
  ungroup() |> 
  arrange(name, year)

rank_n <- pers_art |>
  count(name) |>
  arrange(desc(n))

plt_df <- pers_art |>
  left_join(rank_n, by = "name") |>
  mutate(
    year = as.numeric(year),
    radius = (year - min(year)) * 1.2 + 1,  # Adjust radius to add space between rings
    rank_fct = as.factor(rank),
    name = forcats::fct_reorder(name, n, .desc = TRUE)
  ) |>
  arrange(name, year) |>
  group_by(name) |>
  arrange(year, .by_group = TRUE)

rank_cols <- c("Winner" = "#FF8000", "Semifinalist" = "#006BFF", "Nominee" = "#6B240C")

# Plot
ggplot(plt_df) +
  geom_rect(
    aes(
      xmin = -radius, xmax = radius, ymin = -radius, ymax = radius, color = rank_fct), linewidth = 2, fill = NA) +
  geom_rect(
    aes(
      xmin = -radius, xmax = radius, ymin = -radius, ymax = radius), linewidth = 0.5, color = "#FFFDF0", fill = NA) +
  scale_color_manual(values = rank_cols) +
  coord_fixed() +
  facet_wrap_paginate(
    ~ name, labeller = label_wrap_gen(width = 20), ncol = 5, nrow = 4, page = 1
  ) +
  geom_text(
    data = plt_df |> 
      filter(name == "Ashley Christensen" & year == 2019),
    aes(x = radius-35, y = 0, label = "2019 Winner"), size = 7.5, color = "#FF8000", hjust = 0.5, vjust = -1, family = "Averia Serif Libre"
  ) +
  geom_text(
    data = plt_df |> 
      filter(name == "Ashley Christensen" & year == 2016),
    aes(x = radius-32, y = 0, label = "2018 Nominee"), size = 6.5, color = "#A28B55", hjust = 0.5, vjust = 1, family = "Averia Serif Libre"
  ) +
  geom_text(
    data = plt_df |> 
      filter(name == "Ashley Christensen" & year == 2016),
    aes(x = radius-32, y = 0, label = "2016, 2017 Semifinalist"), size = 5, color = "#9DBDFF", hjust = 0.5, vjust = 4, family = "Averia Serif Libre"
  ) +
  theme_minimal() +
  labs(
    title = "<span style='font-size:40pt;'>JAMES BEARD AWARDS</span> <br><br> The Art of Perseverance and Relentless Pursuit of Culinary Artistry",
    subtitle = "Outstanding Chefs and their Achievements over the years",
    color = "Rank", x = NULL, y = NULL,
    caption = "Graphic: Manasseh Oduor \n Source: https://www.jamesbeard.org/awards \n #TidyTuesday {wk:53-2024}"
  ) +
  theme(
    legend.position = "none",
    text = element_text(family = "Averia Gruesa Libre"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(size = 22, color = "#F8FAE5"),
    plot.title = element_markdown(size = 25, face = "bold", color = "#C6E7FF", family = "Averia Serif Libre", hjust = 0.5, margin = margin(t=15, b=10)),
    plot.subtitle = element_text(size = 22, family = "Averia Sans Libre", color = "#D4F6FF", margin = margin(t=5, b=15)),
    plot.margin = margin(l=10, r=10, t=15, b=5),
    plot.caption = element_text(family = "Rosario", color = "#C6E7FF", size = 18, hjust = 1, margin = margin(t=10, b=10))
  )

# Save plot in HD
ggsave("Chef_rect.png", width = 20, height = 19, bg = "#1B4242")
