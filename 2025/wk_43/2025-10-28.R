# Load packages
pacman::p_load(tidyverse, geomtextpath, ggfx, showtext, ggtext)

## Load font
font_add_google("Nunito")

showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# Load data
prizes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-28/prizes.csv')

prizes_df <- prizes |> 
  filter(person_role == "winner") |> 
  select(prize_year, prize_alias, gender)

winners_df = prizes_df |>
  group_by(prize_year, prize_alias) |>
  summarise(
    n_winners = n(),
    genders = paste(unique(gender), collapse = ", "),
    .groups = "drop"
  ) |>
  filter(n_winners > 1)

all_years_v <- 1991:2022
gap_v <- paste0("Gap", 1:11)  # ~90° void
year_gaps_v <- c(as.character(all_years_v), gap_v)
awards_v <- unique(prizes_df$prize_alias)

awards_order <- c(
  "James Tait Black Prize for Drama",
  "Ted Hughes Award for New Work in Poetry",
  "Baillie Gifford Prize for Non-Fiction",
  "TS Eliot Prize",
  "James Tait Black Prize for Fiction",
  "Costa Book of the Year",
  "Costa Children's Book Award",
  "Costa Biography Award",
  "Costa Poetry Award",
  "Costa Novel Award",
  "Costa First Novel Award",
  "BSFA Award for Best Novel",
  "Gold Dagger",
  "Booker Prize",
  "Women's Prize for Fiction"
)

prizes_df2 <- expand_grid(
  Year = factor(year_gaps_v, levels = year_gaps_v),
  awards_cat = factor(awards_v, levels = awards_order))|>
  mutate(
    year_num = as.numeric(as.character(Year)),
    year_d = as.integer(factor(Year, levels = year_gaps_v)),
    awards_cat_num = as.integer(factor(awards_cat, levels = awards_v)))|>
  left_join(prizes_df, by = c("year_num" = "prize_year", "awards_cat" = "prize_alias"))

prizes_df3 <- prizes_df2|>
  mutate(
    awards_cat = factor(awards_cat, levels = awards_order),
    awards_cat_num = as.integer(awards_cat),
    gender = case_when(
      awards_cat == "BSFA Award for Best Novel" &
        year_num == 2013 ~ "mixed (man/woman)",
      
      awards_cat == "Costa Biography Award" &
        year_num == 2012 ~ "couple (man/woman)",
      
      awards_cat == "James Tait Black Prize for Fiction" &
        year_num == 1997 ~ "mixed (man/woman)",
      
      awards_cat == "Booker Prize" &
        year_num == 1992 ~ "man (shared)",
      
      awards_cat == "Booker Prize" &
        year_num == 2019 ~ "woman (shared)",
      TRUE ~ gender
    )
  )

# Curved year df
year_lab_df <- tibble(
  label = all_years_v,  # 1991–2022
  x_start = seq_along(all_years_v) - 0.4,
  x_end   = seq_along(all_years_v) + 0.4,
  y       = max(prizes_df3$awards_cat_num, na.rm = TRUE) + 0.4)|>
  pivot_longer(cols = c(x_start, x_end), names_to = "pos", values_to = "x")|>
  arrange(label, x)|>
  group_by(label)|>
  mutate(group_id = cur_group_id())|>
  ungroup()

cols_v <- c(
  "man" = "#FFE45E",
  "woman" = "#0118D8",
  "non-binary" = "#393E46",
  "man (shared)" = "#FFC107",
  "woman (shared)" = "#C6CFFF",
  "mixed (man/woman)" = "#476072",
  "couple (man/woman)" = "#CE7777"
)

# Awards label
label_df <- tibble(
  award = awards_order,
  y_pos = seq_along(awards_order)
)

# Plot 1
ggplot(prizes_df3, aes(x = year_d, y = awards_cat_num)) +
  geom_tile(
    data = subset(prizes_df3, !Year %in% gap_v),
    aes(fill = gender), color = "white", size = 0.3) +
  geom_textpath(
    data = year_lab_df,
    aes(x = x, y = y, label = label, group = group_id),
    size = 4.5, color = "black", family = "Nunito", vjust = -0.5, upright = TRUE, text_smoothing = 15, text_only = TRUE, linewidth = 0, inherit.aes = FALSE) +
  with_outer_glow(
    geom_text(
      data = label_df,
      aes(x = length(year_gaps_v), y = y_pos, label = award),
      hjust = 1.05, size = 4, family = "Nunito", color = "black", inherit.aes = FALSE), sigma = 2, expand = 0.2, x_offset = 0, y_offset = 0, colour = "black", alpha = 0.9) +
  scale_fill_manual(values = cols_v, na.value = "#B6CEB4", guide = "none") +
  scale_y_continuous(limits = c(-5, max(prizes_df3$awards_cat_num, na.rm = TRUE) + 1), 
                     expand = c(0, 0)) +
  scale_x_continuous(
    breaks = seq_along(year_gaps_v),
    labels = c(all_years_v, rep("", length(gap_v))),
    expand = c(0, 0)) +
  coord_polar(theta = "x", start = 0, direction = 1, clip = "off") +
  labs(
    title = "Evolution of British Literary Awards & Honorees") +
  annotate(
    "text", x = mean(c(1, length(all_years_v))) / 2, y = -5,
    label = "PRIZES \n EVOLUTION", fontface = "bold", family = "Nunito", size = 6, lineheight = 0.9) +
  theme_void() +
  theme(
    plot.title = element_text(family = "Nunito", size = 22, face = "bold", hjust = 0.5, colour = "black", margin = margin(t=5)),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(t=10,b=10,l=10,r=10)
  )

ggsave("UK_Literary_Prizes.png", height = 12, width = 12, bg = "#F9F5F0")

# GPI
gpi_yearly <- prizes_df |>
  filter(prize_alias != "Women's Prize for Fiction") |> 
  group_by(prize_year, prize_alias) |>
  summarise(
    gender_twin = paste(gender, collapse = ", "),
    n_winners = n(),
    .groups = "drop"
  ) |>
  mutate(
    gender_f = case_when(
      gender_twin == "man, man" ~ "man (shared)",
      gender_twin == "woman, woman" ~ "woman (shared)",
      gender_twin == "man, woman" | gender_twin == "woman, man" ~ "mixed (man/woman)",
      TRUE ~ gender_twin)) |>
  mutate(
    male_weight = case_when(
      gender_f == "man" ~ 1,
      gender_f == "man (shared)" ~ 0.5,
      gender_f == "mixed (man/woman)" ~ 0.5,
      TRUE ~ 0),
    female_weight = case_when(
      gender_f == "woman" ~ 1,
      gender_f == "woman (shared)" ~ 0.5,
      gender_f == "mixed (man/woman)" ~ 0.5,
      TRUE ~ 0)) |>
  group_by(prize_year) |> # replace with "prize_alias" for gpi_awards
  summarise(
    male = sum(male_weight, na.rm = TRUE),
    female = sum(female_weight, na.rm = TRUE),
    total_awards = n(),
    gpi = ifelse(male == 0 & female == 0, NA,
                 ifelse(male == 0, Inf, female / male)),
    .groups = "drop") |> 
  mutate(
    gpi_range = case_when(
      gpi < 0.5 ~ "Extreme male dominance",
      gpi < 1.0 ~ "Moderate to slight male dominance",
      gpi == 1.0 ~ "Perfect parity",
      gpi < 1.5 ~ "Moderate to slight female dominance",
      TRUE ~ "Extreme female dominance"))

max_height <- max(gpi_yearly$total_awards)

gpi_df <- gpi_yearly |>
  mutate(x = prize_year, y = total_awards)

# Plot 2
ggplot(gpi_df) +
  geom_segment(
    aes(x = x, xend = x, y = 0, yend = y, color = gpi_range),
    linewidth = 1.5, lineend = "round", alpha = 0.9) +
  geom_text(
    data = gpi_df |> filter(gpi_range != "Extreme female dominance"),
    aes(x = x, y = y / 2, label = prize_year),
    angle = 90, vjust = 0.5, hjust = 0.5, size = 1, family = "Nunito", color = "grey40") +
  geom_text(
    data = gpi_df |> filter(gpi_range %in% c("Extreme female dominance", "Moderate to slight female dominance")),
    aes(x = x, y = y / 2, label = prize_year),
    angle = 90, vjust = 0.5, hjust = 0.5, size = 1, family = "Nunito", color = "white") +
  scale_color_manual(
    values = c(
      "Extreme male dominance" = "#FFE45E",          
      "Moderate to slight male dominance" = "#FFFD8F", 
      "Perfect parity" = "#E9EED9",
      "Moderate to slight female dominance" = "#7A4CFF",
      "Extreme female dominance" = "#0118D8")) +
  scale_x_continuous(
    breaks = seq(1991, 2022, by = 5),
    expand = expansion(mult = c(0.5, 0.5))) +
  scale_y_continuous(expand = expansion(mult = c(1, 1))) +
  coord_cartesian(clip = "off") +
  theme_void(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "#F9F5F0", color = NA),
    plot.background = element_rect(fill = "#F9F5F0", color = NA),
    legend.position = "none",
    legend.text = element_text(family = "Nunito", size = 9),
    legend.title = element_text(family = "Nunito", size = 10),
    axis.text.x = element_blank(),
    plot.title = element_text(face = "bold", size = 6, hjust = 0.5, family = "Nunito"),
    plot.subtitle = element_text(color = "grey40", size = 5, hjust = 0.5, family = "Nunito")
  ) +
  labs(
    title = "Gender Parity in Literary Prizes",
    subtitle = "1991–2022"
  )

ggsave("GPI_UK_Literary_Prizes.png", height = 3, width = 5, bg = "#F9F5F0")
