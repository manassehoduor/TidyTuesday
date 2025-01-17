# Load Libraries
pacman::p_load(tidyverse, ggtext, showtext)

# Load data
conf2023 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-14/conf2023.csv')
conf2024 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-14/conf2024.csv')

# Load Fonts
font_add_google(name = "Rosario")
font_add_google(name = "Averia Sans Libre")
font_add_google(name = "Averia Gruesa Libre")
font_add_google(name = "Averia Serif Libre")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

title_rad <- -1
freq_labels_rad <- 0.03

conf2023_tt <- conf2023 |>
  select(session_title, session_type) |> 
  mutate(
    session_title_ed = str_wrap(session_title, width = 75),
    angle = seq(0, 2 * pi, length.out = n() + 1)[-1],
    x = title_rad * cos(angle),
    y = title_rad * sin(angle),
    text_angle = angle * 180 / pi
  )

conf2023_tt_freq <- conf2023_tt |> 
  count(session_type) |>
  mutate(
    label = paste0(session_type, " = ", n),
    x = 0,
    y = seq(freq_labels_rad, -freq_labels_rad, length.out = n())
  )

conf2024_tt <- conf2024 |>
  select(talk_title, track) |> 
  mutate(
    talk_title_ed = str_wrap(talk_title, width = 75),
    session_type = case_when(
      track == "Keynote" ~ "keynote",
      track == "Lightning Talks" ~ "lightning",
      TRUE ~ "regular"
    ),
    angle = seq(0, 2 * pi, length.out = n() + 1)[-1],
    x = title_rad * cos(angle),
    y = title_rad * sin(angle),
    text_angle = angle * 180 / pi
  )

conf2024_tt_freq <- conf2024_tt |> 
  count(session_type) |>
  mutate(
    label = paste0(session_type, " = ", n),
    x = 0,
    y = seq(freq_labels_rad, -freq_labels_rad, length.out = n())
  )

# colors
session_type_col <- c("keynote" = "red", "regular" = "blue", "lightning" = "#3B1C32")

# Plot 2023
ggplot(conf2023_tt, aes(x = x, y = y, label = session_title_ed, color = session_type)) +
  geom_text(
    aes(angle = text_angle, hjust = 0, vjust = 0.5), size = 3, family = "Rosario"
  ) +
  geom_text(data = conf2023_tt_freq, aes(x = x, y = y, label = label, color = session_type), size = 5,
            inherit.aes = FALSE, family = "Averia Sans Libre") +
  scale_color_manual(values = session_type_col) +
  theme_void() +
  theme(
    text = element_text(family = "Averia Gruesa Libre"),
    plot.title = element_text(size = 22, color = "#09122C", face = "bold", family = "Averia Serif Libre", hjust = 0.5, margin = margin(t=20)),
    legend.position = "none") +
  labs(
    title = "posit::conf(2023)")

ggsave("conf2023_talks.png", width = 14, height = 14, bg = "#fff")

# Plot 2024
ggplot(conf2024_tt, aes(x = x, y = y, label = talk_title_ed, color = session_type)) +
  geom_text(
    aes(angle = text_angle, hjust = 0, vjust = 0.5), size = 3, family = "Rosario"
  ) +
  geom_text(data = conf2024_tt_freq, aes(x = x, y = y, label = label, color = session_type), size = 5,
            inherit.aes = FALSE, family = "Averia Sans Libre") +
  scale_color_manual(values = session_type_col) +
  theme_void() +
  theme(
    text = element_text(family = "Averia Gruesa Libre"),
    plot.title = element_text(size = 22, color = "#09122C", face = "bold", family = "Averia Serif Libre", hjust = 0.5, margin = margin(t=20)),
    legend.position = "none") +
  labs(
    title = "posit::conf(2024)")

ggsave("conf2024_talks.png", width = 14, height = 14, bg = "#fff")