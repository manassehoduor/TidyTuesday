# Load packages
pacman::p_load(tidyverse, ggtext, showtext, svgparser, geomtextpath)

# Load fonts
font_add_google("Urbanist")
font_add(family = "devora", regular = "Devora.otf")

showtext_auto(enable = TRUE)
showtext_opts(dpi = 400)

# Load data
beaufort_scale <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/beaufort_scale.csv')
birds <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/birds.csv')
sea_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/sea_states.csv')
ships <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/ships.csv')

# Load svg
bird_df <- svgparser::read_svg("bird_n.svg", obj_type = 'data.frame')

# Data wrangling
birds_ships_df <- ships |> 
  inner_join(birds, by = "record_id") |>
  filter(!is.na(wind_speed_class)) |> 
  group_by(wind_speed_class) |>
  summarize(avg_birds = mean(count, na.rm = TRUE)) |> 
  filter(!is.nan(avg_birds))

sightings_df <- birds_ships_df |> 
  left_join(beaufort_scale, by = "wind_speed_class") |> 
  select(wind_speed_class, wind_description, avg_birds) |> 
  rename(beaufort_class = "wind_speed_class",
         wind_label = "wind_description") |> 
  mutate(wind_label = str_to_title(wind_label),
         wind_label = paste0(wind_label, " (", round(avg_birds), ")"))

# Identify body path
body_idx <- 5

# Root centroid (all paths)
root_x <- mean(bird_df$x, na.rm = TRUE)
root_y <- mean(bird_df$y, na.rm = TRUE)

# Feather centroids & Beaufort assignment
beaufort_map <- bird_df |>
  group_by(elem_idx) |>
  summarise(cx = mean(x, na.rm = TRUE),
            cy = mean(y, na.rm = TRUE),
            .groups = "drop") |>
  filter(elem_idx != body_idx) |>
  mutate(
    angle_rad = atan2(-(cy - root_y), cx - root_x),
    angle_deg = angle_rad * 180 / pi,
    angle_deg = if_else(angle_deg < 0, angle_deg + 360, angle_deg)) |>
  arrange(angle_deg) |>
  bind_cols(sightings_df)

# Merge datasets
bird_colored <- bird_df |> left_join(beaufort_map, by = "elem_idx")

feathers <- bird_colored |> filter(!is.na(beaufort_class))
body     <- bird_colored |> filter(elem_idx == body_idx)

# Eye position
eye <- data.frame(x = 661, y = 585)

# Plot
ggplot() +
  geom_polygon(data = body, aes(x = x * 0.1, y = y * 0.1, group = elem_idx), fill = "#5e0a28", color = "#5e0a28", linewidth = 0.8) +
  geom_polygon(data = feathers, aes(x = x * 0.1, y = y * 0.1, group = elem_idx, fill = avg_birds), color = "#deeffc", linewidth = 0.1) +
  geom_textpath(data = feathers, aes(x = x * 0.1, y = y * 0.1, group = elem_idx, label = wind_label), color = NA,
    textcolour = "#8aaab8", size = 3.5, fontface   = "italic", hjust = 0.8, vjust = -1.5, upright = TRUE, gap = FALSE, family = "Urbanist", padding = unit(0.1, "pt")) +
  geom_point(data = eye, aes(x = x, y = y), shape = 21, size = 2, fill = "#ccff00", color = "white", stroke = 1.2) +
  scale_fill_gradient2(
    name = "Average birds per observation",
    low = "#E5F1FB", mid = "#378ADD", high = "#08346C", 
    midpoint = 200, limits = c(0, 400), breaks = seq(0, 400, 50),
    labels = c("0", "50", "100", "150", "200", "250", "300", "350", "400"),
    guide = guide_colorbar(
      title.position = "top", title.hjust = 0.5, barwidth = unit(15, "cm"), barheight = unit(0.3, "cm"),
      ticks.colour = "#D6E8F0", ticks.linewidth = 0.3, frame.colour = "#D6E8F0", draw.ulim = FALSE, draw.llim = FALSE)) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 13, color = "#5F5E5A", family = "Urbanist"),
    legend.text = element_text(size = 10, color = "#5F5E5A", family = "Urbanist"),
    legend.ticks.length = unit(0.3, "cm"),
    legend.ticks = element_line(color = "#D6E8F0"),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    plot.margin = margin(l=30, r=30, t=40, b=20),
    plot.title = element_text(size = 35, face = "bold", family = "devora", hjust = 0.5, color = "#2a2825", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 15, hjust = 0.5, family = "Urbanist", color = "#888780", margin = margin(b = 15)),
    plot.caption = element_text(size = 12, hjust = 0.5, family = "Urbanist", color = "#888780", margin = margin(t = 25))) +
  labs(
    title    = "Wings on the Wind",
    subtitle = "Wind velocity dictates Seabird abundance & Energy harvesting  \n in the Tasman Sea, New Zealand and Australian waters (1969–1990)",
    caption  = "Graphic: Manasseh Oduor \n Source: Museum of New Zealand Te Papa Tongarewa")

ggsave("bird_sighting_n.png", height = 10, width = 8, dpi = 400, bg = "#fff")
