# Load libraries
pacman::p_load(tidyverse, scales, ggforce, showtext, ggtext)

## Load font
font_add_google("Nunito")

showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# Load data
cranes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-30/cranes.csv')

crane_spring_df <- cranes |>
  filter(format(date, "%m-%d") >= "03-07",
         format(date, "%m-%d") <= "04-30") |>
  mutate(day_of_year = format(date, "%m-%d")) |>
  group_by(day_of_year) |>
  summarise(avg_count = mean(observations, na.rm = TRUE), .groups = "drop") |>
  arrange(day_of_year) |>
  mutate(
    date_ref = as.Date(paste0("2023-", day_of_year)),
    day_label = format(date_ref, "%d %b"),
    id = row_number(),
    label_final = case_when(
      day_of_year %in% c("03-07", "03-31", "04-01", "04-30") ~ day_label, TRUE ~ "")
    ) |>
  filter(!is.na(avg_count))

# Function to generate one vane
make_vane <- function(ypos, length, curve, side = "right", id = 1, shaft_halfwidth = 0.02) {
  sgn <- ifelse(side == "right", 1, -1)
  x0 <- sgn * shaft_halfwidth   # start just outside the shaft line
  
  # Scale y steps relative to vane length
  y_steps <- c(0, 0.4 * length, length)
  
  data.frame(
    x = c(x0, x0 + sgn * curve, x0 + sgn * length),
    y = ypos + y_steps,
    side = side,
    id = id,
    group = paste0("pair_", id, "_", side)
  )
}

# Generate vanes from average observations
n_vanes <- nrow(crane_spring_df)
y_positions <- seq(-1.8, 1.8, length.out = n_vanes)
lengths <- rescale(crane_spring_df$avg_count, to = c(0.5, 2.5))
curves <- lengths * 0.3
strength <- rescale(crane_spring_df$avg_count, to = c(0.4, 1.2))

vanes <- do.call(rbind, lapply(1:n_vanes, function(i) {
  rbind(
    make_vane(y_positions[i], lengths[i], curves[i], "right", i),
    make_vane(y_positions[i], lengths[i], curves[i], "left", i)
  )
}))

# Add average observations & strength
vanes <- vanes |>
  left_join(crane_spring_df |> mutate(id = row_number(), strength = strength), by = "id")

# vane tips
tips <- vanes |>
  group_by(group, id, side) |>
  arrange(y) |>
  slice_tail(n = 3) |>
  summarise(
    x_tip = last(x),
    y_tip = last(y),
    dx = (last(x) - first(x)) / 2,
    dy = (last(y) - first(y)) / 2,
    .groups = "drop"
  ) |>
  mutate(
    label = crane_spring_df$day_label[id],
    angle = atan2(dy, dx) * 180 / pi,
    # flip upside-down left labels
    angle = ifelse(side == "left", angle + 180, angle),
    x_tip = x_tip + 0.03 * cos(angle * pi / 180) * ifelse(side == "right", 1, -1),
    y_tip = y_tip + 0.03 * sin(angle * pi / 180) + ifelse(side == "left", 0.05, 0)
  )

# Plot
ggplot() +
  # Shaft
  annotate("segment", x = 0, xend = 0, y = -2, yend = 2, linewidth = 0.8, color = "black") +
  # Vanes
  geom_bezier(data = vanes, aes(x = x, y = y, group = group, linewidth = strength, color = avg_count), alpha = 0.85, lineend = "round") +
  scale_color_gradient(
    low = "#C4DAD2", high = "#49243E",  na.value = "white",
    breaks = c(0, 3000, 6000, 9000, 12000),
    labels = c("0", "3k", "6k", "9k", "12k"),   
    guide = guide_colorsteps(
      title = "Average Obervations",
      title.position = "top",
      title.hjust = 0.5,
      barheight = unit(0.22, "cm"),
      barwidth  = unit(5, "cm")
    )) +
  scale_linewidth(range = c(0.3, 1.5), guide = "none") +
  geom_text(
    data = tips,
    aes(x = x_tip, y = y_tip, label = crane_spring_df$label_final[id], fontface = "bold", angle = angle, family = "Nunito", hjust = ifelse(side == "right", 0, 1)), size = 2, colour = "#615EFC") +
  coord_equal() +
  theme_void() +
  labs(
    title = "Spring Cranes",
    subtitle = "Average Observations at Lake Hornborgasjön, Sweden (1994–2024)",
    caption = "Graphic: Manasseh Oduor | Source: www.hornborga.com \n #TidyTuesday {wk:39-2025}",
    color = "Average Observations") +
  theme(
    text = element_text(family = "Nunito"),
    plot.title = element_text(hjust = 0.5, size = 25, family = "Nunito", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, family = "Nunito", margin = margin(b=20,t=10)),
    plot.margin = margin(t=20,b=20,l=20,r=20),
    legend.position = "top",
    plot.caption = element_text(size = 9, family = "Nunito", hjust = 0.5, color = "black", margin = margin(t=10))
  )

ggsave("Cranes.png", height = 9, width = 9, bg = "#FFFDF6")
