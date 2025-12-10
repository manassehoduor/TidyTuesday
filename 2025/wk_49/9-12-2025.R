# Load libraries
pacman::p_load(tidyverse, scales, ggnewscale, grid, png, ggbeeswarm, showtext, ggtext)

# Load fonts
font_add_google(name = "Urbanist")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 350)

# Load data
qatarcars <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-09/qatarcars.csv')

car_raster <- png::readPNG("car.png")
car_grob <- rasterGrob(car_raster, width = unit(1, "npc"), height = unit(2, "npc"))

# Data Wrangling
cars <- qatarcars |>
  filter(origin == "Germany") |>
  mutate(
    enginetype = tolower(trimws(enginetype)),
    enginetype = factor(enginetype, levels = c("electric", "hybrid", "petrol")),
    type = factor(type, levels = sort(unique(type))),
    x = scales::rescale(performance, to = c(1, 9), from = rev(range(performance, na.rm = TRUE))),
    clean_y = as.numeric(type) * 2)

# Overlap avoidance
set.seed(123)
cars <- cars |>
  group_by(type) |>
  mutate(
    y_jitter = clean_y + seq(-0.65, 0.65, length.out = n())) |>
  ungroup()

engine_cols <- c(
  electric = "#2d8cff",
  hybrid   = "#ffd43b",
  petrol   = "#ff3b3b")

cars <- cars |>
  mutate(
    type = factor(type, levels = c("Hatchback", "Sedan", "SUV",  "Coupe")),
    clean_y = as.numeric(type) * 2
  )

# Lane background
lane_data <- distinct(cars, clean_y, type) |> rename(y = clean_y)

set.seed(123)
cars <- cars |>
  group_by(type) |>
  mutate(
    y_jitter = clean_y + seq(-0.65, 0.65, length.out = n())) |>
  ungroup()

# Finish line
flag <- expand.grid(
  x = seq(9.5, 10, by = 0.1),
  y = seq(0, max(cars$clean_y) + 2, by = 0.5)) |> 
  mutate(tile = factor((round(x*10) + round(y*10)) %% 2))

# Plot
p <- ggplot() +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, max(cars$clean_y) + 1.5)) +
  geom_rect(
    data = lane_data,
    aes(xmin = 0, xmax = 10, ymin = y - 1.2, ymax = y + 1.2, fill = type), show.legend = FALSE, color = "grey30", linewidth = 0.3, linetype = "solid") +
  scale_fill_manual(values = c("grey30", "gray35", "gray40", "grey45"), name = "Car Type Lane") +
  new_scale_fill() +
  geom_tile(data = flag, aes(x = x, y = y, fill = tile)) +
  scale_fill_manual(values = c("black", "white"), guide = "none") +
  geom_point(data = cars, aes(x = x, y = y_jitter, color = enginetype), shape = 15, size = 5.5, alpha = 0.9) +
  scale_color_manual(values = engine_cols, name = "Engine Type") +
  geom_text(
    data = cars |> filter(model != "Macan Standard"),
    aes(x = x, y = y_jitter, label = paste0(performance, "s")),
    nudge_x = 0.35, size = 2.2, fontface = "bold", color = "white", family = "Urbanist") +
  geom_text(
    data = cars |> filter(performance != 7.5 & model != "Macan Standard"),
    aes(x = x, y = y_jitter, label = paste0(horsepower, " hp")),
    nudge_x = -0.45, size = 2.2, fontface = "bold", color = "#6AECE1", family = "Urbanist") +
  geom_text(
    data = cars |> filter(performance == 7.5),
    aes(x = x, y = y_jitter, label = paste0(horsepower, " hp")),
    nudge_x = 0, nudge_y = 0.3, size = 2.2, fontface = "bold", color = "#6AECE1", family = "Urbanist") +
  geom_text(
    data = cars |> filter(model == "Macan Standard"),
    aes(x = x, y = y_jitter, label = paste0(horsepower, " hp")),
    nudge_x = 0, nudge_y = 0.3, size = 2.2, fontface = "bold", color = "#6AECE1", family = "Urbanist") +
  geom_text(
    data = cars |> filter(model == "Macan Standard"),
    aes(x = x, y = y_jitter, label = paste0(performance, "s")),
    nudge_x = 0.35, nudge_y = 0.3, size = 2.2, fontface = "bold", color = "#6AECE1", family = "Urbanist") +
  geom_text(
    data = cars |> filter(performance == 3.3),
    aes(x = x, y = y_jitter, label = paste0(make, " ", model)),
    nudge_x = -0.35, nudge_y = 0.3, size = 2.5, fontface = "bold", color = "#BBCB64", family = "Urbanist") +
  geom_text(
    data = cars |> filter(performance == 3.6),
    aes(x = x, y = y_jitter, label = paste0(make, " ", model)),
    nudge_x = 0.1, nudge_y = 0.3, size = 2.4, fontface = "bold", color = "#BBCB64", family = "Urbanist") + 
  geom_text(
    data = cars |> filter(performance == 3.8),
    aes(x = x, y = y_jitter, label = paste0(make, " ", model)),
    nudge_x = 0.1, nudge_y = 0.3, size = 2.3, fontface = "bold", color = "#BBCB64", family = "Urbanist") + 
  geom_text(
    data = cars |> filter(performance == 4.8),
    aes(x = x, y = y_jitter, label = paste0(make, " ", model)),
    nudge_x = 0.1, nudge_y = 0.3, size = 2.2, fontface = "bold", color = "#BBCB64", family = "Urbanist") + 
  geom_text(
    data = cars |> filter(performance == 4.1),
    aes(x = x, y = y_jitter, label = paste0(make, " ", model)),
    nudge_x = 0.1, nudge_y = -0.3, size = 2.2, fontface = "bold", color = "#BBCB64", family = "Urbanist") + 
  geom_text(
    data = cars |> filter(performance == 4.3),
    aes(x = x, y = y_jitter, label = paste0(make, " ", model)),
    nudge_x = 0.1, nudge_y = -0.3, size = 2.2, fontface = "bold", color = "#BBCB64", family = "Urbanist") + 
  geom_text(
    data = cars |> filter(performance == 9),
    aes(x = x, y = y_jitter, label = paste0(make, " ", model)),
    nudge_x = -0.1, nudge_y = 0.25, size = 2.2, fontface = "bold", color = "#FFE52A", family = "Urbanist") + 
  geom_text(
    data = cars |> filter(horsepower == 333),
    aes(x = x, y = y_jitter, label = paste0(make, " ", model)),
    nudge_x = -0.1, nudge_y = 0.25, size = 2.2, fontface = "bold", color = "#00CAFF", family = "Urbanist") + 
  scale_y_continuous(
    breaks = unique(lane_data$y),
    labels = unique(lane_data$type),
    name = "Car Type (Road Lane)") +
  labs(
    title = "German Cars 0–100 km/h Acceleration Race Simulation",
    subtitle = "Position reflects the relative completion status of the 0-100 km/h acceleration task, scaled by time",
    x = "Progress Towards 100 km/h (Scaled Performance Score)",
    caption = "Graphic: Manasseh Oduor | Source: qatarcars R package | #TidyTuesday {wk: 49}") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "gray15"),
    panel.background = element_rect(fill = "gray15"),
    plot.title = element_text(color = "white", face = "bold", hjust = 0.5, family = "Urbanist", size = 14),
    plot.subtitle = element_text(color = "gray80", hjust = 0.5, family = "Urbanist", size = 11, margin = margin(t=10)),
    axis.text.y = element_text(color = "white", face = "bold", family = "Urbanist", hjust = 1),
    axis.title.y = element_blank(),
    axis.title.x = element_text(color = "white", family = "Urbanist", vjust = 15, margin = margin(t=2)),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(0.5, -0.001),
    legend.direction = "horizontal",
    legend.text = element_text(color = "white", family = "Urbanist"),
    legend.title = element_text(color = "white", face = "bold", family = "Urbanist"),
    plot.caption = element_text(family = "Urbanist", size = 9, hjust = 0.5, colour = "#BBFBFF", margin = margin(t=10,b=10)),
    plot.margin = margin(t=20,b=5,l=20,r=20)
  )

# Add car images
for (i in 1:nrow(cars)) {
  p <- p + annotation_custom(
    grob = car_grob,
    xmin = cars$x[i] - 0.22, xmax = cars$x[i] + 0.22,
    ymin = cars$y_jitter[i] - 0.15, ymax = cars$y_jitter[i] + 0.15
  )
}

ggsave("Qatarcars.png", p, width = 12, height = 8, dpi = 300)