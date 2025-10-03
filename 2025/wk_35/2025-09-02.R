# Load libraries
pacman::p_load(tidyverse, ggforce, ggtext, showtext, scales)

# Load Fonts
font_add_google(name = "Nunito Sans")
font_add_google(name = "Antic")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load data
frogID_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frogID_data.csv')
frog_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frog_names.csv')

# Data wrangling
frog_names_clean <- frog_names |>
  distinct(scientificName, .keep_all = TRUE)   # keeps first occurrence

frog_data_named <- frogID_data |>
  left_join(frog_names_clean |> select(scientificName, commonName), by = "scientificName") |> 
  drop_na()

# Top 10 species
top_species <- frog_data_named |>
  count(commonName, sort = TRUE) |>
  top_n(10, n) |>
  pull(commonName)

# Keep only top 10 species
frog_data_top <- frog_data_named |>
  filter(commonName %in% top_species)

# Build frog bars dataset
df <- frog_data_top |>
  count(commonName, name = "y") |>
  arrange(desc(y)) |>
  mutate(
    x = row_number(),
    species = commonName
  )

# Function to build convex "frog bar" with dome top
frog_bar <- function(x, height, width = 0.8, dome_height = 6, n = 200) {
  # Dome curve
  t <- seq(0, 1, length.out = n)
  x_curve <- (1 - t) * (x - width/2) + t * (x + width/2)
  y_curve <- (1 - t)^2 * height +
    2 * (1 - t) * t * (height + dome_height) +
    t^2 * height
  
  # Polygon (rectangle + dome)
  polygon_df <- data.frame(
    x = c(x - width/2, x_curve, x + width/2, x + width/2, x - width/2),
    y = c(0, y_curve, height, 0, 0)
  )
  
  # Dome curve for eye placement
  curve_df <- data.frame(x = x_curve, y = y_curve)
  
  list(polygon = polygon_df, curve = curve_df, dome_start_y = height)
}

avg_rich <- mean(df$y)

# Build frog bars
bars_list <- lapply(1:nrow(df), function(i) {
  frog_bar(df$x[i], df$y[i], dome_height = 4000)  # same dome height for all bars
})

# Polygons
bars <- bind_rows(lapply(1:length(bars_list), function(i) {
  cbind(bars_list[[i]]$polygon, id = i, species = df$commonName[i])
}))

# Eyes
eyes <- bind_rows(lapply(1:length(bars_list), function(i) {
  dome <- bars_list[[i]]$curve
  idx_left  <- round(nrow(dome) * 0.25)
  idx_right <- round(nrow(dome) * 0.75)
  
  data.frame(
    id = i,
    species = df$commonName[i],
    eye_x = c(dome$x[idx_left], dome$x[idx_right]),
    eye_y = c(dome$y[idx_left], dome$y[idx_right])
  )
}))

# Fade line
fade_lines <- data.frame(
  x1 = df$x - 0.4,
  x2 = df$x + 0.4,
  y = df$y
)

# Mouths
mouths <- df |>
  mutate(
    id = row_number(),
    mood = ifelse(y >= avg_rich, "frown", "smile"),
    y_mouth = ifelse(y >= avg_rich, y + 500, y + 700),
    x_start = x - 0.15,
    x_end   = x + 0.15
  )

smiles <- mouths |> filter(mood == "smile")
frowns <- mouths |> filter(mood == "frown")

# Add labels inside bars
bar_labels <- df |>
  mutate(
    label_y = y / 2  # halfway up each bar
  )

ggplot() +
  # Frog bars with species color
  geom_polygon(data = bars, aes(x, y, group = id, fill = species),
               color = "darkgreen", linewidth = 0.8) +
  
  # Faded horizontal line under dome
  geom_segment(data = fade_lines,
               aes(x = x1, xend = x2, y = y, yend = y),
               color = "darkgreen", alpha = 0.3, linewidth = 2) +
  
  # Average line
  geom_hline(yintercept = avg_rich, linetype = "dotted", color = "red", size = 0.2) +
  annotate("text", x = max(df$x) + 0.5, y = avg_rich, label = avg_rich, 
           color = "red", hjust = -3) +
  
  # Outer eyes
  geom_point(data = eyes, aes(eye_x, eye_y),
             size = 7, shape = 21, fill = "white", color = "black", stroke = 0.8) +
  
  # Pupils
  geom_point(data = eyes, aes(eye_x, eye_y),
             size = 3.5, shape = 21, fill = "black") +
  
  # Smiles / frowns
  geom_curve(data = smiles,
             aes(x = x_start, y = y_mouth, xend = x_end, yend = y_mouth),
             curvature = -0.4, color = "black", size = 1) +
  geom_curve(data = frowns,
             aes(x = x_start, y = y_mouth, xend = x_end, yend = y_mouth),
             curvature = 0.4, color = "black", size = 1) +
  
  # Count labels inside bars
  geom_text(
    data = bar_labels,
    aes(x = x, y = label_y, label = comma(y)),  # comma() formats numbers
    color = "black", family = "Nunito Sans",
    fontface = "bold"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_void() +
  labs(
    title = "Leap to the Top",
    subtitle = "Top 10 Frog Species Abundance in Australia, 2023"
  ) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 20, hjust = 0.5, family = "Nunito Sans", face = "bold"),
        plot.subtitle = element_text(size = 15, hjust = 0.5, family = "Antic"),
        plot.margin = margin(r=20, l=20, t=20, b=20),
        legend.position = "none"
        )

ggsave("frogs.png", height = 8, width = 12, bg = "#FFEDFA")