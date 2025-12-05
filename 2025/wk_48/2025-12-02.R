# Load libraries
pacman::p_load(tidyverse, showtext, ggtext)

# Load fonts
font_add_google(name = "Urbanist")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 320)

# Load data
sechselaeuten <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-02/sechselaeuten.csv')

# Data Wrangling
sechselaeuten <- sechselaeuten |> 
  filter(year >=2011) |> 
  drop_na()

n_pts_per_yr <- 5000 # Points per year
flame_list <- list()

set.seed(42)

for(i in 1:nrow(sechselaeuten)) {
  yr <- sechselaeuten$year[i]
  dur <- sechselaeuten$duration[i]
  
  # Initialize particles
  temp_df <- data.frame(id = 1:n_pts_per_yr)
  temp_df$year <- yr
  
  # Flame shape logic
  max_dur <- max(sechselaeuten$duration)
  flame_h <- 8 + ((max_dur - dur) / max_dur) * 7
  temp_df$y <- rbeta(n_pts_per_yr, 1, 3) * flame_h
  
  # Width/Wobble
  wobble <- sin(temp_df$y) * 0.5
  width_calc <- (1 - (temp_df$y / flame_h))
  temp_df$x <- rnorm(n_pts_per_yr, mean = wobble, sd = width_calc * 0.8)
  
  # Heat/Color logic: We normalize heat. If duration is short, we boost the base heat so it maps to White/Yellow.
  # If duration is long, we lower base heat so it maps to Red/Black.
  heat_pen <- (dur / max_dur) 
  
  # Base heat calculation
  base_heat <- (1 - temp_df$y / flame_h) * (1 - abs(temp_df$x)/3)
  
  # Apply penalty: Longer duration, Lower Heat value, and Darker colors
  temp_df$heat <- base_heat - (heat_pen * 0.5)
  
  # Clip heat at 0 to prevent errors
  temp_df$heat[temp_df$heat < 0] <- 0
  
  flame_list[[i]] <- temp_df
}

# Merge all years into one df
flame_df <- do.call(rbind, flame_list)

# Sun viz
sun_df <- sechselaeuten |>
  mutate(
    x_pos = 6, y_pos = 14, 
    sun_size_scaled = sre000m0 / 8, 
    sunshine_per_d = sre000m0 / 31, 
    label = paste0(year, "\n", duration, " min\n",  round(sunshine_per_d, 1), " hrs/day sunshine"))

# Plot
ggplot() +
  geom_point(data = sun_df, aes(x = x_pos, y = y_pos, size = sun_size_scaled), color = "#ffd700", alpha = 0.3) +
  geom_point(data = sun_df, aes(x = x_pos, y = y_pos, size = sun_size_scaled * 0.6), color = "#fafadc", alpha = 0.8) +
  geom_point(data = sun_df, aes(x = x_pos, y = y_pos, size = sun_size_scaled,  alpha = sunshine_per_d / max(sunshine_per_d)), color = "#ffd700") +
  geom_point(data = flame_df, aes(x = x, y = y, color = heat), alpha = 0.05, size = 1, shape = 16) +
  geom_text(data = sun_df, aes(x = 0, y = -1, label = label), color = "#f7f0f0", size = 5.5, hjust = 0.5, vjust = 1, family = "Urbanist", fontface = "bold") +
  scale_color_gradientn(colors = c("#1f1c1c", "#590202", "#d16111", "#ffaa00", "#ffd700", "#f7f0f0"), limits = c(0, 1)) +
  scale_size_identity() +
  facet_wrap(~year, ncol = 4) +
  theme_void() +
  labs(
    title = "Fires of Forecast ????",
    subtitle = "Zürich’s Böögg and Summer Sunshine. Flame height shows explosion speed. <br> Bigger, brighter sun hint more summer sunshine. Could the snowman effigy really predict summer? Traditionally, <br> faster Böögg explosions are said to signal hotter, sunnier summers; slower ones hint at cooler, cloudier weather. <br> Following tradition, years with slower Böögg explosions - 2013, 16', 22', 23', & 2025 would have predicted <br> cooler, cloudier summers (<6 h/day sunshine). In reality, that wasn’t the case.",
    caption = "Graphic: Manasseh Oduor | #TidyTuesday {wk: 48 2025}") +
  theme(
    panel.background = element_rect(fill = "#1A3636"),
    strip.text = element_blank(),
    legend.position = "none",
    plot.title = element_text(family = "Urbanist", size = 45, color = "white", hjust = 0.5, face = "bold", margin = margin(t=10,b=15)),
    plot.subtitle = element_markdown(family = "Urbanist", size = 21, color = "white", margin = margin(t=10,b=20)),
    plot.caption = element_text(family = "Urbanist", size = 20, color = "white", margin = margin(t=20,b=20)),
    panel.spacing = unit(2, "lines")
  ) +
  coord_fixed(ratio = 1, ylim = c(-5, 18), xlim = c(-10, 10))

ggsave("Boogg.png", height = 18, width = 18, dpi = 310, bg = "#31364700")