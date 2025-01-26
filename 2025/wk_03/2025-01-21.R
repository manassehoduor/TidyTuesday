# Load data
exped_tidy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/exped_tidy.csv')
peaks_tidy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/peaks_tidy.csv')

# Load libraries
pacman::p_load(tidyverse, patchwork, ggtext, showtext)

# Load Fonts
font_add_google(name = "Rosario")
font_add_google(name = "Averia Sans Libre")
font_add_google(name = "Averia Gruesa Libre")
font_add_google(name = "Averia Serif Libre")

showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# Data wrangling
top_peaks <- exped_tidy |>
  left_join(peaks_tidy, by = "PEAKID") |>
  group_by(PKNAME) |>
  summarise(
    exp_n = n(),
    total_deaths = sum(MDEATHS + HDEATHS, na.rm = TRUE),
    avg_success_rate = mean(SUCCESS1 | SUCCESS2 | SUCCESS3 | SUCCESS4, na.rm = TRUE)*100,
    .groups = "drop") |>
  mutate(avg_success_rate = round(avg_success_rate, 0)) |> 
  arrange(desc(exp_n)) |>
  slice_head(n = 10)

# Function to create a triangle ~ mountain shape
gen_mtn <- function(scale_factor) {
  # Simulate a mountain shape
  x <- c(0, 1, 2, 1) * scale_factor
  y <- c(0, 2, 0, 0) * scale_factor
  list(mtn = data.frame(x = x, y = y))
}

# Function to create multiple plots
mult_plts <- function(PKNAME, avg_success_rate, exp_n, total_deaths) {
  # Sun like shape
  sun_radius <- 0.3
  sun_df <- tibble(
    x = 2.8 + cos(seq(0, 2 * pi, length.out = 100)) * sun_radius,
    y = 2 + sin(seq(0, 2 * pi, length.out = 100)) * sun_radius
  )
  
  # Rays around the sun
  ray_data <- tibble(
    xstart = 2.8 + cos(seq(0, 2 * pi, length.out = 20)) * (sun_radius + 0.05),
    ystart = 2 + sin(seq(0, 2 * pi, length.out = 20)) * (sun_radius + 0.05),
    xend = 2.8 + cos(seq(0, 2 * pi, length.out = 20)) * (sun_radius + 0.2),
    yend = 2 + sin(seq(0, 2 * pi, length.out = 20)) * (sun_radius + 0.2)
  )
  
  # Fill the sun with summit success rate
  filled_circle <- tibble(
    x = 2.8 + cos(seq(0, 2 * pi * avg_success_rate / 100, length.out = 100)) * sun_radius,
    y = 2 + sin(seq(0, 2 * pi * avg_success_rate / 100, length.out = 100)) * sun_radius
  )
  
  # Plot
  ggplot() +
    coord_cartesian(xlim = c(-1, 3), ylim = c(0, 3)) +
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank()) +
    # Adding nested mountains (looping over the iterations)
    purrr::map2(1:exp_n, seq(1, exp_n), ~{
      scale_factor <- 1 - (.x * 0.001)
      mtn_df <- gen_mtn(scale_factor)
      geom_polygon(data = mtn_df$mtn, aes(x = x, y = y), fill = "#B7E0FF", color = "#2F4F4F")}) +
    geom_polygon(data = filled_circle, aes(x = x, y = y), fill = "yellow", color = NA) +
    geom_path(data = sun_df, aes(x = x, y = y), color = "black", linetype = "dotted") +
    geom_segment(data = ray_data, aes(x = xstart, y = ystart, xend = xend, yend = yend), color = "orange", size = 0.5) +
    annotate("text", x = 2.8, y = 2, label = paste0(avg_success_rate, "%"), size = 5, hjust = 0.5, color = "black", family = "Averia Sans Libre") +
    geom_text(aes(x = 1, y = 2.8, label = PKNAME), size = 6, fontface = "bold", color = "black", family = "Averia Gruesa Libre")
}

# Generate all plots 
plots <- top_peaks |>
  pmap(function(PKNAME, avg_success_rate, exp_n, total_deaths) {
    mult_plts(PKNAME, avg_success_rate, exp_n, total_deaths)
  })

# Combine plots
final_plot <- wrap_plots(plots, ncol = 2, nrow = 5) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Journey Through the Nepal Himalaya Mountaineering Expeditions",
    subtitle = "Unveiling the journey to Nepal’s most popular peaks, with insights into summit success rates, <br> total number of expeditions, and the fatilities faced along the way between 2020 and 2024",
    caption = "Graphic: Manasseh Oduor | #TidyTuesday {wk:3-2025} \n Data source: www.himalayandatabase.com",
    theme = theme(
      plot.margin = margin(t = 20, r = 80, b = 20, l = 20),
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "Averia Serif Libre", margin = margin(t=15,b=20)),
      plot.subtitle = element_markdown(size = 16, hjust = 0.2, family = "Rosario", margin = margin(b=20)),
      plot.caption = element_text(hjust = 0.5, size = 14, family = "Rosario")
    )
  )

# Save plot
ggsave("himalayas_peaks.png", plot = final_plot, width = 12, height = 15)