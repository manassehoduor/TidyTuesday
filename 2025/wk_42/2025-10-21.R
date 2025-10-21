# Load libraries
pacman::p_load(tidyverse, ggfx, patchwork, showtext, ggtext)

## Load font
font_add_google("Nunito")

showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# Load data
historic_station_met <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-21/historic_station_met.csv')
station_meta <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-21/station_meta.csv')

# Data wrangling
sun_df <- historic_station_met |>
  filter(station == "aberporth" & year >= 1942) |>
  group_by(year) |>
  summarise(avg_sun = mean(sun, na.rm = TRUE)) |>
  ungroup() |>
  mutate(
    ring = row_number(),
    year_short = paste0(substr(year, 3, 4), "'"),
    generation = case_when(
      year >= 1942 & year <= 1964 ~ "Baby Boomers",
      year >= 1965 & year <= 1980 ~ "Generation X",
      year >= 1981 & year <= 1996 ~ "Millennials",
      year >= 1997 & year <= 2012 ~ "Generation Z",
      year >= 2013 & year <= 2024 ~ "Generation Alpha",
      TRUE ~ "Unknown"
    ),
    generation = factor(generation,
                        levels = c("Baby Boomers", "Generation X", "Millennials", "Generation Z", "Generation Alpha"))
  )

# Radial plots
p1 <- ggplot(sun_df, aes(x = 1, y = ring, fill = avg_sun)) +
  geom_tile(color = NA, width = 2*pi, height = 1) +
  coord_polar(theta = "x", start = 0) +
  geom_text(
    data = subset(sun_df, year %% 5 == 0),
    aes(x = 0, y = ring, label = year_short), color = "#0046FF", size = 2.5, fontface = "bold", family = "Nunito") +
  facet_wrap(~ generation, ncol = 5, scales = "free_y") +
  scale_fill_gradientn(
    colors = c("#B12C00", "#F26B0F", "#FDB813", "#FFDC00", "#FFF100", "#FFF8D9"),
    guide = guide_colorsteps(
      title = "Annual Mean Sunshine Duration (hrs)",
      title.position = "top",
      title.hjust = 0.5,
      barheight = unit(0.22, "cm"),
      barwidth  = unit(8, "cm")
    )) +
  labs(
    title = "Shifting Glow of UK Sunshine at Aberporth",
    subtitle = "1942–2024"
  ) +
  theme_void(base_size = 14) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(family = "Nunito", color = "black", size = 14, face = "bold"),
    legend.title = element_text(size = 12, family = "Nunito", color = "grey20"),
    legend.text = element_text(size = 9, family = "Nunito"),
    plot.title = element_text(face = "bold", size = 23, family = "Nunito", hjust = 0.5, color = "black", margin = margin(b=5)),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "#432323", family = "Nunito"),
    plot.background = element_rect(fill = "#D6ECFF", color = "#D6ECFF"),
    panel.background = element_rect(fill = "#D6ECFF", color = "#D6ECFF"),
    plot.margin = margin(10, 10, 10, 10))

p2 <- ggplot(sun_df, aes(x = 1, y = ring, fill = avg_sun)) +
  geom_tile(color = NA, width = 2*pi, height = 1) +
  coord_polar(theta = "x", start = 0) +
  with_outer_glow(
    geom_text(
      data = subset(sun_df, year %% 5 == 0),
      aes(x = 0, y = ring, label = year_short), color = "white", size = 2.5, fontface = "bold", family = "Nunito"), sigma = 1, expand = 3, alpha = 0.8) +
  scale_fill_gradientn(
    colors = c("#B12C00", "#F26B0F", "#FDB813", "#FFDC00", "#FFF100", "#FFF8D9")) +
  labs(
    caption = "Graphic: Manasseh Oduor \n Source: metoffice.gov.uk"
  ) +
  theme_void(base_size = 14) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#D6ECFF", color = "#D6ECFF"),
    panel.background = element_rect(fill = "#D6ECFF", color = "#D6ECFF"),
    plot.caption = element_text(size = 13, hjust = 1, color = "grey10", family = "Nunito", margin = margin(t=20,b=5)),
    plot.margin = margin(2, 2, 2, 2)) 
  
p3 = p1 / p2 + plot_layout(heights = c(1, 2)) 

# Save
ggsave("sunshine.png", p3, height = 11, width = 9.5, bg = "#D6ECFF")