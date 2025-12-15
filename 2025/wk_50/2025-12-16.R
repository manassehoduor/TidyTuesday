# Load libraries
pacman::p_load(tidyverse, osmdata, sf, showtext, ggtext, patchwork)

# Load font
font_add_google("Urbanist")
font_add(family = "Optimus Princeps", regular = "OptimusPrinceps.ttf", bold = "OptimusPrincepsSemiBold.ttf")

showtext_auto(enable = TRUE)
showtext_opts(dpi = 350)

# Load data
roundabouts_clean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-16/roundabouts_clean.csv')

triomphe_france <- roundabouts_clean |> filter(grepl("Place Charles de Gaulle", name, ignore.case = TRUE), country == "France")
swindon_magic_uk <- roundabouts_clean |> filter(grepl("magic", name, ignore.case = TRUE), country == "United Kingdom")

# Define bb for the Arc de Triomphe and Magic Roundabout
location_coords_arc <- c(left = 2.29, bottom = 48.87, right = 2.30, top = 48.878)
location_coords_magic <-c(left = -1.774, bottom = 51.561, right = -1.769, top = 51.564)

# Get road features connecting the roundabout
streets_arc <- location_coords |>
  opq() |>
  add_osm_feature(key='highway',
                  value=c('motorway', 'primary', 'secondary', 'tertiary')) |>
  osmdata_sf()

streets_magic <- location_coords |>
  opq() |>
  add_osm_feature(key='highway',
                  value=c('motorway', 'primary', 'secondary', 'tertiary')) |>
  osmdata_sf()

small_streets_arc <- location_coords |>
  opq() |>
  add_osm_feature(key='highway',
                  value=c('residential', 'living_street', 'service', 'footway')) |>
  osmdata_sf()

small_streets_magic <- location_coords |>
  opq() |>
  add_osm_feature(key='highway',
                  value=c('residential', 'living_street', 'service', 'footway')) |>
  osmdata_sf()

# Building footprints
buildings_query_arc <- location_coords |>  
  opq() |>
  add_osm_feature(key = "building") |>
  osmdata_sf()

buildings_query_magic <- location_coords |>  
  opq() |>
  add_osm_feature(key = "building") |>
  osmdata_sf()

# Plot
p1 <- ggplot() +
  geom_sf(data=buildings_query_arc$osm_polygons, inherit.aes = FALSE, color = '#850E35', size = .2, alpha = .6) +
  geom_sf(data=small_streets_arc$osm_lines, inherit.aes = FALSE, color = '#850E35', size = .2, alpha = .6) +
  geom_sf(data=streets_arc$osm_lines, inherit.aes = FALSE, color = '#000000', size = .3, alpha = .6) +
  coord_sf(xlim = c(2.287, 2.303), ylim = c(48.867, 48.88), expand = FALSE) +
  theme_void() +
  labs(title = "Arc  de  Triomphe") +
  theme(
    plot.background = element_blank(),
    panel.background = element_rect(color='black', fill='#FDE7B3', linewidth = 10),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text  = element_blank(),
    plot.title = element_text(family = "Optimus Princeps", size = 18, colour = "black", face = "bold", hjust = 0.5, margin = margin(t=10,b=10)))

p2 <- ggplot() +
  geom_sf(data=buildings_query_magic$osm_polygons, inherit.aes = FALSE, color = '#850E35', size = .2, alpha = .6) +
  geom_sf(data=small_streets_magic$osm_lines, inherit.aes = FALSE, color = '#850E35', size = .2, alpha = .6) +
  geom_sf(data=streets_magic$osm_lines, inherit.aes = FALSE, color = '#000000', size = .3, alpha = .6) +
  coord_sf(xlim = c(2.287, 2.303), ylim = c(48.867, 48.88), expand = FALSE) +
  theme_void() +
  labs(title = "Swindon  Magic") +
  theme(
    plot.background = element_blank(),
    panel.background = element_rect(color='black', fill='#FDE7B3', linewidth = 10),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text  = element_blank(),
    plot.title = element_text(family = "Optimus Princeps", size = 18, colour = "black", face = "bold", hjust = 0.5, margin = margin(t=10,b=10)))

# Patch work
(p1 + plot_spacer() + plot_layout(nrow = 1, widths = c(1, 0.0001, 1)) + p2) +
  plot_annotation(
    title = "Roundabouts", caption = "Graphic: Manasseh Oduor | Source: {roundabouts} pkg | #TidyTuesday {wk:50}",
    theme = theme(
      plot.background = element_rect(fill = "#D6D6D6"),
      panel.background = element_rect(fill = "#D6D6D6"),
      plot.title = element_text(family = "Urbanist", colour = "black", face = "bold", size = 30, hjust = 0.5, margin = margin(t=5, b=5)),
      plot.caption = element_markdown(colour = 'black', hjust = 0.5, size = 11, family = 'Urbanist', margin = margin(t= 10, b = 10)),
      plot.margin = margin(b=2, t=20, r=10, l=10)))

ggsave("roundabouts.png", width = 10, height = 6, units = "in", dpi = 350)
