
# load libraries
pacman::p_load(tidyverse, maps, ggtext, showtext, ggthemes)

# Font
font_add_google(name = "Roboto Condensed")
font_add_google(name = "Rye")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | Source: hmdb.org | #TidyTuesday {wk:27}")

# load data
historical_markers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/historical_markers.csv')
no_markers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/no_markers.csv')

# data wrangle
# Filter New York Historical Markers
HM_NY <- historical_markers |>
  filter(state_or_prov == "New York")

# New York Map Data
NY_map <- map_data("county", region = "new york")

# Set a dark color palette
dark_palette <- c("#183d31", "#215d48", "#29896c", "#4ca98e", "#78c0a6", "#fef2f2")

# Plot
ggplot() +
  geom_polygon(data = NY_map, aes(x = long, y = lat, group = group),
               color = "#facc15", fill = NA, size = 0.7) +
  geom_polygon(data = NY_map, aes(x = long, y = lat, group = group),
               color = dark_palette[5], fill = dark_palette[1], size = 0.2) +
  geom_point(data = HM_NY, aes(x = longitude_minus_w, y = latitude_minus_s), 
             color = "#fce7f3", size = 0.05) +
  theme_map() +
  labs(
    title = "New York",
    subtitle = "Historical Markers \n by Geographic Location",
    caption = cap
  ) +
  theme(
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.72, colour = "#4f46e5", vjust = -6,
                              family = "Rye", size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.3, vjust = -60, colour = "#172554",
                              family = "Roboto Condensed", size = 10, face = "bold"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.caption = element_markdown(colour = '#064e3b', hjust = 0.9, size = 7,
                                      family = 'Rosario', margin = margin(t=5, b=10)),
    plot.margin = margin(b = 10, t = 20, r = 20, l = 20)
)

ggsave("historical markers.png", height=7, width=9, bg = "#fef2f2")
