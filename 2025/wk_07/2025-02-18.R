# Load libraries
pacman::p_load(tidyverse, sf, paletteer, ggtext, showtext)

# Load Fonts
font_add_google(name = "Rosario")
font_add_google(name = "Averia Sans Libre")
font_add_google(name = "Averia Gruesa Libre")
font_add_google(name = "Averia Serif Libre")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load data
agencies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-18/agencies.csv')

# Data wrangling
agencies_df <- agencies |> 
  summarise(
    part_nibrs = sum(is_nibrs),
    not_part_nibrs = sum(!is_nibrs),
    part_nibrs_per = round(100 * part_nibrs / (part_nibrs + not_part_nibrs), 1), .by = c(state, state_abbr)) |> 
  arrange(desc(part_nibrs_per))

# US Hex grid
us_hex <- read_sf(here::here("us_states_hexgrid.geojson")) |>  
  mutate(state = str_remove(google_name, " \\(United States\\)")) |> 
  select(state)

# Merge
agencies_df1 <- us_hex |>  
  left_join(agencies_df, by = "state")

# Plot
ggplot(agencies_df1) +
  geom_sf(aes(fill = part_nibrs_per), linewidth = 3, color = "black", fill = NA) +
  geom_sf(aes(fill = part_nibrs_per), linewidth = 1, color = "#FFF7FC") +
  geom_richtext(aes(geometry = geometry, label = paste0("**", state_abbr, "**<br>**", part_nibrs_per, "**<br>(", part_nibrs, "/" , not_part_nibrs, ")"), 
                            color = state_abbr == "PA"), stat = "sf_coordinates", fill = NA, show.legend = FALSE, size = 4, family = "Averia Sans Libre", label.size = 0, lineheight = 1) +
  scale_fill_paletteer_c("viridis::plasma") +
  scale_color_manual(values = c("#212121", "#FFF7FC")) +
  coord_sf(crs = 3857) +
  theme_void() +
  labs(
    title = "NIBRS Participation Rates",
    subtitle = "Pennsylvania, Florida, and New York Trail in Agency Participation for the <br> FBI’s National Incident-Based Reporting System (NIBRS)",
    caption = "Graphic: Manasseh Oduor | Source: cde.ucr.cjis.gov \n #TidyTuesday {wk:7-2025}"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(family = "Averia Serif Libre", hjust = 0.5, size = 25, face = "bold", margin = margin(t=30,b=10)),
    plot.subtitle = element_markdown(family = "Averia Gruesa Libre", hjust = 0.5, size = 15, margin = margin(t=2,b=5)),
    plot.caption = element_text(size = 12, family = "Rosario", hjust = 0.5, face = "italic", color = "black", margin = margin(t=15, b=15)))

ggsave("agencies.png", height = 9, width = 12, bg = "#E8F9FF")