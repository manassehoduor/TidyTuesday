# Load libraries
pacman::p_load(tidyverse, rnaturalearth, rnaturalearthdata, sf, ggthemes, packcircles, ggflags, ggtext, showtext)

# Font
font_add_google(name = "Roboto Condensed")
font_add_google(name = "Dangrek")
font_add_google(name = "Rosario")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load data
tuesdata <- tidytuesdayR::tt_load(2024, week = 46)

countries <- tuesdata$countries
country_subdivisions <- tuesdata$country_subdivisions
former_countries <- tuesdata$former_countries

# Data Wrangling
# Count subdivisions
country_subdivisions <- country_subdivisions |> 
  left_join(select(countries, alpha_2, name), by = "alpha_2")

county_n <- country_subdivisions |> 
  filter(type == "County") |> 
  count(name.y, alpha_2, type, name = "country_freq", sort = TRUE)

# Assign colors  
country_colors <- county_n |>
  mutate(color = scales::hue_pal()(n())) |>
  select(alpha_2, color)

# Load country geometries
countries_geo <- ne_countries(scale = "medium", returnclass = "sf")

# Subset countries with counties
countries_with_counties <- county_n |> 
  filter(alpha_2 %in% countries_geo$iso_a2)

# Join the subset countries with their geometries
countries_with_counties_geo <- countries_geo |>
  filter(iso_a2 %in% countries_with_counties$alpha_2)

# Merge counts with geometry data
Geo_df <- countries_with_counties_geo |>
  left_join(countries_with_counties, by = c("iso_a2" = "alpha_2")) |>
  mutate(
    x_start = st_coordinates(st_centroid(geometry))[,1],
    y_start = st_coordinates(st_centroid(geometry))[,2],
    x_end = case_when(
      iso_a2 == "KE" ~ NA_real_,
      TRUE ~ x_start
    ),
    y_end = case_when(
      iso_a2 == "KE" ~ NA_real_,
      iso_a2 == "LR" ~ y_start - 5,
      TRUE ~ y_start + 12
    ),
    label_x = case_when(
      iso_a2 == "KE" ~ x_start,
      TRUE ~ x_start
    ),
    label_y = case_when(
      iso_a2 == "KE" ~ y_start,
      iso_a2 == "LR" ~ y_start - 6,
      TRUE ~ y_start + 13
    )
  )

# Plot 1: Map
ggplot() +
  # Base map
  geom_sf(data = countries_geo, fill = "gray80", color = "white") +
  geom_sf(data = countries_with_counties_geo, fill = "#3D3BF3", color = "white") +
  geom_segment(
    data = Geo_df |> filter(!is.na(x_end)),
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end), color = "#A64D79", linewidth = 0.2) +
  geom_point(
    data = Geo_df |> filter(!is.na(x_end)),
    aes(x = x_end, y = y_end), shape = 21, size = 1, color = "red", fill = "white") +
  geom_text(
    data = Geo_df,
    aes(x = label_x, y = label_y, label = country_freq), size = 2, family = "Roboto Condensed", hjust = 0.5,
    vjust = case_when(Geo_df$iso_a2 == "LR" ~ 1, TRUE ~ 0)) +
  labs(
    title = "ISO Country Codes",
    subtitle = "Countries with County as a Subdivision",
    caption = "Graphic: Manasseh Oduor | Source: ISOcodes R package \n #TidyTuesday {wk:46-2024} "
  ) +
  #coord_sf(crs = "+proj=moll") +
  theme_map() +
  theme(
    text = element_text(family = "Roboto Condensed", colour = "#C9E6F0"),
    plot.title = element_text(family = "Dangrek", colour = "#432E54", face = "bold", size = 18, hjust = 0.5, margin = margin(t = 10, b = 10)),
    plot.subtitle = element_text(family = "Dangrek", colour = "#432E54", face = "bold", size = 13, hjust = 0, margin = margin(t = 10, b = 10)),
    plot.margin = margin(b=5, t=5, r=5, l=5),
    plot.caption = element_text(family = "Rosario", color = "#8B5DFF", size = 8, hjust = 0.95))

# Save plot
ggsave("ISO.png", width = 14, height = 7, bg = "#FBF8EF")

# Plot 2: Circle Packing
# Data Wrangling
# Generate circle packing layout
packing <- circleProgressiveLayout(county_n$country_freq, sizetype = "area")
packing_df <- cbind(county_n, packing)

# Generate circle vertices for plotting
circle_vertices <- circleLayoutVertices(packing, npoints = 50)

# Adjust Country name dynamically
packing_df$text_size <- ifelse(nchar(packing_df$name.y) > 15, 4, 6)

# Plot
ggplot() +
  geom_polygon(data = circle_vertices, aes(x, y, group = id), colour = "black", fill = "#C5D3E8", alpha = 0.6, size = 1) +
  geom_flag(data = packing_df, aes(x = x, y = y), country = tolower(packing_df$alpha_2), size = 50) +
  geom_text(data = packing_df, aes(x = x, y = y - 0.1, label = name.y, size = text_size), 
            vjust = 0, fontface = "bold", family = "Roboto Condensed", color = "#2E073F") +
  geom_text(data = packing_df, aes(x = x, y = y - 1.4, label = country_freq), size = 5, fontface = "bold",
            color = "darkblue", vjust = 0, family = "Roboto Condensed") +
  scale_size_identity() +
  coord_equal() +
  theme_void() +
  theme(
    text = element_text(family = "Roboto Condensed", colour = "#C9E6F0")
  )

# Save plot
ggsave("ISO2.png", width = 12, height = 10, bg = "#FBF8EF")