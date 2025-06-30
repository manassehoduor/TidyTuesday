# Load libraries
pacman::p_load(tidyverse, sf, rnaturalearth, rnaturalearthdata, viridis, ggtext, showtext, gganimate)

# Load Fonts
font_add_google(name = "Rosario")
font_add_google(name = "Roboto Condensed")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Data
cases_month <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_month.csv')
cases_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_year.csv')

# Data wrangling
# Join data sets
cases_month_year <- cases_month |> 
  left_join(
    cases_year |>
      select(iso3, year, annualized_population_most_recent_year_only), by = c("iso3", "year")) |> 
  mutate(measles_incidence_per_million = round((measles_total / annualized_population_most_recent_year_only) * 1e6, 2)) |> 
  select(region, country, iso3, year, month, measles_total, annualized_population_most_recent_year_only, measles_incidence_per_million) |> 
  filter(
    (region == "AFR" | iso3 %in% c("TUN", "MAR", "LBY", "EGY", "DJI", "SDN", "SOM")) &
      year %in% as.character(2012:2025)) |>
  mutate(
    year = as.integer(year),
    month = as.integer(month),
    month_name = factor(month.abb[month], levels = month.abb)
  )

# African map info
africa_map <- ne_countries(scale = "medium", returnclass = "sf", continent = "Africa") |>
  select(iso_a3, name, geometry)

# country-year-month grid
full_df <- expand.grid(
  iso3 = africa_map$iso_a3,
  year = unique(cases_month_year$year),
  month = 1:12) |> 
  left_join(cases_month_year, by = c("iso3", "year", "month")) |>
  mutate(month_name = factor(month.abb[month], levels = month.abb)) |>
  left_join(africa_map, by = c("iso3" = "iso_a3")) |>
  st_as_sf() |> 
  filter(iso3 != "-99")

# Plot
p <- ggplot(full_df) +
  geom_sf(aes(fill = measles_incidence_per_million), color = "black", size = 0.3) +
  facet_wrap(~ year, ncol = 4) +
  scale_fill_viridis(
    trans = "log10",
    breaks = c(1, 10, 100, 1000, 10000),
    labels = scales::label_number(),
    name = "Incidence Rate\n(per million, log10 scale)",
    option = "C",
    na.value = "grey90",
    guide = guide_colorbar(barwidth = 0.2, barheight = 5, title.position = "top")
  ) +
  labs(
    title = "Measles Outbreak in Africa",
    subtitle = "Month: {closest_state}",
    caption = "Graphic: Manasseh Oduor | Source: WHO Provisional Data (as of June 2025) \n #TidyTuesday {wk:25-2025}"
  ) +
  theme_void() +
  theme(
    strip.text = element_text(color = "#FFCFEF", size = 10, face = "bold", family = "Roboto Condensed"),
    plot.title = element_text(color = "#FEF9A7", face = "bold", family = "Rosario", hjust = 0.5, size = 15, margin = margin(t=20,b=10)),
    plot.subtitle = element_text(color = "#FEF9A7", face = "bold", family = "Rosario", hjust = 0.5, size = 12, margin = margin(b=5)),
    legend.position = "right",
    legend.text = element_text(color = "#dddddd", size = 9, family = "Roboto Condensed"),
    legend.title = element_text(color = "#dddddd", size = 10, family = "Roboto Condensed"),
    plot.caption = element_text(size = 10, family = "Rosario", hjust = 0.9, color = "#EBD6FB", margin = margin(t=10)),
    plot.margin = margin(l=40,b=10)
  ) +
  transition_states(month_name, state_length = 2, transition_length = 1, wrap = FALSE) +
  ease_aes("cubic-in-out")

# Animate
animate(p, width = 2700, height = 2700, nframes = 36, fps = 3, duration = 12, end_pause = 6, res = 300,
        bg = "#222831", renderer = gifski_renderer("measles_animation.gif"))
