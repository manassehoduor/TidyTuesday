# Load libraries
pacman::p_load(countrycode, tidyverse, scales, ggimage, ggtext, showtext, hrbrthemes, AfricaCountryBins, gganimate)

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Neucha")

# load data
tuesdata <- tidytuesdayR::tt_load(2022, week = 29)
technology <- tuesdata$technology

# target vaccines
target = c("BCG", "DPT", "MCV1", "Pol3", "pctimmunizmeas")

# Wrangle data
tech_africa <- technology |>
  mutate(country = countrycode(iso3c, 
                               origin = "iso3c", 
                               destination = "country.name"),
         continent = countrycode(iso3c, origin = "iso3c",
                                 destination = "continent")) |>
  filter(continent == "Africa",
         category == "Vaccines",
         variable %in% target) |>
  mutate(variable = str_replace_all(variable, c("BCG" = "BCG Vaccine",
                                                "DPT" = "Diphtheria-Pertussis-Tetanus (DPT) Vaccine",
                                                "MCV1" = "1st dose of Measles-Containing Vaccine",
                                                "Pol3" = "Polio Vaccine",
                                                "pctimmunizmeas" = "Measles Vaccine")))

# All five vaccines
tech_africa_mean <- tech_africa |>
  group_by(iso3c, variable) |>
  summarise(mean_pct = mean(value), .groups = "drop")

# Plot
tech_africa_mean |>
  ggplot(aes(country = iso3c, fill = mean_pct)) +
  geom_countrybins_africa(
    radius = unit(0, "pt"),
    family = "Neucha", size = 0.5
  ) +
  scale_fill_viridis_c(
    name = "Mean (%) of Children\n Receiving the Vaccine",
    option = "inferno",
    direction = -1,
    na.value = "#DEE5E8",
    label = scales::percent_format(scale = 1)
  ) +
  coord_equal() +
  facet_wrap(~variable) +
  guides(
    fill = guide_colourbar(
      title.position = "top"
    )
  ) +
  labs(
    title = "Vaccines Consumption Coverage in Africa: 1980-2019",
    subtitle = "BCG vaccine Immunization Coverage leads Diphtheria-Pertussis-Tetanus, 1st dose of Measles-Containing vaccine, Polio, & Measles Vaccines\nAll the five vaccines immunization Coverage are dominant in most Countries from Eastern & Southern Africa compared to Northern, Western, and Central Africa",
    caption = "Graphic: Manasseh O. | #TidyTuesday | Source: <data.nber.org>"
  ) +
  theme_ipsum(grid="TRUE", strip_text_face = "bold",
              strip_text_size = 14.5,
              base_family = "Neucha",
              grid_col = "#FCE2DB",
              plot_title_face = "bold") +
  theme(legend.position = c(0.9, 0.1),
  legend.direction = "horizontal",
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  plot.title = element_text(family = "Neucha", 
                            face = "bold",
                            size = 26,
                            hjust = 0.5),
  plot.subtitle = element_text(size = 13, hjust = 0.1,
                               family = "Neucha", lineheight = 1.1),
  plot.caption = element_text(family =  "Neucha", size = 12))

ggsave("tech.png", height=10, width=12, bg = "#F9F9F9")


# GIF
Africa <- tech_africa |>
  ggplot(aes(country = iso3c, fill = value)) +
  geom_countrybins_africa(
    radius = unit(0, "pt"),
    family = "Neucha", size = 0.5
  ) +
  scale_fill_viridis_c(
    option = "inferno",
    direction = -1,
    na.value = "#DEE5E8",
    label = scales::percent_format(scale = 1)
  ) +
  coord_equal() +
  facet_wrap(~variable) +
  guides(
    fill = guide_colourbar(
      title.position = "top", barwidth = unit(20, 'lines'), barheight = unit(2.5, 'lines')
    )
  ) +
  labs(
    title = "Vaccines Consumption Coverage in Africa: 1980-2019",
    subtitle = "Vaccine Immunization Coverage has tremendeously improved over the years \n\n However the coverage is more dominant in Countries from Eastern & Southern Africa compared to Northern, Western, and Central Africa \n\n year: {closest_state}",
    caption = "Graphic: Manasseh O. | #TidyTuesday | Source: <data.nber.org>",
    fill = "% of Children Receiving the Vaccine"
  ) +
  theme_ipsum(grid="TRUE", strip_text_face = "bold",
              strip_text_size = 13,
              base_family = "Neucha",
              grid_col = "#FCE2DB",
              plot_title_face = "bold") +
  theme(legend.position = c(0.8, 0.1),
        legend.direction = "horizontal",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(family = "Neucha", 
                                  face = "bold",
                                  size = 20,
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 14, vjust = 0.5, hjust = 0.5,
                                     family = "Neucha", lineheight = 1),
        plot.caption = element_text(family =  "Neucha", size = 12))

anim <- Africa +
    transition_states(year, transition_length = 3,
                      state_length = 1)

animate(anim, height = 18, width = 22, units = "in", res = 150, end_pause = 20,
        nframes = 300, fps = 50, device = "png")

anim_save("tech.gif", animation = last_animation())



