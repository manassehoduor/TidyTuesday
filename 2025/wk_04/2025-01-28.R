# Load libraries
pacman::p_load(tidyverse, janitor, ggforce, ggtext, showtext)

# Load Fonts
font_add_google(name = "Rosario")
font_add_google(name = "Averia Sans Libre")
font_add_google(name = "Averia Gruesa Libre")
font_add_google(name = "Averia Serif Libre")

showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# Load data
water_insecurity_2022 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2022.csv')
water_insecurity_2023 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2023.csv')

# Data wrangling
h2o_insec <- water_insecurity_2022 |> 
  inner_join(water_insecurity_2023, by = c("geoid", "name")) |> 
  clean_names() |> 
  select(name, percent_lacking_plumbing_x, percent_lacking_plumbing_y) |> 
  rename(percent_lacking_plumbing_2022 = percent_lacking_plumbing_x,
         percent_lacking_plumbing_2023 = percent_lacking_plumbing_y)

h2o_insec_df <- h2o_insec |> 
  mutate(
    diff = percent_lacking_plumbing_2022 - percent_lacking_plumbing_2023,
    change_type = case_when(
      percent_lacking_plumbing_2023 > percent_lacking_plumbing_2022 ~ "Worsening",
      percent_lacking_plumbing_2023 < percent_lacking_plumbing_2022 ~ "Improved",
      TRUE ~ "Stable")
    ) |> 
  filter(min_rank(desc(diff)) <= 10 | min_rank(diff) <= 10) |>
  arrange(desc(diff))

# Plot
h2o_insec_df |> 
  ggplot() +
  geom_link(aes(x = percent_lacking_plumbing_2022, y = fct_reorder(name, diff), xend = percent_lacking_plumbing_2023, yend = fct_reorder(name, diff), 
                colour = change_type, linewidth=after_stat(index))) +
  geom_point(aes(percent_lacking_plumbing_2023, y = fct_reorder(name, diff), colour = change_type), shape = 21, size = 5.3, fill = "#fff") +
  geom_text(data = h2o_insec_df |> 
              filter(name == "McKinley County, New Mexico"), aes(x = percent_lacking_plumbing_2022, y = name, label = "2022"), 
            vjust = -1, family = "Averia Gruesa Libre", size = 3, , colour = "#957777") +
  geom_text(data = h2o_insec_df |> 
              filter(name == "McKinley County, New Mexico"), aes(x = percent_lacking_plumbing_2023, y = name, label = "2023"), 
            vjust = 2.5, family = "Averia Gruesa Libre", size = 3, colour = "#694E4E") +
  geom_text(data = h2o_insec_df |> 
              filter(name == "Fairbanks North Star Borough, Alaska"), aes(x = percent_lacking_plumbing_2022, y = name, label = "2022"), 
            vjust = -1, family = "Averia Gruesa Libre", size = 3, colour = "#957777") +
  geom_text(data = h2o_insec_df |> 
              filter(name == "Fairbanks North Star Borough, Alaska"), aes(x = percent_lacking_plumbing_2023, y = name, label = "2023"), 
            vjust = 2.5, family = "Averia Gruesa Libre", size = 3, colour = "#694E4E") +
  annotate(geom = "label", x = 3.5, y = "Henderson County, Texas", label = "Improved Access to Plumbing", colour = "#3D3BF3", fill = "#fff",
           label.size = 0.05, size = 3.5, family = "Averia Sans Libre", angle = -90) +
  annotate(geom = "label", x = 3.5, y = "Anderson County, Tennessee", label = "Worsening Access to Plumbing", colour = "#E52020", fill = "#fff",
           label.size = 0.05, size = 3.5, family = "Averia Sans Libre", angle = -90) +
  scale_colour_manual(values = c("#3D3BF3", "#E52020")) +
  scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    title = "Households Lacking Plumbing Facilities in the U.S.",
    subtitle = "Top Counties with major YoY Changes in Plumbing Access",
    x = "% of Population lacking plumbing facilities",
    caption = "Graphic: Manasseh Oduor \n Source: tidycensus package | #TidyTuesday {wk:4-2025}"
  ) +
  theme_void() +
  theme(
    strip.text = element_text(family = "Averia Gruesa Libre"),
    legend.position = "none",
    axis.text.x = element_text(family = "Averia Sans Libre"),
    axis.text.y = element_text(family = "Rosario", size = 10, hjust = 1),
    axis.title.x = element_text(family = "Rosario", size = 11, face = "bold", margin = margin(t=10,b=10)),
    panel.grid.major.x = element_line(linetype = "dotted", colour = "grey"),
    plot.margin = margin(r=30, l=20, t=10, b=10),
    plot.title = element_text(family = "Averia Serif Libre", size = 13, face = "bold", margin = margin(t=5, b=5), hjust = 2),
    plot.subtitle = element_text(family = "Averia Serif Libre", size = 10, hjust = -40, margin = margin(b=10)),
    plot.caption = element_text(family = "Rosario", color = "#09122C", size = 9, hjust = 1, margin = margin(t=10, b=10))
  )

ggsave("water_insecurity.png", height = 8, width = 7, bg = "#FFEDFA")
