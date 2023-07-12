
# load libraries
pacman::p_load(tidyverse, gghighlight, MetBrewer, ggtext, showtext)

# Font
font_add_google(name = "Roboto Condensed")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | Source: data.giss.nasa.gov | #TidyTuesday {wk:28}")

# load data
global_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/global_temps.csv')
nh_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/nh_temps.csv')
sh_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/sh_temps.csv')
zonann_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/zonann_temps.csv')

# data wrangle
global_temps_df <- global_temps |>
  select(Year, all_of(month.abb))

nh_temps_df <- nh_temps |>
  select(Year, all_of(month.abb))

sh_temps_df <- sh_temps |>
  select(Year, all_of(month.abb))

global_temps_df_long <- global_temps_df |>
  pivot_longer(cols = starts_with(month.abb), names_to = "Month", values_to = "meanDev_Temp")

nh_temps_df_long <- nh_temps_df |>
  pivot_longer(cols = starts_with(month.abb), names_to = "Month", values_to = "meanDev_Temp")

sh_temps_df_long <- sh_temps_df |>
  pivot_longer(cols = starts_with(month.abb), names_to = "Month", values_to = "meanDev_Temp")

# merge
merged_df <- bind_rows(
  mutate(global_temps_df_long, surface = "Global"),
  mutate(nh_temps_df_long, surface = "Northern hemisphere"),
  mutate(sh_temps_df_long, surface = "Southern hemisphere")
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d")) |>
  drop_na()
  )

# plot
merged_df |>
  ggplot() +
  geom_hline(yintercept = 0, linetype = "solid", size = 0.2) +
  geom_point(data = merged_df |>
               group_by(surface) |>
               slice_max(Date),
             aes(x = Date, y = meanDev_Temp, color = surface), shape = 16) +
  geom_line(aes(x = Date, y = meanDev_Temp, color = surface)) +
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(colour = alpha("#d4d4d4", 1))) +
  geom_text(data = merged_df |>
              group_by(surface) |>
              slice_max(Date),
            aes(x = Date, y = meanDev_Temp, color = surface, label = meanDev_Temp),
            hjust = -0.2, vjust = 0.2, size = 6, family = "Roboto Condensed", fontface = "bold") +
  scale_color_met_d(name = "Redon") +
  scale_x_date(date_labels = "%Y") +
  scale_y_continuous(breaks = c(-1.52, 0, 1.94),
                     labels = c("", "0", "")) +
  facet_wrap(~ factor(surface, levels = c('Global', 'Northern hemisphere', 'Southern hemisphere')), ncol = 1) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.title = element_text(face = "bold"),
    axis.title = element_blank(),
    axis.text = element_text(color = "#282424", size = 15, family = "Roboto Condensed"),
    strip.text.x = element_text(face = "bold", family = "Roboto Condensed", size = 25, colour = "#de001b"),
    plot.title = element_markdown(hjust = 0.5, size = 40, color = "#282424", family = "Roboto Condensed",
                                  lineheight = 0.8, face = "bold", margin = margin(b=20, t=20, r=30, l=0)),
    plot.subtitle = element_markdown(hjust = 0.5, size = 25, color = "#282424", family = "Rosario",
                                     lineheight = 1, margin = margin(b=20, t=5, r=0, l=0)),
    plot.caption = element_markdown(hjust = 0.5, margin = margin(t =20, b=20), family = "Rosario",
                                    size = 18, color = "#282424", lineheight = 1.2),
    plot.caption.position = "plot",
    plot.background = element_rect(color = "#f6f5f5", fill = "#f6f5f5"),
    plot.margin = margin(b = 10, t = 20, r = 20, l = 20)
  ) +
  labs(
    title = "Global Surface Temperatures: 1880-2023",
    subtitle = "Global and Hemispheric monthly mean deviations from the corresponding 1951-1980 means",
    caption = cap
  )

ggsave("Global surface temperatures.png", height = 15, width = 18)
