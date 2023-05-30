
rm(list = ls())

# load libraries
pacman::p_load(tidyverse, hrbrthemes, ggbeeswarm, viridis, extrafont, ggtext, 
               showtext, htmltools)

# Font
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add_google(name = "Ubuntu Condensed")
font_add_google(name = "Cabin Sketch")
font_add_google(name = "Rye")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | Source: Wikipedia by frankiethull | #TidyTuesday {wk:22}")

#load data
centenarians <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv')

# data wrangle
centenarians <- centenarians |>
  select(name, place_of_death_or_residence, gender, still_alive, age)

# plot
ggplot(centenarians, aes(x = gender, y = age, color = still_alive, fill = gender)) +
  geom_beeswarm(cex = 3, size = 3) +
  coord_flip() +
  scale_y_continuous(limits = c(110, 124),
                     breaks = seq(110,124,1),
                     labels = seq(110, 124, 1)) +
  scale_color_manual(values = c("#4180f0", "#f5e538"), guide = "none") +
  theme_modern_rc() +
  labs (
    fill = "Alive Status",
    y = "Centenarians Age",
    x = "",
    caption = cap,
    subtitle = "Centenarians Age Distribution by Gender",
    title = "Verified Oldest People"
  ) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(family = "Ubuntu Condensed", 
                                   colour = "white", size = 11, face = "bold"),
        axis.text.y = element_text(family = "Ubuntu Condensed", 
                                   colour = "white", size = 25, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.subtitle = element_text(family = "Cabin Sketch", 
                                     colour = "#fcfcea", size = 18, face = "bold",
                                     margin = margin(t = 3, b = 2, unit = "mm"), hjust = 0.8
        ),
        plot.title = element_text(
          face = "bold", colour = "white", family = "Rye", size = 25, hjust = 0.5
        ),
        plot.caption = element_markdown(colour = '#f5f29e', hjust = 0.5, size = 11,
                                        family = 'Rosario', margin = margin(t=10, b=10)),
        plot.margin = margin(b = 10, t = 20, r = 5, l = 20)) +
  annotate(
    geom = "curve", x = "female", xend = "female", y = 122.6, yend = 123,
    curvature = -.3, arrow = arrow(length = unit(2, "mm")),
    color = "white", size = 0.5) +
  annotate(
    geom = "text", x = "female", y = 123.03, label = "Jeanne Calment (France) \n Deceased",
    hjust = "left", color = "white", size = 3, family = "Roboto Condensed") +
  annotate(
    geom = "text", x = "female", y = 116.35, vjust = 6,  label = "Maria Branyas (Spain) - Alive",
    hjust = "left", color = "white", size = 3, family = "Roboto Condensed")

# Save plot
ggsave("centenarians.png", width = 12, height = 8)


