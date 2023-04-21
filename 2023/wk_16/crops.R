rm(list = ls())

# load libraries
pacman::p_load(tidyverse, viridis, hrbrthemes, extrafont, ggtext, showtext)

# Import fonts
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'>.</span>",
               "<span style='font-family:sans;'> Manasseh Oduor </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | #TidyTuesday: wk 16 | Source: Neolithic Founder Crops")

# load data
founder_crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-18/founder_crops.csv')

# data wrangling
founder_crops_category <- founder_crops |>
  select(prop, category) |>
  summarise(n = sum(prop),.by = category) |>
  drop_na() |>
  mutate(percent = round(n/sum(n)*100,1)) |>
  arrange(desc(percent))

# plot preps
# Compute the cumulative percentages (top of each rectangle)
founder_crops_category$ymax <- cumsum(founder_crops_category$percent)

# Compute the bottom of each rectangle
founder_crops_category$ymin <- c(0, head(founder_crops_category$ymax, n=-1))

# Compute label position
founder_crops_category$labelPosition <- (founder_crops_category$ymax + founder_crops_category$ymin) / 2

# Add label
founder_crops_category$label <- paste0(founder_crops_category$category,"\n",
                                       founder_crops_category$percent, "%")

# Donut chart
founder_crops_category |>
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_viridis(discrete = T) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_ft_rc() +
  theme(
    plot.title = element_markdown(colour = "#eaebf4",
                                     size = 20, hjust = 0.5),
    plot.subtitle = element_markdown(colour = "#eaebf4",
                                     size = 11, hjust = 0.1),
    plot.caption = element_textbox_simple(
      lineheight = 1, colour = "#f49d0c", size = 8, hjust = 0.5, margin = margin(t=1,b=5), 
      family = "Rosario"),
    legend.position = "none") +
  labs(
    x = "",
    y = "",
    caption = cap,
    title = 'Neolithic Founder Crops',
    subtitle = 'Grass was the domiant crop of early Neolithic agriculture in Southwest Asia comprising of 56.7%.<br>The major Grass Genera were Triticum (29.6%), Hordeum (18%), and Lolium (4.1%)') +
  annotate("text", x = 3.5, y = 8, 
           label = "GENUS:\n Triticum (29.6%)\n Hordeum (18%)\n Lolium (4.1%)", size = 2.7, 
           hjust = 0.5, vjust = 0.5, color = "#eaebf4", fontface = "bold", family = "Rosario")

ggsave("crops.png", height=7, width=8)


