# load packages
pacman::p_load(tidyverse, ggtext, showtext, ggcharts, patchwork)

# Font
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add_google(name = "Ubuntu Condensed")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable= TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | Source: datacarpentry.org | #TidyTuesday {wk:24}")

# load data
safi_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-13/safi_data.csv')

# data wrangle
split_items_owned <- safi_data |>
  select(items_owned) |>
  separate(items_owned, into = paste0("item", 1:15), sep = ";", fill = "right") |>
  filter(item1 != "NULL")

split_items_owned_df <- split_items_owned |>
  pivot_longer(starts_with("item"), names_to = NULL, values_drop_na = TRUE)

items_owned_safi <- split_items_owned_df |> 
  select(value) |>
  mutate(Item = str_replace_all(value, "_", " "),
         Item = str_to_title(Item)) |>
  select(!value)

# plot 1
p1 = items_owned_safi |>
  summarize(count = n(), .by = Item) |>
  arrange(desc(count)) |>
  lollipop_chart(x = Item, y = count, threshold = 25) +
  labs(
    x = "Items owned by the household",
    y = " Frequency of Owned Items Mentioned",
    title = "More frequently"
  ) + theme(
    axis.text.x = element_text(family = "Ubuntu Condensed", size = 13),
    axis.text.y = element_text(family = "Ubuntu Condensed", size = 13),
    axis.title.x = element_text(family = "Ubuntu Condensed", size = 15),
    axis.title.y = element_text(family = "Ubuntu Condensed", size = 15),
    plot.background = element_rect(fill = "#bfebfe")
  )

# plot 2
p2 = items_owned_safi |>
  summarize(count = n(), .by = Item) |>
  arrange(desc(count)) |>
  slice_min(order_by = count, n = 7) |>
  lollipop_chart(x = Item, y = count) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 1), expand = expansion(mult = c(0, .05))) +
  labs(
    x = "",
    y = "",
    title = "Less frequently"
  ) + theme(
    axis.text.x = element_text(family = "Ubuntu Condensed", size = 13),
    axis.text.y = element_text(family = "Ubuntu Condensed", size = 13),
    plot.background = element_rect(fill = "#dbf3fe")
  )

# Patch work
(p1 + plot_spacer() + plot_layout(nrow = 1, widths = c(1, 0.02, 1)) + p2) +
  plot_annotation(
    title = "Studying African Farmer-Led Irrigation (SAFI)",
    subtitle = "Which Items are mentioned less/more frequently in households' ownership?",
    caption = cap,
    theme = theme(
      plot.title = element_text(family = "Roboto Condensed", colour = "black", face = "bold",
                                    size = 25, hjust = 0.5, margin = margin(t = 5, b = 5)),
      plot.subtitle = element_markdown(family = "Roboto Condensed", colour = "#a13a0b",
                                    size = 20, hjust = 0.5, margin = margin(t = 5, b = 5)),
      plot.caption = element_markdown(colour = 'blue', hjust = 0.5, size = 13,
                                      family = 'Rosario', margin = margin(t = 20)),
      plot.margin = margin(b = 10, t = 25, r = 20, l = 20)))

ggsave("safi_items.png", height=8, width=14, bg = "#F9F9F9")
