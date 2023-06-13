
# load packages
pacman::p_load(tidyverse, ggwordcloud, ggtext, showtext)

# Font
font_add_google(name = "Ubuntu Condensed")
font_add_google(name = "Roboto Condensed")
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
  select(memb_assoc, items_owned) |>
  separate(items_owned, into = paste0("item", 1:15), sep = ";", fill = "right") |>
  filter(item1 != "NULL" & memb_assoc != "NULL")

split_items_owned_df <- split_items_owned |>
  pivot_longer(starts_with("item"), names_to = NULL, values_drop_na = TRUE)

items_owned_safi <- split_items_owned_df |> 
  select(memb_assoc, value) |>
  mutate(Item = str_replace_all(value, "_", " "),
         Item = str_to_title(Item)) |>
  select(!value)

# frequency
item_freq <- items_owned_safi |>
  mutate(memb_assoc = ifelse(memb_assoc == "no", "Not a member of an irrigation association", 
                             "A member of an irrigation association")) |>
  summarize(count = n(), .by = c(memb_assoc, Item)) |>
  arrange(desc(count))

# most frequent items
split_items_owned1 <- safi_data |>
  select(items_owned) |>
  separate(items_owned, into = paste0("item", 1:15), sep = ";", fill = "right") |>
  filter(item1 != "NULL")

split_items_owned_df1 <- split_items_owned1 |>
  pivot_longer(starts_with("item"), names_to = NULL, values_drop_na = TRUE)

items_owned_safi1 <- split_items_owned_df1 |> 
  select(value) |>
  mutate(Item = str_replace_all(value, "_", " "),
         Item = str_to_title(Item)) |>
  select(!value)

# frequency
item_freq1 <- items_owned_safi1 |>
  summarize(count = n(), .by = Item) |>
  arrange(desc(count))

# plot: word cloud
ggplot(item_freq, aes(label = Item, size = count, color = Item)) +
  geom_text_wordcloud_area(shape = "circle") +
  scale_size_area(max_size = 6) +
  theme_minimal() +
  ggtitle("How does membership in an irrigation association relate to the items owned by households?") +
  theme(
    plot.title = element_text(size = 8, family = "Ubuntu Condensed", hjust = 0.5),
    text = element_text(size = 5.5, family = "Roboto Condensed"),
    plot.caption = element_markdown(colour = 'blue', hjust = 0.5, size = 4,
                                    family = 'Rosario', margin = margin(t = 20))) +
  facet_wrap(~memb_assoc, ncol = 1, dir = "v") +
  labs(caption = cap)

ggsave("items_member.png", height=3, width=4, bg = "#F9F9F9")

