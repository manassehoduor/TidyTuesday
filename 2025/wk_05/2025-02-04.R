# Load libraries
pacman::p_load(tidyverse, tidytext, geomtextpath, ggtext, showtext)

# Load Fonts
font_add_google(name = "Rosario")
font_add_google(name = "Averia Sans Libre")
font_add_google(name = "Averia Gruesa Libre")
font_add_google(name = "Averia Serif Libre")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load data
simpsons_characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_characters.csv')
simpsons_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_episodes.csv')
simpsons_locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_locations.csv')
simpsons_script_lines <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_script_lines.csv')

# Data wrangling
food_words <- c("donut", "beer", "beer nuts", "bacon", "pizza", "burger", "chocolate", "ice cream", "hot dogs", "pork chops", "pancakes")

# Extract single word food
homer_food_single <- simpsons_script_lines |>
  filter(raw_character_text == "Homer Simpson") |>
  unnest_tokens(word, spoken_words, token = "words") |>  # Single words
  filter(word %in% food_words) |>
  count(word, sort = TRUE)

# Extract bigrams (2-word food)
homer_food_bigrams <- simpsons_script_lines |>
  filter(raw_character_text == "Homer Simpson") |>
  unnest_tokens(word, spoken_words, token = "ngrams", n = 2) |>  # Two-word phrases
  filter(word %in% food_words) |>
  count(word, sort = TRUE)

# Merge
homer_food <- bind_rows(homer_food_single, homer_food_bigrams) |>
  rename(food = word) |>
  arrange(desc(n))

food_cols <- c("beer" = "#F4A900", "pizza" = "#E6A04D", "chocolate" = "#4E2A1E", "donut" = "#D99A6C", "pancakes" = "#C69C6D", "bacon" = "#B33D26", "ice cream" = "#F8E8C1", "pork chops" = "#D27D56", "hot dogs" = "#D14124")

# Plot
ggplot(homer_food, aes(x = fct_reorder(food, n), y = n, fill = food)) +
  geom_col(width = 0.9, position = "dodge2", alpha = .9, colour = "#FFF078", size = 0.3) + 
  coord_curvedpolar(clip = "off") +
  geom_textpath(aes(label = n), text_only = TRUE, vjust = 1.5, size = 2.5) +
  scale_fill_manual(values = food_cols) +
  scale_y_continuous(limits = c(-3, 25), expand = c(0, 0), breaks = c(0, 5, 10, 15, 20, 25)) + 
  theme_minimal() +
  labs(title = "The Homeric Pie, D'oh-licious!!!",
       subtitle = "Top Food Mentions in Homer Simpson's Lines",
       fill = "Food") +
  theme(
    text = element_text(family = "Averia Gruesa Libre"),
    legend.position = "none",
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "gray12", size = 14, family = "Averia Sans Libre"),
    plot.margin = margin(r=30, l=30, t=10, b=20),
    plot.title = element_text(family = "Averia Serif Libre", size = 18, face = "bold", margin = margin(t=5, b=5), hjust = 0.5),
    plot.subtitle = element_text(family = "Averia Serif Libre", size = 12, hjust = 0.5, margin = margin(b=40))
  )
 
ggsave("simpsons.png", height = 8, width = 8, bg = "#E8F9FF")
