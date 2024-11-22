# Load libraries
pacman::p_load(tidyverse, ggtext, showtext)

# Font
font_add_google(name = "Roboto Condensed")
font_add_google(name = "Loved by the King")
font_add_google(name ="Faster One")
font_add_google(name = "Rosario")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load the dataset
episode_metrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-19/episode_metrics.csv')

# Data wrangling
episode_df <- episode_metrics |> 
  mutate(episode_id = paste0("S", season, "E", episode),
         episode = factor(episode, levels = unique(episode)),
         season = factor(season, levels = unique(season))) |>
  select(season, episode, avg_length)

max_avg_length <- max(episode_df$avg_length, na.rm = TRUE)
max_episode_df <- episode_df |>
  filter(avg_length == max_avg_length)

# Plot
ggplot(episode_df, aes(x = episode, y = season, color = avg_length)) +
  geom_text(aes(label = "\uf805", size = 10), family = "fa-solid", fontface = "plain") +
  scale_size_continuous(range = c(10, 10)) +
  geom_point(data = max_episode_df, aes(x = episode, y = season), shape = 1, color = "white", size = 18, stroke = 0.5) +
  scale_color_gradientn(
    colors = c("#D6A45E", "#FFB81C", "#D43F00", "#4F2C26"), # burger colors: Bun, Lettuce, Cheese, Tomato, Patty
    limits = c(21, 65),
    na.value = "grey",
    guide = guide_colorsteps(
      title = "Average Dialogue Length",
      title.position = "top",
      title.hjust = 0.5,
      barheight = unit(0.5, "cm"),
      barwidth = unit(5, "cm")
    ),
    breaks = seq(20, 65, by = 10)
  ) +
  labs(
    title = "Bob's Burgers Episodes",
    subtitle = "Season 5, Episode 1 of Bob's Burgers featured the longest and most engaging conversations",
    x = "Episode",
    y = "Season",
    caption = "Graphic: Manasseh Oduor \n Source: Steven Ponce via {bobsburgersR} R-package \n #TidyTuesday {wk:47-2024}",
    color = "Average Dialogue Length"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Roboto Condensed", colour = "#C9E6F0"),
    plot.title = element_text(family = "Faster One", colour = "#C9E6F0", face = "bold", size = 30, hjust = 0.5, margin = margin(t = 10, b = 5)),
    plot.subtitle = element_text(family = "Loved by the King", colour = "#FFE6A9", face = "bold", size = 15, hjust = 0, margin = margin(t = 5, b = 15)),
    plot.margin = margin(b = 5, t = 5, r = 10, l = 10),
    plot.caption = element_text(family = "Rosario", color = "#C9E6F0", size = 10, hjust = 0.95),
    axis.text.x = element_text(size = 11, angle = 0, hjust = 1, margin = margin(t = 10)),
    axis.text.y = element_text(size = 11, angle = 45, hjust = 1, margin = margin(r = 10)), 
    axis.title.x = element_text(size = 16, colour = "#C9E6F0", face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, angle = 90, colour = "#C9E6F0", face = "bold", margin = margin(r = 10)),
    legend.position = "top") +
  guides(size = "none")

# Save plot
ggsave("Burger.png", width = 12, height = 10, bg = "#1A1A1D")

