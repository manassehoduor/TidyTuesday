
# load libraries
pacman::p_load(tidyverse, janitor, lubridate, showtext, ggtext, hrbrthemes, patchwork)

# Import fonts
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | #TidyTuesday wk:21 | Source: data.cityofnewyork.us")

# load data
squirrel_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv')

# data wrangle
squirrel_df <- squirrel_data |>
  select(Date,Shift,Age,`Primary Fur Color`,Location,Approaches,`Runs from`) |>
  clean_names() |>
  drop_na() |>
  mutate(date = as.Date(as.character(date), format = "%m%d%Y"),
         day = day(date),
         month = month(date),
         year = year(date),
         day_of_week = weekdays(date))

squirrel_df1 <- squirrel_data |>
  select(Date,Shift) |>
  clean_names() |>
  mutate(date = as.Date(as.character(date), format = "%m%d%Y"),
         day = day(date),
         day_of_week = weekdays(date))

# Define order of day of week
dow_order <- c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday")


squirrel_df2 <- squirrel_data |>
  select(Date) |>
  clean_names() |>
  mutate(date = as.Date(as.character(date), format = "%m%d%Y"),
         day = day(date))
  
# plot 1
p1 <- squirrel_df1 |>
    summarise(count = n(), .by = c(shift, day_of_week)) |>
    mutate(day_of_week = factor(day_of_week, levels = dow_order)) |>
  ggplot() +
  geom_tile(aes(shift, day_of_week, fill = count), width = 0.9, height = 0.2, colour = 'black') +
  scale_fill_gradient(low = "#fafde9", high = '#9b8f0d') +
  labs(
    fill = "Squirrel Census",
    x = "",
    y = "") +
  theme_modern_rc() +
  theme(
    text = element_text(),
    axis.title.x  = element_text(face = "bold", hjust = 0.5, size = 25),
    axis.text.x = element_text(face = "bold", size = 23),
    axis.text.y = element_text(face = "bold", size = 23),
    strip.text = element_text(face = "bold", size = 18),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "top",
    legend.box.background = element_blank(),
    legend.text = element_text(size = 22),
    legend.title.align = 0.5,
    legend.title = element_text(face = "bold", size = 28)) +
  guides(fill = guide_colourbar(direction = 'horizontal', ticks.linewidth=2,
                                barwidth = 20, barheight = 0.8, title.position = "top"))

# plot 2
p2 <- squirrel_df2 |>
  summarise(count = n(), .by = day) |>
  arrange(day) |>
  mutate(day = case_when(
    day %in% c(1, 21, 31) ~ paste0(day, "st"),
    day %in% c(2, 22) ~ paste0(day, "nd"),
    day %in% c(3, 23) ~ paste0(day, "rd"),
    TRUE ~ paste0(day, "th")
  )) |>
  ggplot(aes(x = factor(fct_inorder(day)), y = count)) +
  geom_bar(stat = "identity", width = 0.2, colour = 'black') + 
  labs(
    x = "Census Day",
    y = "Squirrel Sighting Count") +
  theme_modern_rc() +
  theme(
    text = element_text(),
    axis.title.x  = element_text(hjust = 0.5, size = 25, colour = "#e0ff55"),
    axis.title.y  = element_text(hjust = 0.5, size = 25, colour = "#e0ff55"),
    axis.text.x = element_text(face = "bold", size = 23),
    axis.text.y = element_text(face = "bold", size = 23),
    strip.text = element_text(face = "bold", size = 18),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())

# Patching
(p1 / p2) +
  plot_annotation(
    title = "Central Park Squirrel Census",
    subtitle = "The majority of squirrel sightings were observed during the late afternoon on Saturdays<br>*Tuesday had no census activity",
    caption = cap,
    theme = theme(
      plot.title = element_markdown(family = "Roboto Condensed", colour = "white", face = "bold",
                                    size = 40, hjust = 0.5, margin = margin(t = 5, b = 5)),
      plot.subtitle = element_markdown(family = "Roboto Condensed", colour = "#d9ceff",
                                       size = 30, hjust = 0, margin = margin(t = 5, b = 5)),
      plot.background = element_rect(fill = "#1d1616", color = NA),
      panel.background = element_rect(fill = "#1d1616", color = NA),
      plot.caption = element_markdown(colour = 'white', hjust = 0.5, size = 20,
                                      family = 'Rosario', margin = margin(t = 20, b = 10))))

# save plot
ggsave("Squirrel3.png", height=20, width=16, bg = "#1d1616")
