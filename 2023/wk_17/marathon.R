
# load libraries
pacman::p_load(tidyverse, showtext, ggtext, ggsvg, glue)


# Import fonts
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'>.</span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | #TidyTuesday wk:17")

# Extract 'run' SVG
run_url <- "https://www.svgrepo.com/download/431236/run.svg"
svg_txt <- paste(readLines(run_url), collapse = "\n")
grid::grid.draw( svg_to_rasterGrob(svg_txt))

# load data
winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/winners.csv')
london_marathon <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/london_marathon.csv')

# data wrangle
winners_df <- winners|>
  filter(Category=='Men' & Year >= 2003) |>
  select(!Category) |>
  mutate(Nationality = toupper(Nationality))

# Plot
winners_df |> 
  ggplot(aes(x = Time, y = Year)) +
  geom_point_svg(aes(Time), svg = svg_txt, size = 20) +
  geom_text(aes(Time, label = glue("{Time} \n {Nationality} \n {Athlete}")),  
            family = 'Roboto Condensed', size = 4.5, 
            colour = "black", lineheight = 0.8, nudge_x = 25) +
  scale_y_continuous(breaks = c(2003:2022), expand = c(0.05, 0.05)) +
  scale_x_reverse() +
  labs(
    title = "LONDON MARATHON: A TWO-DECADE- RIVALRY",
    subtitle = "Kenya and Ethiopia Dominance in the London Marathon",
    caption = cap
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = 'Roboto Condensed'),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 25, face = "bold"),
    axis.title = element_text(size = 30, face = "bold"),
    plot.title = element_markdown(face = "bold", hjust = 0.5, size = 45, colour = '#291a98'),
    plot.subtitle = element_markdown(hjust = 0.5, size = 28, color = "#2f9c14"),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid = element_blank(),
    plot.caption = element_markdown(colour = '#291a98', hjust = 0.5, size = 20,
                                    family = 'Rosario', margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50)
  )


# Save plot
ggsave("marathon.png", width = 20, height = 15, bg = "white")


