rm(list = ls())

# libraries
pacman::p_load(ggimage, magick, tidyverse, ggplot2, extrafont, ggtext, showtext, patchwork)

# Import fonts
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# get image 
egg <- magick::image_read("https://freesvg.org/img/carlitos_Egg.png") |>
  magick::image_trim()

egg_img <- magick::image_write(egg, path = "egg.img", format = "png")
image <- egg_img

# load data
eggproduction <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/egg-production.csv')

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'>.</span>",
               "<span style='font-family:sans;'> Manasseh Oduor </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | #TidyTuesday: wk 15 | Source: Samara Mendez | Data coverage: July 2016 : Feb 2021")
# data wrangle
eggprod <- eggproduction |>
  select(observed_month, prod_type, n_eggs) |>
  mutate(year = lubridate::year(observed_month),
         month = lubridate::month(observed_month),
         day = lubridate::day(observed_month),
         tray = n_eggs/30)

eggprod_month_year <- eggprod |>
  select(prod_type, tray, month, year) |>
  mutate(month = factor(month.name[month], levels = month.name)) |>
  group_by(month, year, prod_type) |>
  summarise(av_tray = round(median(tray)/1000000,0), .groups = "drop")

eggprod_month_year_table_eggs <- eggprod_month_year |>
  filter(prod_type == 'table eggs')


# Plot
eggprod_month_year_table_eggs |> 
  ggplot(aes(y = month, x = year)) +
  geom_image(aes(image = image), size = 0.08, by = "height", asp = 1.0, hjust = 0.5) +
  geom_point(size = 12, color="#a16207", data = eggprod_month_year_table_eggs |> filter(av_tray >= 100)) +
  geom_point(size = 10, color="#ca8a04", data = eggprod_month_year_table_eggs |> filter(av_tray >= 50)) +  
  geom_point(size = 8, color="#eab308", data = eggprod_month_year_table_eggs |> filter(av_tray >= 40)) + 
  geom_point(size = 6, color="#facc15", data = eggprod_month_year_table_eggs |> filter(av_tray >= 30)) +
  geom_point(size = 4, color="#fde047", data = eggprod_month_year_table_eggs |> filter(av_tray >= 20)) +
  geom_point(size = 2, color="#fef08a", data = eggprod_month_year_table_eggs |> filter(av_tray < 20)) +
  geom_text(aes(label = paste0(av_tray, " m")), color = "#0a0a0a", size = 2.5, data = eggprod_month_year_table_eggs) + 
  coord_equal(ratio = 0.5) +
  scale_y_discrete(limits = rev(month.name)) +
  labs(x = "Year",
       y = "Month",
       caption = cap,
       title = '<br>Production of Eggs in the US',
       subtitle = "The demand for table eggs has remained consistent over the years.<br>In July 2016, the production of table eggs peaked with a median of 245 million trays produced") +
  theme(
    text = element_text(family = "Roboto Condensed"),
    plot.title = element_markdown(margin = margin(b = 8), 
                              color = "#fefce8",face = "bold",size = 30,
                              hjust = 0.5,
                              family = "Roboto Condensed"),
    plot.subtitle = element_markdown(family = "Roboto Condensed", colour = "#fcd34d",
                                     size = 16, hjust = 0.1),
    plot.caption = element_textbox_simple(
      lineheight = 1, colour = "black", size = 14, hjust = 0.5, margin = margin(t=30,b=5), 
      family = "Rosario"),
    axis.title.x = element_text(family = "Roboto Condensed", size = 15, vjust = -3, face = "bold"),
    axis.title.y = element_text(family = "Roboto Condensed", size = 15, face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(family = "Roboto Condensed", size = 14, color = "#fefce8"),
    axis.text.y = element_text(family = "Roboto Condensed", size = 14, color = "#fefce8"),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    strip.text.x = element_blank(),
    plot.background = element_rect(fill = "black", color = NA),
    axis.ticks = element_blank()
  )

ggsave("Eggz.png", height=12, width=12, bg = "black")

