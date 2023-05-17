rm(list=ls())

# load libraries
pacman::p_load(tidyverse, statebins, ggtext, showtext, extrafont)


# Import fonts
font_add_google(name = "Raleway")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'>  Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | Source: spc.noaa.gov | #TidyTuesday wk:20")

# load data
tornados <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-16/tornados.csv')

# data wrangling
tornados_df <- tornados |>
  select(st, stf, inj, sn) |>
  filter(sn == 1) |>
  summarise(tot_injuries = sum(inj), .by = c(st, stf, sn)) |>
  select(!sn)

# clean state fips
state_fips <- data.frame(state_abbreviation = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
                         fips_code = c("01", "02", "04", "05", "06", "08", "09", "10", "12", "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56"))


tornados_df <- merge(tornados_df, state_fips, by.x = "st", by.y = "state_abbreviation", all.x = TRUE)

tornados_df <- tornados_df |>
  drop_na() |>
  slice(c(-14,-44))

# Mapping
tornados_df |>
  statebins(value_col = 'tot_injuries', state_col = "st", font_size = 3,
            palette = "OrRd", direction = 1) +
  theme_statebins() +
  theme(text = element_text(family = "Raleway"),
        plot.title = element_markdown(colour = "black",face = "bold",
                                      size = 15, hjust = 0.5, family = "Raleway"),
        plot.subtitle = element_markdown(colour = "black",size = 12,
                                         hjust = 0, family = "Raleway"),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_text(face = "bold", size=12),
        legend.position = "top",
        legend.title.align = 0.5,
        legend.box.background = element_blank(),
        legend.title = element_text(family = "Raleway", face = "bold", size = 10),
        plot.caption = element_markdown(colour = '#003616', hjust = 0.5, size = 8,
                                        family = 'Rosario', margin = margin(t = 5, b=5))) +
  labs(
    fill = ("Total Injuries"),
    y = "",
    x = "",
    caption = cap,
    subtitle = "Tracking Tornado-related injuries in the US from 1950 to 2022",
    title = "TORNADOS") +
  guides(fill = guide_colourbar(direction = 'horizontal', ticks.linewidth=2,
                                barwidth = 12, barheight = 0.3, title.position = "top"))

ggsave("Tornados.png", height=7, width=9, bg = "white")


