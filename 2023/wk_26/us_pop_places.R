
# load libraries
pacman::p_load(tidyverse, sf, maps, ggtext, showtext)

# Font
font_add_google(name = "Cabin Sketch")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | Source: usgs.gov | #TidyTuesday {wk:26}")

# load data
us_place_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_names.csv')
us_place_history <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_history.csv')

# data wrangle
# Filter rows containing the word "Country Club" 
Country_club <- us_place_names[grepl("Country club", us_place_names$feature_name, ignore.case = TRUE), ]

# Check for points outside the U.S coordinates
outside_us <- Country_club$prim_lat_dec < 24.396308 | Country_club$prim_lat_dec > 49.384358 | 
  Country_club$prim_long_dec < -125.000000 | Country_club$prim_long_dec > -66.934570

# Get the points outside the U.S
points_outside_us <- Country_club[outside_us, ]

Country_club = Country_club |>
  filter(state_name != "Puerto Rico")

# Filter rows with complete coordinates
Country_club <- Country_club[complete.cases(Country_club[c("prim_lat_dec", "prim_long_dec")]), ]

# Download the U.S states shapefile
us_states <- map_data("state")

# Set a dark color palette
dark_palette <- c("#1a2c32", "#2d464c", "#356169", "#95a5a6", "#bdc3c7")

# plot
ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), color = dark_palette[2], fill = dark_palette[1]) +
  geom_point(data = Country_club, aes(x = prim_long_dec, y = prim_lat_dec), color = "#eefb9b", size = 1.5) +
  labs(title = "US Populated Areas with the word ~Country Club~ in their Vicinity",
       caption = cap) +
  theme_void() +
  theme(panel.background = element_blank(),
        plot.title = element_text(color = "#0f0f0f", family = "Cabin Sketch",
                                  face = "bold", size = 15, hjust = 0.1),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.caption = element_markdown(colour = '#1d4ed8', hjust = 0.5, size = 11,
                                        family = 'Rosario', margin = margin(t=10, b=10)),
        plot.margin = margin(b = 20, t = 20, r = 20, l = 20)
        )

ggsave("US_country_clubs.png", height=8, width=12, bg = "#F9F9F9")
