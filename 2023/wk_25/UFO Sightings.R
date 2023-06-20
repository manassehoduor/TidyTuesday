
# load libraries
pacman::p_load(tidyverse, leaflet, htmltools, htmlwidgets, ggtext, showtext, mapview, webshot2)

# Font
font_add_google(name = "Ubuntu Condensed")
font_add_google(name = "Roboto Condensed")

showtext_opts(dpi = 300)
showtext_auto(enable= TRUE)


# load data
ufo_sightings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')
places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/places.csv')
day_parts_map <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/day_parts_map.csv')

# data wrangle
ufo_data <- places |>
  select(latitude, longitude, country_code, state) |>
  filter(country_code == "AU") 

# Plot
UFOmap <- leaflet() |>
  addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
           attribution = 'Map data &copy; <a href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors, Tiles &copy; <a href="https://carto.com/attribution/">CARTO</a>') |>
  addCircleMarkers(data = ufo_data,
                   lat = ~latitude, lng = ~longitude, popup = ~state,
                   color = ~"#FFFF00", radius = 7,
                   stroke = FALSE, fillOpacity = 0.5, opacity = 0.9) |>
  setView(lng = 133.7751, lat = -25.2744, zoom = 4.5) |>
  addControl(html = '<h1 style="font-size: 16px; font-family: Ubuntu Condensed; text-align: center; padding: 2px; margin: 1; ">UFO Sightings in Australia</h1>',
             position = "topright") |>
  addControl(html = '<h3 style="font-size: 12px; font-family: Roboto Condensed; text-align: center; padding: 1px; margin: 0; ">Graphic: Manasseh Oduor | Data Source: nuforc.org</h3>',
             position = "bottomleft")

# Save the leaflet map as an HTML file
saveWidget(UFOmap, file = "leaflet_map.html", selfcontained = TRUE)

# Use webshot2 to capture the HTML map and save as a PNG image
webshot2::webshot("leaflet_map.html", "leaflet_map.png", vwidth = 1000, vheight = 800)


