
# load data
owid_energy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv')

# load libraries
pacman::p_load(tidyverse, countrycode, ggtext, showtext, CGPfunctions,
               AfricaCountryBins, rnaturalearth, rnaturalearthdata)

# Font
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add_google(name = "Ubuntu Condensed")
font_add_google(name = "Cabin Sketch")
font_add_google(name = "Rye")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | Source: ourworldindata.org | #TidyTuesday {wk:23}")

# data wrangle
energy_df <- owid_energy |>
  select(country, year, iso_code, per_capita_electricity)

energy_df <- energy_df |>
  mutate(continent = countrycode(iso_code, origin = "iso3c",
                                 destination = "continent")) |>
  filter(continent == "Africa",
         year >= 2000)

## mean electricity generation per Capita
energy_df1 <- energy_df |>
  select(country, iso_code, per_capita_electricity) |>
  summarise(mean_per_cap_elec = round(mean(per_capita_electricity),2), .by = c(country, iso_code))

## load the map data for East Africa
east_africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf") |>
  filter(name %in% c("Burundi", "Comoros", "Djibouti", "Eritrea", "Ethiopia", "Kenya", "Madagascar",
                     "Malawi", "Mauritius", "Rwanda", "Seychelles", "Somalia", "Sudan", "Tanzania",
                     "Uganda", "Zambia"))

merged_energy <- merge(east_africa, energy_df1, by.x = "iso_a3", by.y = "iso_code", all.x = TRUE)

## East Africa Data 2012 & 2021
EA <- energy_df |>
  select(country, year, iso_code, per_capita_electricity) |>
  filter(country %in% c("Burundi", "Comoros", "Djibouti", "Eritrea", "Ethiopia", "Kenya", "Madagascar",
                     "Malawi", "Mauritius", "Rwanda", "Seychelles", "Somalia", "South Sudan", "Tanzania",
                     "Uganda", "Zambia"),
         year %in% c(2012, 2021)) |>
  mutate(year = paste("Year: ", year))

EA2 <- EA |>
  filter(!country %in% c("Seychelles", "Mauritius", "Zambia"))

cols <- c("Burundi" = "#1e159d", "Comoros" = "#b0b9ff", "Djibouti" = "#e11d48", 
          "Eritrea" = "#ec4899", "Ethiopia" = "#155e75", "Kenya" = "#042f2e", 
          "Madagascar" = "#16a34a", "Malawi" = "#65a30d", "Mauritius" = "#422006", 
          "Rwanda" = "#f59e0b", "Seychelles" = "#b91c1c", "Somalia" = "#78716c", 
          "South Sudan" = "#a3a3a3", "Tanzania" = "#6b7280", "Uganda" = "#450a0a", 
          "Zambia" = "#172554")

cols2 <- c("Burundi" = "#1e159d", "Comoros" = "#b0b9ff", "Djibouti" = "#e11d48", 
           "Eritrea" = "#ec4899", "Ethiopia" = "#155e75", "Kenya" = "#042f2e", 
           "Madagascar" = "#16a34a", "Malawi" = "#65a30d", "Rwanda" = "#f59e0b", 
           "Somalia" = "#78716c", "South Sudan" = "#a3a3a3", "Tanzania" = "#6b7280", 
           "Uganda" = "#450a0a")

# plot 1
ggplot() +
  geom_sf(data = merged_energy, aes(fill = mean_per_cap_elec)) +
  scale_fill_gradient(low = "#fca5a5", high = "#b91c1c", name = "Mean (KWh)") +
  labs(title = "Per Capita Electricity Generation in East Africa",
       subtitle = "Data: {2000:2021} measured in kilowatt-hours",
       caption = cap) +
  theme_void() +
      theme(
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15, colour = "#022c22", face = "bold", 
                                    family = "Ubuntu Condensed"),
        plot.subtitle = element_text(family = "Cabin Sketch", 
                                     colour = "black", size = 15, face = "bold",
                                     margin = margin(t = 3, b = 2, unit = "mm"), hjust = 0.2
        ),
        plot.title = element_text(
          face = "bold", colour = "black", family = "Rye", size = 20, hjust = 0.5
        ),
        plot.caption = element_markdown(colour = '#1d4ed8', hjust = 0.5, size = 11,
                                        family = 'Rosario', margin = margin(t=10, b=10)),
        plot.margin = margin(b = 10, t = 20, r = 5, l = 20)) +
  guides(
    fill = guide_colourbar(direction = 'horizontal', ticks.linewidth=2,
                           barwidth = 12, barheight = 0.3, title.hjust = 0.5,
      title.position = "top"))

ggsave("africa1.png", height=10, width=9, bg = "#F9F9F9")

# plot 2
energy_df1 |>
  ggplot(aes(country = iso_code, fill = mean_per_cap_elec)) +
  geom_countrybins_africa(
    radius = unit(0, "pt"),
    family = "Roboto Condensed", size = 0.5) +
  scale_fill_gradient(low = "#fca5a5", high = "#b91c1c", name = "Mean (KWh)") +
  labs(
    title = "Per Capita Electricity Generation in Africa",
    subtitle = "Data: {2000:2021} measured in kilowatt-hours",
    caption = cap) +
  guides(
    fill = guide_colourbar(direction = 'horizontal', ticks.linewidth=2,
                           barwidth = 12, barheight = 0.3, title.hjust = 0.5,
                           title.position = "top")) +
  theme_void() +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15, colour = "#022c22", face = "bold", 
                                family = "Ubuntu Condensed"),
    legend.position = "top",
    plot.subtitle = element_text(family = "Cabin Sketch", 
                                 colour = "black", size = 15, face = "bold",
                                 margin = margin(t = 3, b = 2, unit = "mm"), hjust = 0.5
    ),
    plot.title = element_text(
      face = "bold", colour = "black", family = "Rye", size = 20, hjust = 0.5
    ),
    plot.caption = element_markdown(colour = '#1d4ed8', hjust = 0.5, size = 11,
                                    family = 'Rosario', margin = margin(t=10, b=10)),
    plot.margin = margin(b = 10, t = 20, r = 5, l = 20))

ggsave("africa2.png", height=10, width=9, bg = "#F9F9F9")

# plot 3
newggslopegraph(EA, year, per_capita_electricity, country,
                Title = "Per Capita Electricity Generation Evolution in East Africa",
                SubTitle = "Evolution of Per Capita Electricity Generation {2012-2021}, measured in kilowatt-hours",
                LineColor = cols,
                DataTextSize = 3,
                DataTextColor = "#a16207",
                LineThickness = 0.5,
                XTextSize = 15,
                YTextSize = 3.5,
                ThemeChoice = "ipsum",
                Caption = cap) +
  theme(
    legend.position = "none",
    text = element_text(family = "Roboto Condensed", size = 12),
    plot.subtitle = element_text(family = "Ubuntu Condensed", 
                                 colour = "#3b82f6", size = 14, face = "bold",
                                 margin = margin(t = 3, b = 2, unit = "mm"), hjust = 0.5),
    plot.title = element_text(
      face = "bold", colour = "#3730a3", family = "Rye", size = 16, hjust = 0.5),
    plot.caption = element_markdown(colour = 'black', hjust = 0.5, size = 10,
                                    family = 'Rosario', margin = margin(t=10, b=10)),
    plot.margin = margin(b = 10, t = 20, r = 5, l = 20))

ggsave("EA.png", height=8, width=8, bg = "#F9F9F9")


# plot 4
newggslopegraph(EA2, year, per_capita_electricity, country,
                Title = "Per Capita Electricity Generation Evolution in East Africa",
                SubTitle = "Evolution of Per Capita Electricity Generation {2012-2021}, measured in kilowatt-hours",
                LineColor = cols2,
                DataTextSize = 3,
                DataTextColor = "#a16207",
                LineThickness = 0.5,
                XTextSize = 15,
                YTextSize = 3.5,
                ThemeChoice = "ipsum",
                Caption = cap) +
  theme(
    legend.position = "none",
    text = element_text(family = "Roboto Condensed", size = 12),
    plot.subtitle = element_text(family = "Ubuntu Condensed", 
                                 colour = "#3b82f6", size = 14, face = "bold",
                                 margin = margin(t = 3, b = 2, unit = "mm"), hjust = 0.5),
    plot.title = element_text(
      face = "bold", colour = "#3730a3", family = "Rye", size = 16, hjust = 0.5),
    plot.caption = element_markdown(colour = 'black', hjust = 0.5, size = 10,
                                    family = 'Rosario', margin = margin(t=10, b=10)),
    plot.margin = margin(b = 10, t = 20, r = 5, l = 20))

ggsave("EA2.png", height=8, width=8, bg = "#F9F9F9")

