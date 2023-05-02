
# load libraries
pacman::p_load(tidyverse, viridis, hrbrthemes, ggridges, showtext, ggtext, ggpath, patchwork)

# Import fonts
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add_google(name = "Caveat")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | #TidyTuesday wk:18 | Source: Portal Project")

# load data
plots <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/plots.csv')
species <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/species.csv')
surveys <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/surveys.csv')

# data wrangle
## filter 'White-throated woodrat-Neotama albigula'
# replace incorrect "NA" values with correct specie text
surveys_df <- surveys |>
  select(censusdate, month, day, year, treatment, species, sex, wgt) |>
  mutate(species = case_when(is.na(species) | species == NA ~ "Neotama albigula", TRUE ~ species)) |>
  filter(species == "Neotama albigula") |>
  drop_na(wgt, sex)

# plot 1: Tile plot
p1 <- surveys_df |>
  summarise(med_wgt = median(wgt), .by = c(month,year)) |>
  ggplot() +
  geom_tile(aes(year, month, fill = med_wgt), width = 0.9, height = 0.2, colour = 'black') +
  scale_x_continuous(breaks = seq(1978, 2018, by = 2)) +
  scale_y_discrete(limits = month.name, expand = c(0, 0.5)) +
  scale_fill_gradient(low = "#fafde9", high = '#9b8f0d') +
  labs(
    fill = "Median Weight (g)",
    x = "",
    y = "") +
  theme_modern_rc() +
  theme(
    text = element_text(),
    axis.title.x  = element_text(face = "bold", hjust = 0.5, size = 18),
    axis.text.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "top",
    legend.box.background = element_blank(),
    legend.text = element_text(size = 15),
    legend.title.align = 0.5,
    legend.title = element_text(face = "bold", size = 20)) +
  guides(fill = guide_colourbar(direction = 'horizontal', ticks.linewidth=2,
                                barwidth = 14, barheight = 0.3, title.position = "top"),
         guide_legend(title.position = "top"))

# plot 2: ridgeline plot
p2 <- surveys_df |>
  mutate(year = factor(year)) |>
  ggplot(aes(x = wgt, y = year, col = sex, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(option = "B") +
  scale_y_discrete(breaks = seq(1978, 2018, by = 1)) +
  theme_modern_rc() +
  theme(text = element_text(),
        legend.position="none",
        panel.spacing = unit(0.1, "lines"),
        axis.title.x  = element_text(face = "bold", hjust = 0.5, size = 18),
        axis.text.x = element_text(face = "bold", size = 16),
        axis.text.y = element_text(face = "plain", size = 16),
        strip.text = element_text(face = "bold", size = 14)) +
  labs(
    y = "",
    x = "Weight of White-Throated Woodrat (g)") +
  annotate("text", x = 75, y = '2001', label = "Male", colour ="white", 
           family = "Roboto Condensed", vjust = -2, hjust =-0.5) +
  annotate("text", x = 150, y = '2002', label = "Female", 
           family = "Roboto Condensed", colour ="white", vjust = -2)

# Patching
(p1 + p2) +
  plot_annotation(
    title = "THE PORTAL PROJECT: A LONG-TERM STUDY OF A CHIHUAHUAN DESERT ECOSYSTEM",
    subtitle = "White-Throated Woodrat's Surprising Palate: Preferring Protein-Rich Spiny Cacti for Nutrition.<br>Could that be the reason for their Big Size?",
    caption = cap,
    theme = theme(
      plot.title = element_markdown(family = "Roboto Condensed", colour = "white", face = "bold",
                                size = 40, hjust = 0.5, margin = margin(t = 5, b = 5)),
      plot.subtitle = element_markdown(family = "Caveat", colour = "#ecdb22",
                                size = 30, hjust = 0, margin = margin(t = 5, b = 5)),
      plot.background = element_rect(fill = "#1d1616", color = NA),
      panel.background = element_rect(fill = "#1d1616", color = NA),
      plot.caption = element_markdown(colour = 'white', hjust = 0.5, size = 20,
                                      family = 'Rosario', margin = margin(t = 20))))

# save plot
ggsave("rodent.png", height=16, width=24, bg = "#1d1616")
