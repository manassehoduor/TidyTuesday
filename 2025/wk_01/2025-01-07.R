# Load Libraries
pacman::p_load(tidyverse, glue, ggtext, showtext)

# Load Fonts
font_add_google(name = "Rosario")
font_add_google(name = "Averia Sans Libre")
font_add_google(name = "Averia Gruesa Libre")
font_add_google(name = "Averia Serif Libre")

showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# Data Prep
svns_2024_standings <- data.frame(
  Country = c("FIJ", "ESP", "FRA", "RSA", "ARG", "NZL", "GBR", "AUS", "KEN", "URU", "USA", "IRE"),
  DXB = c(20, 18, 12, 10, 16, 14, 6, 8, 3, 4, 1, 2),
  CPT = c(16, 14, 18, 20, 12, 10, 6, 4, 8, 2, 3, 1)
)

svns_2024_standings <- svns_2024_standings |>
  mutate(Tot_n = DXB + CPT) |>
  arrange(Tot_n)

# Reshape
svns_2024_standings_long <- svns_2024_standings |>
  gather(Events, Points, -Country, -Tot_n) |>
  mutate(Events = factor(Events, levels = c("CPT", "DXB")),
         Country = factor(Country, levels = data$Country))

# Plot
ggplot(svns_2024_standings_long, aes(x = Country, y = Points, fill = Events)) +
  geom_col(width = 0.18, color = "#31363F", linewidth = 0.5, show.legend = TRUE, position = "stack") +
  geom_text(aes(x = Country, y = 0, label = glue("{Country}  {Tot_n}")), 
            hjust = 1.2, size = 3.5, colour = "#FFFFFF", family = "Averia Sans Libre") +
  coord_polar(theta = "y",  start = 0) +
  scale_fill_manual(values = c("DXB" = "#FFEB00", "CPT" = "#7C00FE")) +
  ylim(0, 50) +
  annotate("point", x = 0, y = 0, size = 10, color = "#ffffff", shape = 20, fill = "#ffffff") + 
  theme_void() +
  labs(title = "HSBC World Rugby SVNS 2024/2025 Series Men's Standings",
       subtitle = "Events completed: Dubai and Cape Town",
       caption = "Graphic: Manasseh Oduor \n Source: https://www.svns.com/en/standings#men \n #TidyTuesday {wk:1-2025}",
       fill = "Event") +
  theme(
        text = element_text(family = "Averia Gruesa Libre"),
        plot.title = element_markdown(size = 12, color = "#EBD3F8", face = "bold", family = "Averia Serif Libre", hjust = 0.5, margin = margin(t=15,b=10)),
        plot.subtitle = element_text(size = 11, family = "Averia Sans Libre", color = "#B4CDE6", margin = margin(t=5,b=15)),
        plot.margin = margin(l=5, r=5, t=5, b=5),
        plot.caption = element_text(family = "Rosario", color = "#F9F9F9", size = 9, hjust = 1, margin = margin(t=10, b=10)),
        legend.position = "top",
        legend.title = element_text(color = "#F9F9F9", size = 10, family = "Rosario"),
        legend.text = element_text(color = "#F9F9F9", size = 8, family = "Rosario")) +
  guides(fill = guide_legend(title = "Event", title.position = "top", title.hjust = 0.5)) 

# Save plot
ggsave("hsbc7s.png", width = 8, height = 8, bg = "#31363F")