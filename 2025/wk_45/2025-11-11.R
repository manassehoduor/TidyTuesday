# Load libraries
pacman::p_load(tidyverse, showtext, ggtext)

# Load fonts
font_add_google("Urbanist")

showtext_auto(enable = TRUE)
showtext_opts(dpi = 350)

# Load data
who_tb_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-11/who_tb_data.csv')

# Data wrangling
who_tb_kenya <- who_tb_data |> 
  filter(country == "Kenya") |> 
  select(year, e_mort_exc_tbhiv_100k, e_mort_tbhiv_100k)

# Plot
ggplot(who_tb_kenya, aes(y = year)) +
  geom_point(aes(x = e_mort_exc_tbhiv_100k), color = "#0046FF", size = 7, shape = 45, stroke = 5) +
  geom_point(aes(x = e_mort_tbhiv_100k), color = "#FFA239", size = 2, shape = 3, stroke = 2) +
  geom_segment(
    aes(y = year, yend = year,
        x = e_mort_exc_tbhiv_100k, xend = e_mort_tbhiv_100k), color = "#999999", linewidth = 1.2, alpha = 0.4) +
  scale_y_reverse(breaks = seq(2000, 2023, by = 2)) +
  labs(
    title = "TB Mortality per 100,000 Kenya Population",
    subtitle = "<span style='color:#0046FF'>HIV-Negative</span> vs <span style='color:#FFA239'>HIV-Positive</span>",
    x = "Mortality Rate per 100,000", y = "Year",
    caption = "Graphic: Manasseh Oduor | Source: WHO via getTBinR package") +
  theme_bw() +
  theme(
    text = element_text(color = "#3D5656", family = "Urbanist"),
    axis.text = element_text(color = "#3D5656", size = 13, family = "Urbanist"),
    axis.title.y = element_text(color = "#3D5656", size = 15, family = "Urbanist", face = "bold"),
    axis.title.x = element_text(color = "#3D5656", size = 15, family = "Urbanist", face = "bold", vjust = -2),
    plot.title = element_text(color = "#0C2B4E", hjust = 0.5, size = 20, family = "Urbanist", face = "bold", margin = margin(b=10)),
    plot.subtitle = element_markdown(color = "#427A76", size = 15, hjust = 0.5, family = "Urbanist", face = "bold"),
    panel.background = element_rect(fill="#F9F8F6"),
    plot.background = element_rect(fill="#F9F8F6"),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#B6FFCE"),
    panel.border = element_blank(),
    legend.position = "top",
    plot.caption = element_text(size = 11, margin = margin(t=30,b=10)),
    plot.margin = margin(t=20, b=20, r=20, l=20)
  )

ggsave("tb_mortality.png", height = 9, width = 12)