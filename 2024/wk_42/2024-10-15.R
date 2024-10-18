# Load libraries
pacman::p_load(tidyverse, lubridate, ggtext, showtext)

# Font
font_add_google(name = "Roboto Condensed")
font_add_google(name = "Ubuntu Condensed")
font_add_google(name = "Rosario")
font_add_google(name = "Dangrek")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Caption
cap <- "Graphic: Manasseh Oduor | #TidyTuesday | #wk:42 2024"

# Load data
tuesdata <- tidytuesdayR::tt_load(2024, week = 42)
orcas <- tuesdata$orcas

# Data Wrangling
orcas_df <- orcas |> 
  mutate(begin_hour = hour(hms::as.hms(begin_time)))

min_year <- min(orcas_df$year, na.rm = TRUE)
max_year <- max(orcas_df$year, na.rm = TRUE)
min_hour <- min(orcas_df$begin_hour, na.rm = TRUE)
max_hour <- max(orcas_df$begin_hour, na.rm = TRUE)

# Plot
ggplot(orcas_df, aes(x = year, y = begin_hour)) +
  geom_bin2d(bins = 20, na.rm = TRUE) +
  scale_fill_gradientn(
    colors =  c("white", "lightblue", "blue"),
    na.value = "gray90",
    guide = guide_colorsteps(
      title = "No. of Orcas Encountered", title.position = "top", title.hjust = 0.5, 
      barheight = unit(0.4, "cm"), 
      barwidth = unit(2.5, "cm")),
    breaks = c(5, 10, 15, 20),
    labels = as.character(c(5, 10, 15, 20))) +
  scale_x_continuous(limits = c(min_year, max_year), 
                     breaks = seq(min_year, max_year, by = 1), expand = c(0, 0)) +  
  scale_y_continuous(limits = c(min_hour, max_hour), 
                     breaks = seq(min_hour, max_hour, by = 1), expand = c(0, 0)) +
  theme_void() +
  coord_flip() +
  labs(
       title = "Temporal Patterns of Southern Resident Killer Whale Encounters",
       x = "Year", y = "Hour of Day (24-hr)",
       caption = cap) +
  theme(
    text = element_text(family = "Roboto Condensed"),
    legend.position = "top",
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(size = 10, angle = 10, hjust = 1),
    axis.text.y = element_text(size = 10, angle = 45, hjust = 0.5),
    panel.grid = element_blank(),
    strip.text = element_text(size = 16, color = "#354259", family = "Roboto Condensed"),
    plot.title = element_markdown(size = 18, hjust = 0.5, colour = "steelblue", face = "bold", family = "Dangrek", margin = margin(t = 5, b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    plot.caption = element_markdown(colour = 'black', hjust = 0.5, size = 11, family = 'Rosario', margin = margin(t = 20, b = 5)),
    plot.margin = margin(b=30, t=30, r=20, l=20)
  ) 
# +guides(x = guide_axis(position = "top")) 

ggsave("orcas.png", width = 10, height = 10, bg = "white")
