# load libraries
pacman::p_load(countrycode, tidyverse, janitor, ggimage, scales, ggtext, showtext, lubridate, patchwork)

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Neucha")

# Load data
tuesdata <- tidytuesdayR::tt_load(2022, week = 28)
flights <- tuesdata$flights

# Wrangle data
flights_cln <- flights |>
  clean_names() |>
  mutate(flt_date = ymd(flt_date)) |>
  filter(between(flt_date, as.Date("2020-05-1"), as.Date("2022-05-31"))) |>
  mutate(period = ifelse(flt_date > as.Date("2021-04-30"), "recent", "earlier")) |>
  group_by(period, state_name) |>
  summarise(departure = sum(flt_dep_1, na.rm = TRUE),
            arrivals = sum(flt_arr_1, na.rm = TRUE), .groups = "drop") |>
  arrange(period, desc(departure))

# Filter the top 10 countries
top_flights_countries <- flights_cln |>
  group_by(period) |>
  slice_max(order_by = departure, n = 10) |>
  mutate(state_name = str_replace(state_name, "Türkiye", "Turkey"),
         code = countrycode(state_name, "country.name", "iso2c")) |>
  arrange(desc(departure))

rlb <- top_flights_countries |>
  filter(period == "recent") |>
  arrange(desc(departure))

llb <- top_flights_countries |>
  filter(period == "earlier") |>
  arrange(desc(departure))

# Plot
dep <- ggplot(top_flights_countries, aes(departure, reorder(state_name, -departure))) +
  geom_line(aes(group = state_name), alpha = 0.9, size = 0.7, color="#000000") +
  geom_point(aes(color = period), size = 3.5, shape = 21, fill = "white", 
             stroke = 1, show.legend = FALSE) +
  geom_flag(x = -55000, aes(image = code)) +
  geom_text(aes(x = -400000, y = state_name, label = state_name),
            family = "Neucha",
            hjust = 0, size = 3, color = "#14213d"
  ) +
  geom_text(data = rlb, aes(color = period, label = departure),
            size = 1.5, hjust = -.5, family = "Neucha") +
  geom_text(data = llb, aes(color = period, label = departure),
            size = 1.5, hjust = 1.3, family = "Neucha") +
  scale_color_manual(values=c("#dd1c1a","#07a0c3")) +
  scale_x_continuous(breaks = c(0, 200000, 400000, 600000, 800000, 1000000),
                     labels = label_comma(scale = 0.001,  suffix = "k"),
                     limits = c(-400000, 1250000)) +
  labs(x="Depatures",
       title = "Flight Departures") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(color = "#000000", family = "Neucha", size = 7),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(family = "Neucha", face = "bold",
                                  size = 10, hjust = 0.5))

arr <- ggplot(top_flights_countries, aes(arrivals, reorder(state_name, -arrivals))) +
  geom_line(aes(group = state_name), alpha = 0.9, size = 0.7, color="#000000") +
  geom_point(aes(color = period), size = 3.5, shape = 19, 
             stroke = 1, show.legend = FALSE) +
  geom_flag(x = -55000, aes(image = code)) +
  geom_text(aes(x = -400000, y = state_name, label = state_name),
            family = "Neucha",
            hjust = 0, size = 3, color = "#14213d"
  ) +
  geom_text(data = rlb, aes(color = period, label = arrivals),
            size = 1.5, hjust = -.5, family = "Neucha") +
  geom_text(data = llb, aes(color = period, label = arrivals),
            size = 1.5, hjust = 1.3, family = "Neucha") +
  scale_color_manual(values=c("#dd1c1a","#07a0c3")) +
  scale_x_continuous(breaks = c(0, 200000, 400000, 600000, 800000, 1000000),
                     labels = label_comma(scale = 0.001,  suffix = "k"),
                     limits = c(-400000, 1250000)) +
  labs(x="Arrivals",
       title = "Flight Arrivals") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(color = "#000000", family = "Neucha", size = 7),
        axis.title.y = element_blank(),
        legend.position = "none",
        axis.line.x = element_line(colour = "#000000", size = 0.3),
        plot.title = element_text(family = "Neucha", face = "bold",
                                  size = 10, hjust = 0.5))
# Combining the two plots

dep /
  plot_spacer() /
  arr  +
  plot_annotation(
    title = 'Top 10 European Countries by the Number of <br> Flight Departures & Arrivals',
    subtitle= "Flight Arrivals has increased significantly across Europe in the 12 mths to end of <br> <span style='color:#07a0c3;'>**May 2022**</span> compared to <span style='color:#dd1c1a;'>**May 2021**</span>",
    caption = 'Graphic: Manasseh. O | #TidyTuesday | Source: Eurocontrol',
    theme = theme(
      plot.title = element_markdown(
        family = "Neucha",
        lineheight = 1,
        hjust = 0.5,
        size = 12
      ),
      plot.subtitle = element_markdown(size = 8, hjust = 0.1,
                                       family = "Neucha", lineheight = 1, 
                                       face = "italic", colour = "#000000"),
      plot.caption = element_text(
        family = "Neucha", size = 6,
        margin = margin(20, 0, 0, 0), hjust = 0.9
      )
    )
  ) +
  plot_layout(heights = c(0.6, 0.01,  0.6)) &
  theme(
    plot.background = element_rect(fill = "#F9F9F9", colour = NA)
  )

ggsave("flight.png", height=11, width=8)

