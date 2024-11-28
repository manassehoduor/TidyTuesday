
# load libraries
pacman::p_load(tidyverse, ggtext, showtext, scales, shadowtext, grid)

# Font
font_add_google(name = "Roboto Condensed")
font_add_google(name = "Roboto Slab")
font_add_google(name = "Rosario")
font_add_google(name = "Averia Gruesa Libre")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# load data
cbp_resp <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-26/cbp_resp.csv')
cbp_state <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-26/cbp_state.csv')

# Data wrangling
cbp_resp_df <- cbp_resp |>
  mutate(month_abbv = stringr::str_to_title(month_abbv),
         date = as.Date(paste(fiscal_year, month_abbv, "01", sep = "-"), format = "%Y-%b-%d")) |>
  summarize(monthly_total = sum(encounter_count, na.rm = TRUE), .by = c(encounter_type, fiscal_year, month_abbv)) |>
  mutate(month_number = match(month_abbv, month.abb)) |>
  group_by(encounter_type, fiscal_year) |>
  mutate(peak_month = month_number[which.max(monthly_total)]) |>
  ungroup()

cbp_stream_data_fiscal_year <- cbp_resp |>
  group_by(encounter_type, fiscal_year) |>
  summarize(value = sum(encounter_count, na.rm = TRUE), .groups = "drop")

min_value <- min(cbp_stream_data_fiscal_year$value, na.rm = TRUE)
max_value <- max(cbp_stream_data_fiscal_year$value, na.rm = TRUE)

missing_months <- cbp_resp |>
  mutate(month_number = match(stringr::str_to_title(month_abbv), month.abb)) |>
  complete(encounter_type, fiscal_year, month_number = 1:12) |>
  filter(is.na(encounter_count)) |>
  group_by(encounter_type, fiscal_year) |>
  summarize(missing_months = list(month.abb[month_number]), .groups = "drop") |> 
  filter(lengths(missing_months) > 0)

# Heat map
heatmap_plot <- ggplot(cbp_stream_data_fiscal_year, aes(x = fiscal_year, y = encounter_type, fill = value)) +
  geom_tile(color = "black", size = 5) +
  scale_fill_gradient(
    low = "#FFF7D1", high = "#D4B000",
    limits = c(min_value, max_value),
    na.value = "#F5F5DC",
    labels = function(x) {
      ifelse(x >= 1e6, 
             paste0(scales::comma(x / 1e6, accuracy = 0.1), "m"),
             paste0(scales::comma(x / 1e3, accuracy = 1), "k"))
    },
    guide = guide_colorsteps(
      title = "Total Encounters",
      title.position = "top",
      title.hjust = 0.6,
      barheight = unit(0.6, "cm"),
      barwidth = unit(8, "cm"),
      breaks = seq(floor(min_value / 1e6), ceiling(max_value / 1e6), by = 1)
    )
  ) +
  geom_shadowtext(data = cbp_stream_data_fiscal_year |> 
                    group_by(encounter_type) |> 
                    summarize(fiscal_year = min(fiscal_year), value = max(value)),
                  aes(x = fiscal_year, y = encounter_type, label = encounter_type),
                  family = "Roboto Condensed", color = "#FFFFFF", size = 8, bg.r = 0.1, bg.g = 0.1, bg.b = 0.1, bg.alpha = 0.8, angle = 90, hjust = 0.5, vjust = -8) +
  geom_text(aes(label = scales::comma(value)), color = "#03001C", size = 6, family = "Roboto Condensed", position = position_nudge(y = -0.45), vjust = 0.1) +
  labs(
    title = "U.S. Customs and Border Protection (CBP) Encounters",
    subtitle = "Encounters often peak in spring (March, April, May) or late summer (August, September). <br> Apprehensions surged over sevenfold, while inadmissibles more than quadrupled from 2020 to 2024.
    <br> Note: Missing data for Expulsion encounter type for the Fiscal Year: 2020 (Jan, Feb, Oct, Nov, Dec) <br> Fiscal Year: 2023 (Jun, Jul, Aug, Sep) & Fiscal Year 2024 (All months)",
    x = "Fiscal Year",
    fill = "Total Encounters",
    caption = "Graphic: Manasseh Oduor \n Source: Tony Galván via www.cbp.gov \n #TidyTuesday {wk:48-2024}") +
  theme_void() +
  theme(
    text = element_text(family = "Roboto Condensed", colour = "black", size = 15),
    axis.text.x = element_text(hjust = 1, size = 15, color = "black", vjust = 1, margin = margin(t = 10)),
    axis.title.x = element_text(size = 18, face = "bold", color = "black",  margin = margin(t = 10)),
    plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size = 30, family = "Roboto Slab", margin = margin(t = 10, b = 5)),
    plot.subtitle = element_markdown(hjust = 0.1, color = "#2E0249", size = 15, family = "Averia Gruesa Libre", margin = margin(t = 5, b = 10)),
    plot.caption = element_text(family = "Rosario", color = "#091057", size = 14, hjust = 0.95),
    legend.position = "top",
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(t = 10, b = 10, l = 10, r = 10)
  )

sparklines <- cbp_resp |> 
  mutate(month_abbv = stringr::str_to_title(month_abbv),
         date = as.Date(paste(fiscal_year, month_abbv, "01", sep = "-"), format = "%Y-%b-%d")) |>
  group_by(encounter_type, fiscal_year, month_abbv) |>
  summarize(monthly_total = sum(encounter_count, na.rm = TRUE), .groups = "drop") |>
  mutate(month_number = match(month_abbv, month.abb)) |>
  group_by(encounter_type, fiscal_year) |>
  nest() |>
  mutate(
    sparkline_plot = map(data, ~ {
      peak_month <- .x[which.max(.x$monthly_total), ]$month_abbv
      peak_value <- .x[which.max(.x$monthly_total), ]$monthly_total
      
      ggplot(.x, aes(x = month_number, y = monthly_total)) +
        geom_line(size = 0.3, color = "#4E3636", linetype = "dotted") +
        geom_smooth(method = "loess", color = "#116D6E", se = FALSE) +
        geom_point(size = 1.5, color = "#1A1A1D", shape = 20) +
        geom_point(data = .x |> filter(month_abbv == peak_month), 
                   size = 2, color = "#1A1A1D", shape = 8) +
        geom_text(data = .x |> filter(month_abbv == peak_month), 
                  aes(label = paste0(month_abbv, ":", scales::comma(peak_value / 1e3, accuracy = 1), "k")),
                  size = 3.5, vjust = -0.01, hjust = 1.15, family = "Roboto Condensed", color = "#5F264A") +
        theme_void() +
        theme(legend.position = "none")
    })
  )

# Add sparklines to the heatmap
for (i in seq_len(nrow(sparklines))) {
  encounter <- sparklines$encounter_type[i]
  year <- sparklines$fiscal_year[i]
  g <- ggplotGrob(sparklines$sparkline_plot[[i]])
  
  heatmap_plot <- heatmap_plot +
    annotation_custom(
      grob = g,
      xmin = year - 0.4, xmax = year + 0.4,
      ymin = as.numeric(factor(encounter, levels = c("Apprehensions", "Expulsions", "Inadmissibles"))) - 0.4, 
      ymax = as.numeric(factor(encounter, levels = c("Apprehensions", "Expulsions", "Inadmissibles"))) + 0.4
    )
}

# Save plot
ggsave("CBP.png", width = 17, height = 13, bg = "#F5F5DC")
