# Load libraries
pacman::p_load(tidyverse, showtext, ggtext, grid)

# Add fonts
font_add("dsdigital", "DS-DIGIT.TTF")
font_add("pixel10", "10Pixel-Bold.ttf")
font_add_google(name = "Rosario")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load data
weekly_gas_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-01/weekly_gas_prices.csv')

# Data wrangling
fuels_df <- weekly_gas_prices |>
  filter(
    (fuel == "gasoline" & grade %in% c("regular", "midgrade", "premium") & formulation == "all") |
      (fuel == "diesel" & grade == "ultra_low_sulfur")
  )

combn_wk_prices_df <- fuels_df |>
  filter(!is.na(price)) |>
  group_by(date, fuel, grade) |>
  summarise(price = mean(price), .groups = "drop") |>
  unite("fuel_grade", fuel, grade) |>
  pivot_wider(names_from = fuel_grade, values_from = price)

combn_wk_prices_df <- combn_wk_prices_df |>
  mutate(
    total_price = rowSums(pick(starts_with("gasoline_"), starts_with("diesel_")), na.rm = TRUE)
  )

exp_wk_df <- combn_wk_prices_df |>
  filter(total_price == max(total_price, na.rm = TRUE))

exp_wks_df <- combn_wk_prices_df |>
  arrange(desc(total_price)) |>
  slice_head(n = 5)

exp_wks_df <- exp_wks_df |>
  mutate(rank = row_number())

exp_wks_df <- exp_wks_df |>
  mutate(
    date_label = format(date, "%b %d, %Y"),
    date_label = factor(date_label, levels = date_label[order(rank)])
  )

prices_lg_df <- exp_wks_df |>
  pivot_longer(
    cols = starts_with("gasoline") | starts_with("diesel"),
    names_to = "TypeFull",
    values_to = "Price"
  ) |>
  mutate(
    Type = case_when(
      TypeFull == "gasoline_regular" ~ "Regular",
      TypeFull == "gasoline_midgrade" ~ "Extra",
      TypeFull == "gasoline_premium" ~ "Supreme+",
      TypeFull == "diesel_ultra_low_sulfur" ~ "Diesel",
      TRUE ~ TypeFull
    ),
    DigitColor = ifelse(Type == "Diesel", "green", "red"),
    y = case_when(
      Type == "Regular"   ~ 4.0,
      Type == "Extra"     ~ 3.8,
      Type == "Supreme+"  ~ 3.6,
      Type == "Diesel"    ~ 3.4
    )
  )

prices_lg_df$OuterFill <- ifelse(prices_lg_df$Type == "Diesel", "#F7FD04", "#fdf5e6")

# Plot
ggplot(prices_lg_df) +
  # Background
  geom_rect(
    xmin = 0.4, xmax = 1.6, ymin = 0.5, ymax = 4.6, fill = "#003399", color = NA) +
  # Outer layout box
  geom_label(
    aes(x = 1, y = y, label = "", fill = OuterFill), color = NA,
    label.r = unit(0.3, "lines"), label.padding = unit(c(2, 4), "lines"), label.size = 0) +
  scale_fill_identity() +
  # Fuel type label
  geom_text(
    aes(x = 0.7, y = y + 0.09, label = toupper(Type)),
    hjust = 0, vjust = 1, size = 4, family = "dsdigital", color = "black") +
  # Price digits
  geom_label(
    aes(x = 1, y = y, label = sprintf("%.2f", Price), color = DigitColor),
    size = 7, fill = "#001f4d", family = "dsdigital", label.r = unit(0.2, "lines"), label.padding = unit(0.3, "lines"), label.size = 0) +
  scale_color_identity() +
  xlim(0.4, 1.6) +
  ylim(
    min(prices_lg_df$y) - 0.2,
    max(prices_lg_df$y) + 0.2) +
  facet_wrap(~ date_label, nrow = 1, strip.position = "top") +
  theme_void() +
  labs(
    title = "Pain at the Pump ",
    subtitle = "Fuel prices soared to historic highs in the summer of 2022. A perfect storm of global turmoil, supply shocks, the economic fallout from Russia’s invasion of Ukraine, and post-COVID demand pushed U.S. fuel costs to record breaking. However, it marked a turning point. Electric Vehicles market share crossed 6%, and public attention shifted sharply toward alternatives. These were the five most expensive weeks ever recorded across all fuel types.",
    caption = "Graphic: Manasseh Oduor • Source: eia.gov \n #TidyTuesday {wk:26-2025}"
  ) +
  theme(
    plot.background = element_rect(fill = "#003399", color = NA),
    strip.text = element_text(color = "black", size = 11, face = "bold", family = "pixel10"),
    strip.background = element_rect(fill = "#EBD6FB", color = "black", linewidth = 0.2),
    panel.spacing.x = unit(2, "lines"),
    panel.spacing.y = unit(2, "lines"),
    plot.title = element_text(color = "#FCF259", size = 25, face = "bold", hjust = 0.5, family = "pixel10"),
    plot.subtitle = element_textbox_simple(color = "white", box.colour = "#EBD6FB", size = 8, width = unit(0.9, "npc"),
                                           margin = margin(t=10, b=20), family = "Rosario", lineheight = 1.2),
    plot.caption = element_text(color = "white", size = 8, hjust = 1, family = "Rosario"),
    plot.margin = margin(t=10, l=50, b=10, r=50),
    strip.placement = "outside"
  )

ggsave("US_Gas_Prices.png", width = 9, height = 6)
