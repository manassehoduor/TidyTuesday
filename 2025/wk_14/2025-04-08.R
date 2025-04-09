# Load libraries
pacman::p_load(tidyverse, geofacet, ggtext, showtext)

# Load Fonts
font_add_google(name = "Rosario")
font_add_google(name = "Roboto Condensed")
font_add_google(name = "Averia Sans Libre")
font_add_google(name = "Averia Serif Libre")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load data
care_state <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-08/care_state.csv')

# Data wrangling
care_state_df <- care_state |> 
  select(state, condition, measure_id, measure_name, score) |> 
  filter(measure_id %in% c("OP_18b", "OP_18b_LOW_MIN", "OP_18b_MEDIUM_MIN", "OP_18b_HIGH_MIN", "OP_18b_VERY_HIGH_MIN")) |> 
  filter(!state %in% c("AS", "GU", "MP", "PR", "VI")) |> 
  mutate(measure_ids = case_when(
           measure_id == "OP_18b_VERY_HIGH_MIN" ~ "Very high",
           measure_id == "OP_18b_HIGH_MIN" ~ "High",
           measure_id == "OP_18b_MEDIUM_MIN" ~ "Moderate",
           measure_id == "OP_18b_LOW_MIN" ~ "Low",
           TRUE ~ measure_id
         ))

# Split acuity levels and median
acuity_levels_df <- care_state_df |>
  filter(measure_ids != "OP_18b")

median_df <- care_state_df |>
  filter(measure_ids == "OP_18b") |>
  mutate(
    OP_18b_hr = ifelse(
      score >= 60,
      paste0(floor(score / 60), " hr ", score %% 60, " min"),
      paste0(score, " min")
    )
  )

# Compute ymin/ymax for each state
acuity_levels_stacked_df <- acuity_levels_df |>
  group_by(state) |>
  arrange(state, score) |>
  mutate(
    ymin = cumsum(lag(score, default = 0)),
    ymax = ymin + score,
    measure_ids = factor(measure_ids, levels = c("Low", "Moderate", "High", "Very high"))
  ) |>
  ungroup()

# Plot
ggplot(acuity_levels_stacked_df) +
  geom_rect(aes(xmin = 0.4, xmax = 0.6, ymin = ymin, ymax = ymax, fill = measure_ids)) +
  geom_segment(data = median_df,
               aes(x = 0.35, xend = 0.65, y = score, yend = score), color = "black", linewidth = 0.2, lineend = "round", linetype = 3) +
  geom_text(data = median_df,
            aes(x = 0.5, y = score, label = OP_18b_hr), size = 2.4, family = "Roboto Condensed", nudge_y = 150) +
  facet_geo(vars(state)) +
  scale_fill_manual(values = c("Very high" = "#FC5404", "High" = "#F98404", "Moderate" = "#F9B208", "Low" = "#F7FD04", "OP_18b" = "blue")) +
  coord_flip() +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "#343131", color = "black", linewidth = 0.5),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.2),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.5, "lines"),
    strip.text = element_text(size = 10, color = "white", family = "Averia Sans Libre"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(l=30, r=30, t=30, b=20),
    legend.text = element_text(family = "Averia Serif Libre"),
    legend.title.position = "top",
    legend.title = element_text(family = "Rosario", face = "bold"),
    legend.position = "right",
    plot.caption = element_text(size = 11, family = "Rosario", hjust = 0.5, face = "italic", color = "black", margin = margin(t=10)),
    plot.title = element_text(size = 20, colour = "black", face = "bold", family = "Rosario"),
    plot.subtitle = element_markdown(family = "Rosario", size = 15)
  ) +
  labs(
    title = "Emergency Room Visit Times by State",
    subtitle = "How long patients of a particular acuity level spent<br>in the emergency room before being sent home",
    fill = "Acuity Level",
    caption = "Graphic: Manasseh Oduor | Source: data.cms.gov \n #TidyTuesday {wk:14-2025}"
  ) 

ggsave("emergency_waiting_time2.png", height = 9, width = 12, bg = "#FFFAEC")
