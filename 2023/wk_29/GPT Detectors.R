
# load libraries
pacman::p_load(tidyverse, janitor, waffle, ggtext, showtext, patchwork)

# Font
font_add_google(name = "Roboto Condensed")
font_add_google(name = "Ubuntu Condensed")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | Source: arxiv.org/abs/2304.02819 | #TidyTuesday {wk:29}")

# load data
detectors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-18/detectors.csv')

# data wrangle
detectors_df <- detectors |>
  mutate_if(is.character, factor) |>
  count(kind, .pred_class, name = 'Freq') |>
  clean_names() |>
  summarise(n = sum(freq), .by = c(kind, pred_class)) |>
  mutate(percent = round(prop.table(n) * 100, 0), .by = kind)

# waffle chart 1: Human Essays
wf1 = detectors_df |> 
  filter(kind == "Human") |>
  ggplot(aes(fill = pred_class, values = percent)) +
  geom_waffle(
    n_rows = 10, size = 0.5, colour = "black", flip = TRUE, na.rm = TRUE,
    make_proportional = TRUE,
    radius = unit(10, "pt"),
    height = 0.8, width = 0.8) +
  coord_equal() +
  labs(
    title = "<span style='color:green;'>**Human**</span> Essays",
    subtitle = "<br>GPT Detectors incorrectly classified 449 (18%) of 2,468 <span style='color:green;'>**Human**</span> essays as being written by <span style='color:blue;'>**AI**</span><br>",
    fill = NULL, colour = NULL) +
  theme_minimal() +
  theme_enhance_waffle() +
  scale_fill_manual(
    values = c("blue", "green")) +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    text = element_text(family = "Roboto Condensed"),
    legend.position = "none",
    plot.title = element_markdown(family = "Roboto Condensed", size = 30, colour = "white", hjust = 0.5),
    plot.subtitle = element_textbox(
      size = 20, color = "black", fill = "#fbfdc4", box.color = "#e5ca0d",
      halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3), family = "Ubuntu Condensed"
    )
  )

# waffle chart 2: AI Essays
wf2 = detectors_df |> 
  filter(kind == "AI") |>
  ggplot(aes(fill = pred_class, values = percent)) +
  geom_waffle(
    n_rows = 10, size = 0.5, colour = "white", flip = TRUE, na.rm = TRUE,
    make_proportional = TRUE,
    radius = unit(10, "pt"),
    height = 0.8, width = 0.8) +
  coord_equal() +
  labs(
    title = "<span style='color:blue;'>**AI**</span> Essays",
    subtitle = "<br>GPT Detectors incorrectly classified 2,559 (69%) of 3,717 <span style='color:blue;'>**AI**</span> essays as being written by a <span style='color:green;'>**Human**</span><br>",
    fill = NULL, colour = NULL) +
  theme_minimal() +
  theme_enhance_waffle() +
  scale_fill_manual(
    values = c("blue", "green")
  ) +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    text = element_text(family = "Roboto Condensed"),
    legend.position = "none",
    plot.title = element_markdown(family = "Roboto Condensed", size = 30, colour = "white", hjust = 0.5),
    plot.subtitle = element_textbox(
      size = 20, color = "black", fill = "#fbfdc4", box.color = "#e5ca0d",
      halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3), family = "Ubuntu Condensed"
    )
  )


# Patch work
(wf1 + plot_spacer() + plot_layout(nrow = 1, widths = c(1, 0.01, 1)) + wf2) +
  plot_annotation(
    title = "GPT Detectors",
    subtitle = "Detect whether Essay was written by <span style='color:blue;'>**AI**</span> or <span style='color:green;'>**Human**</span>",
    caption = cap,
    theme = theme(
      plot.background = element_rect(fill = "black"),
      panel.background = element_rect(fill = "black"),
      plot.title = element_text(family = "Roboto Condensed", colour = "#fbfdc4", face = "bold",
                                size = 45, hjust = 0.5, margin = margin(t = 5, b = 5)),
      plot.subtitle = element_markdown(family = "Ubuntu Condensed", colour = "white",
                                       size = 30, hjust = 0.5, margin = margin(t = 5, b = 5)),
      plot.caption = element_markdown(colour = 'white', hjust = 0.5, size = 14,
                                      family = 'Rosario', margin = margin(t = 20)),
      plot.margin = margin(b = 10, t = 25, r = 20, l = 20)))

ggsave("GPT Detectors.png", height = 10, width = 15, bg = "black")


