# Load packages
pacman::p_load(tidyverse,  ggtext, showtext, networkD3, htmlwidgets, htmltools, webshot, treemapify)

# Load Fonts
font_add_google(name = "Roboto Condensed")
font_add_google(name = "Roboto Slab")
font_add_google(name = "Rosario")
font_add_google(name = "Averia Gruesa Libre")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load data
parfumo_data_clean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-10/parfumo_data_clean.csv')

# Data wrangling
top_perfumes <- parfumo_data_clean |> 
  drop_na(Rating_Value, Rating_Count) |>
  mutate(Weighted_Score = Rating_Value * log1p(Rating_Count)) |>  # Weight score with log of count
  arrange(desc(Weighted_Score)) |> 
  select(Name, Brand, Release_Year, Rating_Value, Rating_Count, Main_Accords, Weighted_Score) |> 
  slice_head(n = 20)

main_accords <- top_perfumes |> 
  separate_rows(Main_Accords, sep = ", ") |> 
  count(Main_Accords, sort = TRUE) |> 
  mutate(perc = n / 20 * 100) 

treemap_df <- top_perfumes |>
  select(Name, Main_Accords) |>
  separate_rows(Main_Accords, sep = ", ") |> 
  count(Main_Accords, Name) |> 
  rename(Accord = Main_Accords)

nodes <- top_perfumes |>
  select(Name, Main_Accords) |>
  separate_rows(Main_Accords, sep = ", ") |>
  distinct() |>
  pivot_longer(cols = everything(), names_to = "type", values_to = "label") |>
  distinct(label) |>
  mutate(id = row_number() - 1)  # Assign node IDs starting from 0

links <- top_perfumes |>
  select(Name, Main_Accords) |>
  separate_rows(Main_Accords, sep = ", ") |>
  left_join(nodes, by = c("Name" = "label")) |>
  rename(source = id) |>
  left_join(nodes, by = c("Main_Accords" = "label")) |>
  rename(target = id) |>
  group_by(source, target) |> 
  summarize(value = n(), .groups = "drop")

# Sankey diagram
sn <- sankeyNetwork(
  Links = links, Nodes = nodes, 
  Source = "source", Target = "target", Value = "value", NodeID = "label", units = "Count",  nodePadding = 20, nodeWidth = 0,
  fontFamily = "Roboto Condensed", fontSize = 10
)

saveWidget(sn, "perfume_sankey.html")

# Save as PNG with higher resolution
webshot(
  "perfume_sankey.html",
  file = "perfume_sankey.png",
  vwidth = 1000, vheight = 600, zoom = 4)

# Hierarchical treemap
ggplot(treemap_df, aes(area = n, fill = Accord, label = Name, subgroup = Accord)) +
  geom_treemap() +
  geom_treemap_subgroup_border(color = "#000", size = 1) +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE, reflow = TRUE, alpha = 0.9, fontface = "bold", color = "#000", family = "Averia Gruesa Libre") +  # Label accords
  geom_treemap_text(place = "topleft", grow = FALSE, reflow = TRUE, alpha = 0.7, color = "#fff", fontface = "italic", size = 12, family = "Roboto Slab") +  # Label perfume names
  scale_fill_viridis_d() +
  labs(
    title = "Fragrance Profiles of the Top-Rated 20 Perfumes",
    subtitle = "75% of the Top 20 Perfumes feature Spicy scents, followed by Woody (60%) and Sweet (55%)",
    caption = "Graphic: Manasseh Oduor \n Source: olgagmiufana1 (kaggle) via https://www.parfumo.com/ \n #TidyTuesday {wk:50-2024}") +
  theme_minimal() +
  theme(
    text = element_text(family = "Roboto Slab"),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5, margin = margin(t=10, b=10)),
    plot.subtitle = element_text(hjust = 0.1, color = "#2E0249", size = 15, family = "Roboto Slab", margin = margin(t=5, b=10)),
    plot.caption = element_text(family = "Rosario", color = "#091057", size = 14, hjust = 1, margin = margin(t=10)),
    legend.position = "none",
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(t = 10, b = 20, l = 40, r = 40)
  )

# Save plot
ggsave("perfume.png", width = 12, height = 12, bg = "#FFCFEF")
