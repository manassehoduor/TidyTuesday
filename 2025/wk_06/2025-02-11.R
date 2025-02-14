# Load libraries
pacman::p_load(tidyverse, igraph, ggraph, ggtext, showtext)

# Load Fonts
font_add_google(name = "Rosario")
font_add_google(name = "Averia Sans Libre")
font_add_google(name = "Averia Gruesa Libre")
font_add_google(name = "Averia Serif Libre")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load data
cdc_datasets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-11/cdc_datasets.csv')
fpi_codes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-11/fpi_codes.csv')
omb_codes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-11/omb_codes.csv')

# Data wrangling
cdc_datasets_df <- cdc_datasets |>
  mutate(category = case_when(
    category %in% c("NCHS", "National Center for Health Statistics", "Health Statistics") ~ "NCHS",
    TRUE ~ category
  ))

# Subset NCHS theme category
nchs_tags <- cdc_datasets_df |>
  filter(category == "NCHS" & !is.na(tags) & tags != "This dataset does not have any tags") |>
  separate_rows(tags, sep = ",") |>
  mutate(tags = str_trim(tags) |> str_to_lower() |> str_remove_all('["]')) |>
  count(tags, sort = TRUE)

# Generate Co-occurrence Tag Pairs
nchs_tags_clean <- cdc_datasets_df |>
  filter(category == "NCHS" & !is.na(tags) & tags != "This dataset does not have any tags") |>
  separate_rows(tags, sep = ",") |>
  mutate(tags = str_trim(tags) |> str_to_lower() |> str_remove_all('["]'))

nchs_tag_pairs <- nchs_tags_clean |>
  group_by(dataset_url) |>
  summarise(tags_list = list(unique(tags)), .groups = "drop") |>
  mutate(tag_combinations = map(tags_list, ~ combn(.x, 2, simplify = FALSE))) |>
  select(dataset_url, tag_combinations) |>
  unnest(tag_combinations) |>
  unnest_wider(tag_combinations, names_sep = "_") |>
  count(tag_combinations_1, tag_combinations_2, sort = TRUE) |>
  rename(tags.x = tag_combinations_1, tags.y = tag_combinations_2) |> 
  filter(n > 19)

# Bar plot
nchs_tags |> 
  top_n(17) |> 
  mutate(bar_col = ifelse(n == max(n), "#88AB8E", "#C62300")) |>
  ggplot(aes(reorder(tags, n), n, , fill = bar_col)) +
  geom_col() +
  scale_fill_identity() +
  geom_text(aes(label = n), hjust = 1.5, size = 5, color = "#F9F6E6", family = "Averia Gruesa Libre") +
  geom_text(aes(y = 0.01 * max(n), label = tags), hjust = 0,, size = 5, color = "#F9F6E6", family = "Averia Gruesa Libre") +
  coord_flip() +
  theme_void() +
  labs(
    title = "Top Keywords in CDC Datasets",
    subtitle = "All from the National Center for Health Statistics (NCHS) Category") +
  theme(
    axis.title = element_blank(),
    plot.title = element_text(family = "Averia Serif Libre", colour = "#FFFBF5", face = "bold", size = 18, hjust = 0.5, margin = margin(t = 10, b = 10)),
    plot.subtitle = element_text(family = "Averia Serif Libre", colour = "#FFFBF5", face = "bold", size = 14, hjust = 0.15, margin = margin(t = 2, b = 20)),
    plot.margin = margin(b=10, t=20, r=20, l=20))

ggsave("CDC Datasets_tags.png", width = 10, height = 10, bg = "#2A004E")

# Network plot
nchs_tag_graph <- graph_from_data_frame(nchs_tag_pairs, directed = FALSE)

ggraph(nchs_tag_graph, layout = "stress") +
  geom_edge_link(aes(width = n), edge_alpha = 0.6, show.legend = FALSE, color = "#88AB8E") +
  geom_node_point(size = degree(nchs_tag_graph), color = "#C62300") +
  geom_node_text(aes(label = name), repel = TRUE, nudge_y = 0.1, nudge_x = 0.035, max.overlaps = 100, size = 4, color = "#F9F6E6", family = "Averia Gruesa Libre") +
  theme_void() +
  labs(title = "Top Tags Co-occurrence Network",
       tag = "Graphic: Manasseh Oduor | Source: archive.org \n #TidyTuesday {wk:6-2025}") +
  theme(
    text = element_text(family = "Averia Sans Libre", colour = "#F5EFFF"),
    plot.tag = element_text(size = 10, family = "Rosario", color = "#F3FDE8"),
    plot.tag.position = c(0.7, 0.05),
    plot.title = element_text(family = "Averia Serif Libre", colour = "#FFFBF5", face = "bold", size = 18, hjust = 0.5, margin = margin(t = 10, b = 10)),
    plot.margin = margin(b=10, t=20, r=20, l=20))

ggsave("CDC Datasets.png", width = 11, height = 8, bg = "#2A004E")