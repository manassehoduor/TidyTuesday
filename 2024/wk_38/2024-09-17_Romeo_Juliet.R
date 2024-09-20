# load libraries
pacman::p_load(tidyverse, igraph, ggraph, viridis, ggtext, showtext)

# Font
font_add_google(name = "Roboto Condensed")
font_add_google(name = "Dangrek")
font_add_google(name = "Rosario")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# load data
tuesdata <- tidytuesdayR::tt_load(2024, week = 38)
romeo_juliet <- tuesdata$romeo_juliet

# Data wrangling
# character interactions
romeo_juliet_df <- romeo_juliet  |> 
  filter(!str_detect(character, "\\[stage direction\\]"))

int_df <- romeo_juliet_df  |>
  group_by(act, scene) |> 
  summarize(characters = list(unique(character)), .groups = 'drop') 

# character-interaction pairs within each scene
int_pairs <- int_df |> 
  rowwise() |> 
  filter(length(characters) > 1) |>
  mutate(pairs = list(combn(characters, 2, simplify = FALSE))) |>
  unnest(pairs) |>
  unnest_wider(pairs, names_sep = "_")

int_n <- int_pairs |> 
  count(pairs_1, pairs_2, name = "interaction_count")

# igraph object
igr_ob <- graph_from_data_frame(int_n, directed = FALSE)
  
# Network plot
ggraph(igr_ob, layout = "graphopt") + 
    geom_edge_link(aes(width = interaction_count), edge_alpha = 0.6, color = "grey") +
    geom_node_point(aes(size = degree(igr_ob)), show.legend = FALSE, color = "white") +
    geom_node_text(aes(label = name), repel = TRUE, size = 4, color = "#FFEB55", max.overlaps = 100, family = "Roboto Condensed") +
    #scale_size(range = c(3, 10)) +
    theme_void() +
    labs(title = "Character Interaction Network in Shakespeare's Romeo and Juliet",
         size = "Degree of Interaction",
         edge_width = "Shared Scenes",
         tag = "Graphic: Manasseh Oduor \n Source: shakespeare.mit.edu \n (via github.com/nrennie/shakespeare)") +
  theme(
    text = element_text(family = "Roboto Condensed", colour = "#F5EFFF"),
    plot.tag = element_text(size = 10, family = "Rosario", color = "#F3FDE8"),
    plot.tag.position = c(0.9, 0.1),
    legend.position = "bottom",
    legend.title.position = "top",
    plot.title = element_text(family = "Dangrek", colour = "#FFFBF5", face = "bold",
                              size = 18, hjust = 0.2, margin = margin(t = 10, b = 10)),
    plot.margin = margin(b=10, t=20, r=20, l=20))

# Save plot
ggsave("Romeo & Juliet.png", width = 10, height = 8, bg = "#31363F")

