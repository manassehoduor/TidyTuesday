# Load data
fide_ratings_august <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_august.csv')
fide_ratings_september <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_september.csv')

# load packages
pacman::p_load(tidyverse, ggbeeswarm, ggrepel, ggtext, showtext)

# Load Fonts
font_add_google(name = "Rambla")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Data wrangling
aug_rank <- fide_ratings_august |>
  arrange(desc(rating)) |>
  distinct(id, .keep_all = TRUE) |>
  mutate(rank_aug = row_number()) |>
  select(id, name, rating_aug = rating, rank_aug)

sep_rank <- fide_ratings_september |>
  arrange(desc(rating)) |>
  distinct(id, .keep_all = TRUE) |>
  mutate(rank_sep = row_number()) |>
  select(id, name, rating_sep = rating, rank_sep)

# merge top 50
rank_compare <- aug_rank |>
  inner_join(sep_rank |> select(-name), by = "id") |>
  filter(rank_aug <= 50 | rank_sep <= 50)

# filter top 10 in either month
top10_ids <- rank_compare |>
  filter(rank_aug <= 10 | rank_sep <= 10) |>
  pull(id)

rank_long <- rank_compare |>
  pivot_longer(
    cols = c(rank_aug, rank_sep),
    names_to = "month", values_to = "rank"
  ) |>
  mutate(
    month = recode(month,
                   "rank_aug" = "August",
                   "rank_sep" = "September"),
    top10 = id %in% top10_ids
  )

# rank movement
rank_compare <- rank_compare |>
  mutate(
    label_both = paste0(name, " (", rank_aug, " to", " ", rank_sep, ")"),
    move = case_when(
      rank_sep < rank_aug ~ "improved",
      rank_sep > rank_aug ~ "dropped",
      TRUE ~ "same"
    )
  )

# labels
label_data <- rank_long |>
  filter(top10 & month == "September") |>
  left_join(rank_compare |> select(id, label_both, move), by = "id")

rank_long <- rank_long |>
  left_join(rank_compare |> select(id, move), by = "id")

# Plot
ggplot(rank_long, aes(x = month, y = rank, group = id)) +
  geom_line(data = filter(rank_long, !top10),
            alpha = 0.3, color = "grey60") +
  geom_line(data = filter(rank_long, top10),
            aes(color = move), linewidth = 0.5, alpha = 0.7) +
  geom_beeswarm(aes(color = top10, size = top10), alpha = 1) +
  geom_text_repel(
    data = label_data,
    aes(x = month, y = rank, label = label_both, color = move),
    nudge_x = 0.3, hjust = 0, size = 4,
    segment.color = "grey60", segment.size = 0.3, family = "Rambla",
    max.overlaps = Inf
  ) +
  scale_y_reverse(breaks = seq(0, 55, 5)) +
  scale_color_manual(
    values = c(
      "improved" = "green",
      "dropped"  = "red",
      "same"     = "black",
      "FALSE"    = "grey60",
      "TRUE"     = "blue"
    )
  ) +
  scale_size_manual(values = c("FALSE" = 1.5, "TRUE" = 2.5)) +
  expand_limits(x = c(1, 2.5)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "Rambla", size = 20),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, family = "Rambla", hjust = 0.5),
    legend.position = "none") +
  labs(
    title = "Top 50 FIDE Rankings",
      y = "World Rank", x = ""
  )

ggsave("chess.png", height = 14, width = 16, bg = "#fff")
