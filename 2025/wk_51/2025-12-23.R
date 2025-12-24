# Load libraries
pacman::p_load(tidyverse, geomtextpath, shadowtext, showtext, ggtext, ggfx, grid)

# Load fonts
font_add_google("Urbanist")
font_add_google("Jura")
showtext_auto(enable = TRUE)
showtext_opts(dpi=350)

# Load data
endangered_status <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-23/endangered_status.csv')
families <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-23/families.csv')
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-23/languages.csv')

# Data wrangling
KE <- languages |> filter(grepl("KE", countries))

sel_df <- endangered_status |> select(id, status_label)
KE_df <- KE |> left_join(sel_df, by = "id")

families_df <- families |> rename(family_id = id)
KE_dfs <- KE_df |> left_join(families_df, by = "family_id") |> 
  arrange(family)

n_items <- nrow(KE_dfs)

KE_dfs2 <- KE_dfs |>
  mutate(
    id_n = row_number(),
    angle_deg = 90 - 360 * (id_n - 0.5) / n_items,
    hjust = ifelse(angle_deg < -90, 0, 1), 
    text_angle = ifelse(angle_deg < -90, angle_deg + 180, angle_deg))

wave_points <- data.frame(x = seq(0.5, n_items + 0.5, length.out = 2000)) |>
  mutate(y = 4 + 0.15 * sin(x * 2 * pi * 0.5))

partitions <- KE_dfs2 |>
  group_by(family) |>
  summarize(end_id = max(id_n)) |>
  mutate(x_pos = end_id + 0.5)

KE_dfs2 <- KE_dfs2 |>
  group_by(family) |>
  mutate(
    arc_count = n(),
    dynamic_size = ifelse(arc_count < 5, 3.2, 
                          ifelse(arc_count == 13, 6,
                                 ifelse(arc_count == 20, 8,
                                        ifelse(arc_count == 43, 10))))) |>
  ungroup()

radbg <- radialGradient(
  colours = grey(c(0.6, 0.99)), 
  cx1 = unit(0.5, "npc"), cy1 = unit(0.5, "npc"), r1 = unit(0, "npc"), cx2 = unit(0.5, "npc"), cy2 = unit(0.5, "npc"), r2 = unit(0.6, "npc"))

# Plot
ggplot(KE_dfs2) +
  geom_rect(aes(xmin = id_n - 0.5, xmax = id_n + 0.5, ymin = 1, ymax = 4, fill = status_label), alpha = 0.5, color = NA) +
  geom_hline(yintercept = 1, color = "black", size = 0.5) +
  geom_line(data = wave_points, aes(x = x, y = y), color = "black", size = 0.4) +
  #geom_segment(aes(x = id_n - 0.5, xend = id_n - 0.5, y = 1, yend = 4), color = "black", size = 0.1) +
  with_outer_glow(geom_segment(data = partitions, aes(x = x_pos, xend = x_pos, y = 1, yend = 4.3), color = "black", size = 0.4), colour = "yellow", sigma = 5, expand = 1) +
  geom_shadowtext(aes(x = id_n, y = 3.8, label = name, angle = text_angle, hjust = hjust), size = 4, color = "black", bg.color = "white", bg.r = 0.1, family = "Urbanist") +
  geom_textpath(aes(x = id_n, y = 4.6, label = family, group = family), size = KE_dfs2$dynamic_size, fontface = "bold", vjust = -0.2, lineheight = 0.9, text_only = TRUE, family = "serif") +
  annotate("text", x = 0.5, y = -0.2, label = "Kenyan\nLanguages\nand Families", size = 5, family = "Urbanist", fontface = "italic", lineheight = 1) +
  scale_size_identity() +
  coord_polar(start = 0) +
  scale_y_continuous(limits = c(-0.2, 4.7)) + 
  scale_x_continuous(limits = c(0.5, n_items + 0.5)) +
  scale_fill_manual(values = c("extinct" = "#FCF8F8", "nearly extinct" = "#FBEFEF", "not endangered" = "#FAD7A0", "threatened"  = "#D2B4DE", "shifting"  = "#AED6F1", "moribund" = "purple")) +
  theme_void() +
  labs(
    title = "Genealogical Classification and Vitality Status of Kenyan Languages",
    caption = "Graphic: Manasseh Oduor | Source: Glottolog | #TidyTuesday {wk:51 2025}") +
  theme(
    plot.title = element_text(family = "Jura", size = 15, face = "bold", hjust = 0.5, colour = "black"),
    plot.caption = element_text(family = "Urbanist", size = 14, hjust = 0.5, colour = "black"),
    plot.background=element_rect(fill = radbg, colour=NA),
    panel.background = element_blank(),
    legend.position = "none",
    plot.tag.position = c(0.3, 0.1),
    plot.margin = margin(t=10, b=20))

ggsave("ke_languages.png", height = 10, width = 10, dpi = 350)