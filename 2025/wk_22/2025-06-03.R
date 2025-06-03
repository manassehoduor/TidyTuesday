# Load packages
pacman::p_load(tidyverse, ggforce, ggtext, showtext)

# Load Fonts
font_add_google(name = "Rosario")
font_add_google(name = "Roboto Condensed")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load Data
gutenberg_languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_languages.csv')

# Generate language pairs from multilingual books
lang_pairs <- gutenberg_languages |>
  group_by(gutenberg_id) |>
  filter(n() > 1) |>
  summarise(pairs = list(combn(language, 2, simplify = FALSE)), .groups = "drop") |>
  unnest(pairs) |>
  mutate(
    lang1 = map_chr(pairs, 1),
    lang2 = map_chr(pairs, 2)) |>
  count(lang1, lang2, sort = TRUE)

# Language full names
lang_names <- tibble(
  language = c("af", "ale", "ang", "ar", "bg", "bo", "br", "ca", "cs", "cy", "da", "de", "el", "en", "eo", "es", "fi",
               "fr", "fy", "ga", "gl", "he", "hu", "is", "it", "ja", "la", "lt", "nl", "no", "pl", "pt", "ro", "ru",
               "sv", "zh", "brx", "bgs", "ceb", "csb", "enm", "grc", "nav", "tl", "nah", "fur", "hai", "kha", "kld",
               "ko", "mi", "myn", "nai", "ilo", "oc"),
  name = c("Afrikaans", "Aleut", "Old English", "Arabic", "Bulgarian", "Tibetan", "Breton", "Catalan", "Czech",
           "Welsh", "Danish", "German", "Greek", "English", "Esperanto", "Spanish", "Finnish", "French", "Frisian",
           "Irish", "Galician", "Hebrew", "Hungarian", "Icelandic", "Italian", "Japanese", "Latin", "Lithuanian",
           "Dutch", "Norwegian", "Polish", "Portuguese", "Romanian", "Russian", "Swedish", "Chinese", "Bodo",
           "Bulgarian (dialect)", "Cebuano", "Kashubian", "Middle English", "Ancient Greek", "Navajo", "Tagalog",
           "Nahuatl", "Friulian", "Haida", "Khasi", "Unknown", "Korean", "Maori", "Mayan", "North American Indian",
           "Ilocano", "Occitan"))

# Language families
lang_family_lookup <- tibble(
  language = c("en", "fr", "de", "es", "la", "it", "pt", "nl", "sv", "da", "ru", "pl", "cs", "cy", "el", "zh", "ja", "he", "ar", "fi", "hu"),
  family   = c("Germanic", "Romance", "Germanic", "Romance", "Classical", "Romance", "Romance", "Germanic", "Germanic", "Germanic",
               "Slavic", "Slavic", "Slavic", "Celtic", "Hellenic", "Sino-Tibetan", "Japonic", "Semitic", "Semitic", "Uralic", "Uralic")
)

# Circular layout for unique languages
langs <- unique(c(lang_pairs$lang1, lang_pairs$lang2)) |> 
  sort()

lang_df <- tibble(language = langs) |>
  left_join(lang_names, by = "language") |>
  arrange(name) |>
  mutate(
    angle = seq(0, 2 * pi, length.out = n() + 1)[-1],
    x = cos(angle),
    y = sin(angle)) |>
  left_join(lang_family_lookup, by = "language") |>
  mutate(
    family = if_else(is.na(family), "Other", family),
    family = factor(family, levels = c(
      "Germanic", "Romance", "Slavic", "Celtic", "Hellenic", "Sino-Tibetan", "Japonic", "Semitic", "Uralic", "Classical", "Other")),
    angle_deg = angle * 180 / pi,
    hjust = ifelse(angle_deg > 90 & angle_deg < 270, 1, 0),
    angle_lab = ifelse(angle_deg > 90 & angle_deg < 270, angle_deg + 180, angle_deg)
  )

# Merge coordinates into language pairs
lang_pairs_coords <- lang_pairs |>
  left_join(lang_df, by = c("lang1" = "language")) |>
  rename(x1 = x, y1 = y, family1 = family) |>
  left_join(lang_df, by = c("lang2" = "language")) |>
  rename(x2 = x, y2 = y, family2 = family)

# Bezier curves for arcs between languages
bezier_df <- lang_pairs_coords |>
  rowwise() |>
  mutate(bezier = list(tibble(
    x = c(x1, 0, x2),
    y = c(y1, 0, y2)
  ))) |>
  unnest(bezier)

# Custom colors for language families
fam_col <- c(
  "Germanic"      = "#FCF259",
  "Romance"       = "#00CFFF",
  "Slavic"        = "#FFD166",
  "Celtic"        = "#B388EB",
  "Hellenic"      = "#66FFB3",
  "Sino-Tibetan"  = "#FFA500",
  "Japonic"       = "#FF69B4",
  "Semitic"       = "#F6C90E",
  "Uralic"        = "#00FA9A",
  "Classical"     = "#A7C7E7",
  "Other"         = "#CCCCCC"
)

# Network Plot
ggplot() +
  geom_bezier(
    data = bezier_df,
    aes(x = x, y = y, group = interaction(lang1, lang2), size = n, color = family1),
    alpha = 0.7) +
  geom_text(
    data = lang_df,
    aes(x = 1.08 * x, y = 1.08 * y, label = name, angle = angle_lab, hjust = hjust),
    size = 2.7, color = "#ffffff", family = "Roboto Condensed") +
  coord_equal(expand = TRUE) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  theme_void() +
  scale_size(range = c(0.2, 2)) +
  scale_color_manual(values = fam_col) +
  labs(
    title = "A Cultural Web of Languages",
    subtitle = "Co-occurrences in Multilingual Gutenberg Books",
    caption = "Graphic: Manasseh Oduor | Source: {gutenbergr} R package! \n #TidyTuesday {wk:22-2025}",
    color = "Language Family",
    size = "No. of Co-occurrences"
  ) +
  guides(
    size = guide_legend(override.aes = list(color = "#FFFFFF", alpha = 1))) +
  theme(
    plot.title = element_text(color = "#FEF9A7", face = "bold", family = "Rosario", hjust = 0.5, size = 12, margin = margin(t=5,b=5)),
    plot.subtitle = element_text(color = "#dddddd", family = "Rosario", hjust = 0.5, size = 9, margin = margin(b = 5)),
    legend.text = element_text(color = "#EFEFEF", family = "Roboto Condensed", size = 8),
    legend.title = element_text(color = "#FEF9A7", family = "Roboto Condensed", size = 9, face = "bold"),
    plot.caption = element_text(size = 8, family = "Rosario", hjust = 0.5, face = "italic", color = "#FFF8D9", margin = margin(t=10)),
    plot.margin = margin(l=10,r=10,b=5)
  )

ggsave(filename = "gutenberg.png", width = 7, height = 7, bg = "#09122C")
