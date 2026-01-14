# Load packages
pacman::p_load(tidyverse, countrycode, ggtext, showtext, patchwork, ggimage, scales, grid)

# Load fonts
font_add_google("Urbanist")
font_add_google("Monoton")
font_add(family = "Tanker", regular = "Tanker-Regular.ttf")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 350)

# Load data
africa <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-13/africa.csv')

# Data Wrangling
top20_africa = africa |> group_by(language) |> summarise(speakers = sum(native_speakers)) |> arrange(desc(speakers)) |> top_n(20)

top20_africa_df <- top20_africa |>
  mutate(language = ifelse(str_detect(language, "Tsonga"), "Tsonga", language),
         language = str_squish(language)) |> 
  mutate(
    country = case_when(
    language == "Arabic"           ~ "Egypt",
    language == "Fulani"           ~ "Guinea",
    language == "Oromo"            ~ "Ethiopia",
    language == "Hausa"            ~ "Nigeria",
    language == "Yoruba"           ~ "Nigeria",
    language == "Somali"           ~ "Somalia",
    language == "Mooré"            ~ "Burkina Faso",
    language == "Portuguese"       ~ "Portugal",
    language == "Lingala"          ~ "Congo - Kinshasa",
    language == "Kanuri"           ~ "Nigeria",
    language == "Xhosa"            ~ "South Africa",
    language == "Amharic"          ~ "Ethiopia",
    language == "Tswana"           ~ "Botswana",
    language == "Berber"           ~ "Morocco",
    language == "Igbo"             ~ "Nigeria",
    language == "Soninke"          ~ "Senegal",
    language == "Swahili"          ~ "Tanzania",
    language == "Kituba"           ~ "Congo - Brazzaville",
    str_detect(language, "Tsonga") ~ "South Africa",
    language == "Zarma"            ~ "Niger",
    TRUE                           ~ NA
  )) |>
  mutate(iso = tolower(countrycode(country, "country.name", "iso2c"))) |>
  arrange(desc(speakers)) |>
  mutate(
    lab_r = label_number(accuracy = 0.1, scale_cut = cut_short_scale())(speakers),
    lab_s = paste0("**", language, "**<br><span style='color:#777777; font-size:12pt;'>", lab_r, "</span>"),
    flag_url = paste0("https://flagpedia.net/data/flags/w580/", iso, ".png")
  )

radbg <- radialGradient(
  colours = c("#FFD41D", "#FCF8F8"),
  cx1 = unit(0.1, "npc"), cy1 = unit(0.5, "npc"), r1 = unit(0, "npc"),
  cx2 = unit(0.1, "npc"), cy2 = unit(0.5, "npc"), r2 = unit(0.9, "npc")
)

# Plot
make_column <- function(df) {
  ggplot(df, aes(x = 1, y = reorder(lab_s, speakers))) +
    geom_image(aes(image = flag_url), x = 0.8, size = 0.12, asp = 0.75) +
    geom_richtext(aes(label = lab_s), hjust = 0, size = 6, fill = NA, label.color = NA, label.padding = unit(c(0, 0, 0, 1.8), "lines"), family = "Urbanist") +
    xlim(0.7, 1.8) +
    theme_void() +
    theme(plot.margin = margin(10, 20, 10, 80))
  }

# Patchwork
p1 <- make_column(top20_africa_df[1:10, ])
p2 <- make_column(top20_africa_df[11:20, ])

p3 <- p1 + p2 + 
  plot_annotation(
    title = 'LINGUISTICS',
    subtitle = 'Languages in <span style="color:#D81B60;">Africa</span>',
    caption = 'Most spoken languages \n flag = largest native speakers: N = all native speakers within Africa \n Graphic: Manasseh Oduor | Source: Wikipedia',
    theme = theme(
      plot.background=element_rect(fill = radbg, colour=NA),
      text = element_text(family = "Tanker"),
      plot.title = element_text(hjust = 0.4, size = 20, face = "bold", family = "Monoton", color = "grey50", margin = margin(t=30)),
      plot.subtitle = element_markdown(hjust = 0.4, size = 40, face = "bold", margin = margin(t=10, b=5)),
      plot.caption = element_text(hjust = 0.4, family = "Urbanist", size = 12, color = "grey40", margin = margin(b=30))
    )
  )

# Save
ggsave("languages_africa.png", p3, width = 10, height = 12)