# load packages
pacman::p_load(tidyverse, jsonlite, purrr, sf, ggtext, showtext)

# Load Fonts
font_add_google(name = "Nunito Sans")
font_add_google(name = "Antic")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load data
country_lists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-09/country_lists.csv')
rank_by_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-09/rank_by_year.csv')

# Data Wrangling
africa_names <- c(
  "Algeria","Angola","Benin","Botswana","Burkina Faso","Burundi","Cape Verde Islands",
  "Cameroon","Central African Republic","Chad","Comoro Islands","Congo (Rep.)","Congo (Dem. Rep.)",
  "Cote d'Ivoire","Djibouti","Egypt","Equatorial Guinea","Eritrea","eSwatini","Ethiopia",
  "Gabon","The Gambia","Ghana","Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia","Libya",
  "Madagascar","Malawi","Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia",
  "Niger","Nigeria","Rwanda","Sao Tome and Principe","Senegal","Seychelles","Sierra Leone",
  "Somalia","South Africa","South Sudan","Sudan","Tanzania","Togo","Tunisia","Uganda",
  "Zambia","Zimbabwe","Western Sahara"
)

# Parser that outputs names only
parse_json_names <- function(js) {
  if (is.na(js) || js == "") return(character(0))
  s <- trimws(js)
  
  attempts <- list(
    s,
    gsub("\\\\", "", s),
    gsub("'", '"', s),
    gsub("\\\\'", "'", s)
  )
  
  parsed <- NULL
  for (st in attempts) {
    parsed <- tryCatch(fromJSON(st, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(parsed)) break
  }
  if (is.null(parsed)) return(character(0))
  
  while (is.list(parsed) && length(parsed) == 1 && (is.list(parsed[[1]]) || is.data.frame(parsed[[1]]))) {
    parsed <- parsed[[1]]
  }
  
  # Extract names directly
  names_out <- if (is.data.frame(parsed)) {
    if ("name" %in% names(parsed)) as.character(parsed$name) else character(0)
  } else {
    unlist(map(parsed, function(x) {
      if (is.list(x) && !is.null(x$name)) return(as.character(x$name))
      if (is.character(x)) return(x)
      return(NULL)
    }))
  }
  
  unique(na.omit(names_out))
}

# Apply parsing and compute intra-African visa-free lists/counts
country_lists_df <- country_lists |>
  mutate(
    visa_free_names = map(visa_free_access, parse_json_names),
    visa_free_africa = map(visa_free_names, ~ intersect(.x, africa_names)),
    visa_free_africa_count = map_int(visa_free_africa, length)
  )

# Keep only African passports and sort by count
africa_passports <- country_lists_df |>
  filter(country %in% africa_names) |>
  arrange(desc(visa_free_africa_count)) |>
  select(country, visa_free_africa_count, visa_free_africa)

# Kenya
kenya_access <- africa_passports |>
  filter(country == "Kenya") |>
  select(country, visa_free_africa_count, visa_free_africa)

# Kenya's visa-free African countries
unlist(kenya_access$visa_free_africa)

# Checks
raw_json <- country_lists |>
  filter(code == "SC") |>
  pull(visa_free_access)

parsed <- fromJSON(raw_json)

# Top 8 passports
top8 <- africa_passports |>
  slice_max(visa_free_africa_count, n = 8)

# Name harmonization
name_map <- c(
  "Cape Verde Islands" = "Cabo Verde",
  "Congo (Rep.)" = "Republic of the Congo",
  "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
  "Comoro Islands" = "Comoros",
  "Cote d'Ivoire" = "Ivory Coast",
  "Sao Tome and Principe" = "São Tomé and Principe",
  "The Gambia" = "Gambia",
  "Tanzania" = "United Republic of Tanzania"
)

fix_name <- function(x) {
  ifelse(x %in% names(name_map), name_map[x], x)
}

# Map for one passport
map_passport_access <- function(passport_country) {
  visa_free_list <- africa_passports 
    filter(country == passport_country) |>
    pull(visa_free_africa) |>
    .[[1]] |> fix_name()
  
  passport_admin <- fix_name(passport_country)
  
  africa_map |>
    mutate(
      status = case_when(
        admin == passport_admin ~ paste0("passport_country_", passport_country),
        admin %in% visa_free_list ~ "visa_free",
        TRUE ~ "no_access"
      ),
      passport = passport_country
    )
}

# data for top 8
maps_list <- map_df(top8$country, map_passport_access)

maps_list <- maps_list |>
  left_join(top8 |> select(country, visa_free_africa_count),
            by = c("passport" = "country")) |>
  mutate(passport_label = paste0(passport, " (", visa_free_africa_count, ")"),
         passport_label = factor(passport_label,
                                 levels = paste0(top8$country, " (", top8$visa_free_africa_count, ")")))

# Build color scale
passport_colors <- setNames(
  RColorBrewer::brewer.pal(8, "Dark2"),
  paste0("passport_country_", top8$country)
)

all_colors <- c(passport_colors,
                "visa_free" = "skyblue",
                "no_access" = "white")

# Plot
ggplot(maps_list) +
  geom_sf(aes(fill = status, group = admin), color = "black", size = 0.05) +
  scale_fill_manual(values = all_colors, guide = "none") +
  facet_wrap(~ passport_label, ncol = 4) +
  labs(
    title = "Visa-Free Kings of Africa",
    subtitle = "The 8 Strongest African Passports for Traveling Within Africa"
  ) +
  theme_void() +
  theme(
    strip.text = element_text(size = 13, family = "Nunito Sans", color = "black"),
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", color = "black", family = "Nunito Sans", margin = margin(t=10,b=10)),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray20", family = "Antic", margin = margin(b=20)),
    plot.margin = margin(t=10,b=10,l=10, r=10)
    )

ggsave("passport.png", height = 8, width = 14, bg = "white")