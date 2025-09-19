# load packages
pacman::p_load(tidyverse, ggtext, showtext)

# Load Fonts
font_add_google(name = "Inria Sans")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load data
all_recipes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-16/all_recipes.csv')
cuisines <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-16/cuisines.csv')

# Data Wrangling
pasta_df <- all_recipes |> 
  select(name, calories, fat, carbs, protein, avg_rating, total_ratings, reviews) |> 
  filter(str_detect(name, regex("^World's Best Lasagna$|^Baked Ziti$", ignore_case = TRUE))) |> 
  mutate(
    total_ratings = case_when(
      name == "World's Best Lasagna" ~ 20963,
      name == "Baked Ziti" ~ 8924,
      TRUE ~ total_ratings
    ),
    reviews = case_when(
      name == "World's Best Lasagna" ~ 15174,
      name == "Baked Ziti" ~ 6316,
      TRUE ~ reviews
    ),
    calcium = case_when(
      name == "World's Best Lasagna" ~ 442,
      name == "Baked Ziti" ~ 359,
      TRUE ~ NA_real_ 
    )
  )

# Reference Daily Values (FDA, 2,000 kcal diet)
dv_ref <- tribble(
  ~nutrient, ~dv, ~unit,
  "fat", 78, "g",
  "carbs", 275, "g",
  "protein", 50, "g",
  "calcium", 1300, "mg"
)

pasta_df2 <- pasta_df |> 
  pivot_longer(cols = all_of(dv_ref$nutrient), 
               names_to = "nutrient", values_to = "value") |>
  left_join(dv_ref, by = "nutrient") |>
  mutate(dv_perc = round((value / dv) * 100, 0)) |> 
  select(-dv, -unit) |>
  pivot_wider(
    names_from = nutrient,
    values_from = c(value, dv_perc),
    names_glue = "{nutrient}{ifelse(.value=='dv_perc','_dv_perc','')}"
  )

# Fork tines-as-percent
nutrient_order <- c("protein_dv_perc", "calcium_dv_perc", "fat_dv_perc", "carbs_dv_perc")

# pull values that equates to percents ~ tine heights
tine_perc <- pasta_df2 |>
  filter(name == "World's Best Lasagna") |>
  select(all_of(nutrient_order)) |>
  as.numeric()

# Fork neck height
neck_n <- 120 

# Fork Parameters
n_tines <- length(tine_perc)
spacing <- 0.5
tine_w <- 0.2
max_tine_h <- 8
neck_scale <- 0.03

# Fork df
fork_df <- tibble(
  id = seq_len(n_tines),
  perc = tine_perc) |>
  mutate(
    x_center = seq(1, by = spacing, length.out = n_tines),
    xmin = x_center - tine_w/2,
    xmax = x_center + tine_w/2,
    ymax = (perc / 100) * max_tine_h,
    ymin = 0
  )

# Fork neck height
neck_h <- neck_n * neck_scale

# Handle size (tapered)
handle_top_width <- 0.20   # wider at top
handle_bottom_width <- 0.20  # narrower at bottom
handle_h <- 3

# Funnel shape, symmetric around centerline
y_seq <- seq(0, -neck_h, length.out = 100)
bend_factor <- 2.8  

fork_center <- mean(range(fork_df$x_center))  # centerline
funnel_halfwidth_top <- max(fork_df$xmax) - fork_center + 0.5
funnel_halfwidth_bottom <- handle_top_width / 2  # matches handle top

# Smooth easing curve for rounded blend (cosine taper)
ease <- (1 - cos(pi * (abs(y_seq)/neck_h)))/2

funnel_left <- data.frame(
  x = fork_center - (funnel_halfwidth_bottom +
                       (funnel_halfwidth_top - funnel_halfwidth_bottom) * (1 - ease^bend_factor)),
  y = y_seq
)

funnel_right <- data.frame(
  x = fork_center + (funnel_halfwidth_bottom +
                       (funnel_halfwidth_top - funnel_halfwidth_bottom) * (1 - ease^bend_factor)),
  y = y_seq
)

# Fork funnel polygon
funnel_df <- rbind(funnel_left, funnel_right[nrow(funnel_right):1, ])

# Fork handle with taper + rounded bottom
arc_seq <- seq(pi, 2*pi, length.out = 60)   # semi-circle
arc_radius <- handle_bottom_width / 2
arc_bottom <- -neck_h - handle_h   # bottom center y

handle_arc <- data.frame(
  x = fork_center + arc_radius * cos(arc_seq),
  y = arc_bottom + arc_radius * sin(arc_seq)
)

# Fork handle
handle_df <- rbind(
  # top edge (wider)
  data.frame(x = c(fork_center - handle_top_width/2, fork_center + handle_top_width/2),
             y = c(-neck_h, -neck_h)),
  # right taper side
  data.frame(x = fork_center + handle_bottom_width/2, y = arc_bottom),
  # rounded arc bottom
  handle_arc,
  # left taper side
  data.frame(x = fork_center - handle_bottom_width/2, y = arc_bottom),
  # close polygon back to top-left
  data.frame(x = fork_center - handle_top_width/2, y = -neck_h)
)

# Plot
ggplot() +
  # Handle with taper + rounded cap
  geom_polygon(data = handle_df, aes(x = x, y = y),
               fill = "#bfbfbf", color = "#7a7a7a") +
  # Funnel
  geom_polygon(data = funnel_df, aes(x = x, y = y),
               fill = "#c9d6df", color = "#555555") +
  # Tines
  geom_rect(data = fork_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = perc),
            color = "#444444", size = 0.35) +
  # Labels on tines
  geom_text(data = fork_df, aes(x = x_center, y = ymax + 0.5, label = paste0(perc, "%")),
            size = 4.2, fontface = "bold", family = "Inria Sans") +
  scale_fill_gradient(low = "#dcdcdc", high = "#7d7f80", guide = "none") +
  coord_equal(expand = FALSE, ylim = c(-neck_h - handle_h - 1, max(fork_df$ymax) + 1)) +
  theme_void() +
  labs(
    title = "World's Best Lasagna’s Daily Nutrient Boost",
    subtitle = "Based on a 2,000-Calorie reference diet",
    caption = "Graphic: Manasseh Oduor | #TidyTuesday {wk:37 2025} \n Source: https://www.allrecipes.com/") +
  theme(
    plot.title = element_text(family = "Inria Sans", size = 18, hjust = 0.5, face = "bold", margin = margin(t=10,b=5)),
    plot.subtitle = element_text(family = "Inria Sans", size = 12, hjust = 0.5, face = "italic"),
    plot.margin = margin(t=10,b=10,l=10,r=10),
    plot.caption = element_text(size = 12, family = "Inria Sans", hjust = 0.5)
  )

ggsave("top_recipes_Lasagna.png", height = 8, width = 8, bg = "#FFFFF0")