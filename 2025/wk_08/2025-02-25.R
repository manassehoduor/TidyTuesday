# Load libraries
pacman::p_load(tidyverse, ggdist, ggtext, showtext)

# Load Fonts
font_add_google(name = "Rosario")
font_add_google(name = "Roboto Condensed")
font_add_google(name = "Averia Sans Libre")
font_add_google(name = "Averia Serif Libre")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load data
article_dat <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-25/article_dat.csv')
model_dat <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-25/model_dat.csv')

# Data wrangling
model_dat_df <- model_dat |> 
  select(measure, ref, compare, point, lower, upper) |> 
  filter(measure == "OR" & ref == "White" & if_all(c(point, lower, upper), ~ . != -99) & if_all(c(point, lower, upper), ~ . <= 7)) |> 
  drop_na() |> 
  mutate(
    major_compare = case_when(
      str_detect(compare, regex("Non-Hispanic White", ignore_case = TRUE)) ~ "White",
      str_detect(compare, regex("\\bBlack\\b|African[- ]?American", ignore_case = TRUE)) ~ "Black or African American",
      str_detect(compare, regex("Hispanic|Latino|Latina", ignore_case = TRUE)) ~ "Hispanic or Latino",
      str_detect(compare, regex("Asian|Chinese", ignore_case = TRUE)) ~ "Asian",
      str_detect(compare, regex("Native American|American Indian|Alaska Native|Alaskan Native", ignore_case = TRUE)) ~ "Native American or Alaska Native",
      str_detect(compare, regex("Pacific Islander|Native Hawaiian", ignore_case = TRUE)) ~ "Native Hawaiian or Pacific Islander",
      str_detect(compare, regex("Non-White|nonwhite|Nonwhite race", ignore_case = TRUE)) ~ "Non-White",
      str_detect(compare, regex("Other|Mixed|None of the above|Another race", ignore_case = TRUE)) ~ "Other or Mixed",
      str_detect(compare, regex("Unknown|Missing", ignore_case = TRUE)) ~ "Unknown/Missing",
      TRUE ~ "Other or Mixed"
    )
  ) |> 
  filter(!major_compare == "White")

nn = model_dat_df |> 
  group_by(major_compare) |> 
  summarise(n=n())

model_dat_df <- model_dat_df |> 
  left_join(nn, by = "major_compare")

cus <- c("Unknown/Missing", "Other or Mixed", "Non-White", "Native Hawaiian or Pacific Islander",
         "Native American or Alaska Native", "Asian", "Hispanic or Latino", "Black or African American")

model_dat_df$major_compare <- factor(model_dat_df$major_compare, levels = cus)

# Plot
ggplot(model_dat_df, aes(y = major_compare, x = point)) +
  stat_halfeye(fill_type = "segments", alpha = 0.6, stroke = 1.5, fill = "#D3E671", .width = c(0.5, 0.95)) +  
  stat_summary(geom = "point", fun = median) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +  # OR = 1 reference line
  geom_text(aes(label = paste0("(n=", n, ")"), x = min(lower) - 0.1),
            hjust = 1, size = 8, color = "#003092", family = "Averia Serif Libre") +
  scale_x_continuous(limits = c(-1,7), breaks = c(0, 2, 4, 6), labels = c("0", "2", "4", "6")) +
  annotate("text", x = 1.05, y = max(as.numeric(reorder(model_dat_df$major_compare, -model_dat_df$n))) + 0.8,  
           label = "OR = 1 (No Disparity), Reference: White", color = "#3F4F44", size = 6, fontface = "bold", hjust = 0, family = "Averia Sans Libre") +
  labs(x = "Odds Ratio (OR)", 
       y = "Race/Ethnicity", 
       title = "Who Faces Barriers in Reproductive Medicine in the US?",
       caption = "Graphic: Manasseh Oduor | Source: www.ajog.org/article/S0002-9378(24)00775-0/ \n #TidyTuesday {wk:8-2025}") +
  theme_minimal() +
  theme(
    text = element_text(family = "Rosario", colour = "black"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 25, face = "bold", colour = "black", margin = margin(t=15, b=15)),
    axis.text.x = element_text(family = "Averia Sans Libre", size = 18, colour = "black", margin = margin(t=10, b=10)),
    axis.text.y = element_text(family = "Roboto Condensed", size = 24, colour = "black", hjust = 1, margin = margin(r = -60)),
    legend.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_line(colour = "grey"),
    panel.grid.major.x = element_line(colour = "#F5ECD5"),
    plot.title = element_text(family = "Rosario", hjust = 0, size = 35, colour = "black", face = "bold", margin = margin(t=20,b=70)),
    plot.caption = element_text(size = 20, family = "Rosario", hjust = 0.5, face = "italic", color = "black", margin = margin(t=10, b=10)),
    plot.margin = margin(t=15,b=15,r=0,l=20))

ggsave("disparities.png", height = 18, width = 18, bg = "#E8F9FF")