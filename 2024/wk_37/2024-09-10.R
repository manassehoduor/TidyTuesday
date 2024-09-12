# Load libraries
pacman::p_load(tidyverse, forcats, ggrepel, ggtext, showtext)

# Font
font_add_google(name = "Roboto Condensed")
font_add_google(name = "Ubuntu Condensed")
font_add_google(name = "Dangrek")
font_add_google(name = "Rosario")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load data
tuesdata <- tidytuesdayR::tt_load(2024, week = 37)
college_admissions <- tuesdata$college_admissions

# Data Wrangle
df1 <- college_admissions |> 
  select(par_income_lab, name, tier, test_band_tier, attend, rel_attend) |> 
  filter(par_income_lab == "Top 1")

# Function to identify outliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

# Specify the desired order of the tiers
tier_order <- c("Ivy Plus", 
                "Other elite schools (public and private)", 
                "Highly selective private", 
                "Highly selective public", 
                "Selective private", 
                "Selective public")

df2 <- df1 |> 
  select(tier, attend, name) |> 
  group_by(tier) |> 
  mutate(is_outlier = is_outlier(attend),
         tier = factor(tier, levels = tier_order))

# Plot
ggplot(df2, aes(x = tier, y = attend, fill = tier)) +
  geom_violin(trim = FALSE, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", color = "black", alpha = 0.7, outlier.shape = NA) +
  geom_point(data = subset(df2, is_outlier), aes(color = tier), size = 2, shape = 20, fill = "black", color = "black") +
  geom_text_repel(data = subset(df2, is_outlier), aes(label = name), family = "Roboto Condensed", size = 3,
                  box.padding = 0.5, point.padding = 0.5, force = 2, max.overlaps = 20) +
  stat_summary(fun = "median", geom = "text", aes(label = round(after_stat(y), 4)), 
               vjust = -2, hjust = 0.5, color = "black", size = 3, angle = 90, family = "Roboto Condensed") +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(title = "The Impact of School Selectivity on College Attendance Patterns in US",
       subtitle = "The Role of Parental Wealth in Shaping College Attendance from the Top 1% Income Families",
       tag = "Graphic: Manasseh Oduor | Source: Opportunity Insights | #TidyTuesday {wk:37 2024}",
       x = "Tier",
       y = "Attendance (Test-score-reweighted absolute attendance rate)") +
  #coord_cartesian(ylim = c(-0.01, 0.045)) +
  scale_x_discrete(position = "top", labels = c(
    "Ivy Plus" = "Ivy Plus",
    "Other elite schools (public and private)" = "Other elite schools\n(public and private)",
    "Highly selective private" = "Highly selective private",
    "Highly selective public" = "Highly selective public",
    "Selective private" = "Selective private",
    "Selective public" = "Selective public"
  )) +
  cowplot::theme_minimal_grid(9.5, line_size = 0.3) +
  theme(
    text = element_text(family = "Roboto Condensed"),
    axis.title.x = element_text(size = 16, color = "white", face = "bold", family = "Roboto Condensed"),
    axis.title.y = element_text(size = 12, color = "#435055", face = "bold", family = "Roboto Condensed"),
    axis.text.y = element_text(hjust=0, color="#1A3636", size = 11),
    axis.text.x = element_text(color="#362FD9", size = 11),
    strip.text = element_text(size = 16, color = "#435055", family = "Roboto Condensed"),
    plot.background = element_rect(fill="#FDFAF6"),
    panel.background = element_rect(fill = "#F9F9F9"),
    panel.grid.major.y = element_line(linetype = "dotted"),
    plot.title = element_text(family = "Dangrek", colour = "#2C3333", face = "bold",
                              size = 20, hjust = 0, margin = margin(t = 5, b = 5)),
    plot.subtitle = element_text(family = "Rosario", colour = "black",
                                     size = 12, hjust = 0, margin = margin(t = 5, b = 10)),
    plot.tag = element_text(size = 10, family = "Rosario", color = "#697565", margin = margin(b = 20, t = 10)),
    plot.tag.position = c(0.05, 0.9),
    legend.position = "none") 

ggsave("College Admissions.png", width = 14, height = 8)

