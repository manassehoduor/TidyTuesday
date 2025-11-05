# Load libraries
pacman::p_load(tidyverse, scales, ggtext, showtext)

## Load font
font_add_google("Nunito")

showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# Load data
flint_mdeq <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-04/flint_mdeq.csv')
flint_vt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-04/flint_vt.csv')

# Data Wrangling
flint_vt = flint_vt[sample(nrow(flint_vt)), ]

# Summary stats
mean_val <- mean(flint_vt$lead)
median_val <- median(flint_vt$lead)
p90_val <- quantile(flint_vt$lead, 0.9)
cutoff <- 15

# Polar mapping
flint_vt <- flint_vt |>
  mutate(
    angle = seq(0, 2*pi, length.out = n()),
    # ensure a hole by scaling radius to start above a min radius
    radius = rescale(lead, to = c(3, 10)))

# Plot
ggplot(flint_vt, aes(x = angle, y = radius)) +
  geom_point(aes(color = lead), size = 1.2, alpha = 0.8, shape = 21, color = "black", fill = "white",  stroke = 0.3) +
  geom_point(data = flint_vt |> filter(lead > p90_val),
             aes(x = angle, y = radius), size = 5, color = "#F9E400", alpha = 0.25) +
  geom_point(data = flint_vt |> filter(lead > p90_val),
             aes(x = angle, y = radius), size = 2, color = "white", alpha = 0.9) +
  geom_point(data = flint_vt |> filter(lead > cutoff & lead < p90_val),
             aes(x = angle, y = radius), size = 3, color = "#C2E2FA", alpha = 0.25) +
  geom_point(data = flint_vt |> filter(lead > cutoff & lead < p90_val),
             aes(x = angle, y = radius), size = 1.5, color = "white", alpha = 0.9) +
  scale_color_gradientn(colours = c("#00C9A7", "#FFD700", "#FF4C00"), name = "Lead (ppb)") +
  geom_hline(yintercept = rescale(cutoff, to = c(3, 10), from = range(flint_vt$lead)), color = "#FFB200", size = 0.6) +
  geom_hline(yintercept = rescale(mean_val, to = c(3, 10), from = range(flint_vt$lead)), color = "#0079FF", linetype = "solid", size = 0.4) +
  geom_hline(yintercept = rescale(median_val, to = c(3, 10), from = range(flint_vt$lead)), color = "#ED3F27", linetype = "dashed", size = 0.4) +
  geom_hline(yintercept = rescale(p90_val, to = c(3, 10), from = range(flint_vt$lead)), color = "orange", linetype = "dotted", size = 0.8) +
  coord_polar(start = 0, clip = "off") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Lead Concentration in Flint Water") +
  annotate("text", x = 0, y = 0, 
           label = paste0(
             "Mean: ", round(mean_val, 1), "\n",
             "Median: ", round(median_val, 1), "\n",
             "90th %: ", round(p90_val, 1), "\n",
             "Cutoff: ", cutoff, " ppb"), size = 3, color = "white", fontface = "bold", lineheight = 1, family = "Nunito") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", colour = "#FFEDFA", vjust = 10, size = 25, family = "Nunito"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid = element_line(colour = "grey70", linewidth = 0.2),
    panel.background = element_rect(fill = "#180161", color = "#180161"),
    plot.background = element_rect(fill = "#180161"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.margin = margin(t=20,b=20,l=80,r=80)
  )

ggsave("lead.png", height = 8, width = 8, dpi = 320)