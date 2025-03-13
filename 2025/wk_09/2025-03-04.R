# Load libraries
pacman::p_load(tidyverse, waffle, ggtext, showtext)

# Load Fonts
font_add_google(name = "Rosario")
font_add_google(name = "Roboto Condensed")
font_add_google(name = "Averia Sans Libre")
font_add_google(name = "Averia Serif Libre")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Load data
longbeach <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-04/longbeach.csv')

# Data wrangling
stray_surv <- longbeach |>
  filter(animal_type == "dog" | animal_type == "cat") |>
  filter(intake_type == "stray") |> 
  filter(!is.na(intake_type), !is.na(intake_condition), !is.na(animal_type), !is.na(was_outcome_alive)) |>
  filter(str_detect(intake_condition, "ill|injured")) |> 
  mutate(survival_status = ifelse(was_outcome_alive == TRUE, "Alive", "Dead")) |>
  group_by(animal_type, intake_type, intake_condition, survival_status) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(animal_type, intake_type, intake_condition) |>
  mutate(percent = round(count / sum(count) * 100, 0))

stray_surv = data.frame(stray_surv)

stray_surv |> 
  select(animal_type, survival_status, intake_condition, count, percent) |>  
  #filter(str_detect(intake_condition, "ill")) |> 
  summarise(mean_surv = mean(percent),
            sum_surv = sum(count), .by = c(animal_type, survival_status))


# Plot
ggplot(stray_surv, aes(values = percent, fill = survival_status)) +
  geom_waffle(color = "white", size = 1.125, n_rows = 10) +
  facet_grid(vars(intake_condition), vars(animal_type)) +
  labs(
    title = "Long Beach Animal Shelter",
    subtitle = "Nine Lives vs. Man’s Best Friend: Who Survives More? <br> Survival of Ill and Injured Stray Animals in Shelters",
    x = "Intake Condition",
    y = "Percentage of Outcomes",
    fill = "Survival Status"
  ) +
  scale_x_discrete(expand = c(0,0,0,0)) +
  scale_y_discrete(expand = c(0,0,0,0)) +
  coord_equal() +
  ggthemes::scale_fill_tableau(name=NULL) +
  theme_enhance_waffle() +
  scale_fill_manual(values = c("Alive" = "black", "Dead" = "#EAD196")) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(family = "Roboto Condensed", size = 13),
    legend.title.position = "top",
    legend.title = element_text(family = "Rosario", size = 15),
    strip.background = element_rect(fill = "lightblue", color = NA),
    strip.text = element_text(color = "black", face = "bold", size = 13, family = "Roboto Condensed"),
    strip.placement = "inside", strip.switch.pad.grid = unit(0.5, "cm"),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(hjust = 0.5, size = 20, colour = "black", face = "bold", family = "Rosario", margin = margin(t=10,b=5)),
    plot.subtitle = element_markdown(family = "Rosario", size = 15, margin = margin(t=5,b=70))
  )


ggsave("long_beach.png", height = 12, width = 8, bg = "white")