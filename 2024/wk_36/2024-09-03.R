# Load libraries
pacman::p_load(tidyverse, quantmod, ggtext, showtext, png, ggfx)

# Font
font_add_google(name = "Roboto Condensed")
font_add_google(name = "Ubuntu Condensed")
font_add_google(name = "Rosario")
font_add_google(name = "Dangrek")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <- paste0(
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:sans;color:white;'></span>",
  "<span style='font-family:Rosario;'> Manasseh Oduor </span>",
  "<img src='../X_logo_2023.svg.png' width='9'/> Manasseh_6 | #TidyTuesday #wk:36 2024"
)

# Load data
tuesdata <- tidytuesdayR::tt_load(2024, week = 36)

qname_levels_single_response_crosswalk <- tuesdata$qname_levels_single_response_crosswalk
stackoverflow_survey_questions <- tuesdata$stackoverflow_survey_questions
stackoverflow_survey_single_response <- tuesdata$stackoverflow_survey_single_response

# Data Wrangling
sof_dev_type <- qname_levels_single_response_crosswalk |>
  filter(qname == "dev_type") |>
  select(!qname) |>
  rename("dev_type"="level")

survey_sof_df <- stackoverflow_survey_single_response |>
  select(dev_type, country, currency, comp_total, main_branch) |>
  filter(country %in% "Kenya" & main_branch == 1) |>
  select(!main_branch) |>
  left_join(sof_dev_type, by = "dev_type") |>
  filter_all(all_vars(!is.na(.))) |>
  select(!dev_type)

# currency mapping
currency_map <- c(
  "KES\tKenyan shilling" = "KES",
  "USD\tUnited States dollar" = "USD"
)

# Retrieve exchange rates
exchange_rates <- getQuote("KESUSD=X")[, "Last"]
names(exchange_rates) <-  "KES"

# Disable scientific notation
options(scipen = 999)

survey_sof_df <- survey_sof_df |>
  mutate(currency_abbr = currency_map[currency]) |>
  rowwise() |>
  mutate(salary_usd = ifelse(currency_abbr == "USD", comp_total, comp_total * exchange_rates[currency_abbr])) |>
  select(!c(currency, comp_total, currency_abbr)) |>
  ungroup() 

# Median salary
med_salary <- survey_sof_df |>
  rename("dev_type"="label") |>
  summarise(med_sal = median(salary_usd), .by = c(country, dev_type),
            sample_size = n()) |>
  arrange(desc(sample_size)) |>
  mutate(Role_lab = paste0(dev_type, " (n=", sample_size, ")"),
         Salary_lab = paste0("$", scales::comma(med_sal)),
         h_just = ifelse(med_sal < 800, -0.1, 1.2),
         sal_col = ifelse(med_sal < 800, "black", "white"))

# Plot
ggplot(med_salary, aes(x = reorder(dev_type, med_sal), y = med_sal)) +
  with_inner_glow(geom_bar(stat = "identity", fill = "#03051E", width = 0.5), color = "#E7F6F2", sigma = 9) +
  geom_text(aes(x = dev_type, y = 0, label = Role_lab), hjust = 0, vjust = -2.2, color = "#2C3333", size = 3, family = "Ubuntu Condensed") +
  geom_text(aes(label = Salary_lab), 
            hjust = ifelse(med_salary$med_sal < 800, -0.1, 1.2), 
            color = ifelse(med_salary$med_sal < 800, "#1D1CE5", "#F0FF42"), size = 3, family = "Roboto Condensed") +
  coord_flip() +
  theme_void() +
  labs(
    title = paste0("<img src='C:/Users/Admin/OneDrive/Documents/R/#TidyTuesday/2024/TidyTuesday/Week 36/ke_flag.png' width='14'/> Professional Devs Annual Salary"),
    caption = cap, 
    tag = "<i>Source: Stack Overflow Annual Developer Survey 2024</i>") +
  theme(
    text=element_text(family="Roboto Condensed"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    strip.text = element_text(size = 16, color = "#354259", family = "Roboto Condensed"),
    plot.title = element_markdown(size=18, hjust=0.5, colour = "#399918", face = "bold", family = "Dangrek", margin = margin(t = 5, b = 10)),
    plot.background = element_rect(fill="#FEFCF3"),
    plot.caption = element_markdown(colour = 'black', hjust = 0.5, size = 8, family = 'Rosario', margin = margin(t = 10)),
    plot.tag.position = c(0.7, 0.4),
    plot.tag = element_markdown(size = 12, family = "Ubuntu Condensed"), 
    plot.margin = margin(b=20, t=20, r=20, l=20))
  
ggsave("Stack_Overflow_Annual_Developer_Survey.png", width = 10, height = 8)
