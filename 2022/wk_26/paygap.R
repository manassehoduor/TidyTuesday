# Libraries
library(tidytuesdayR)
library(rio)
library(tidyverse)
library(ggtext)
library(showtext)

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Font
font_add_google("Merriweather", "Libre Franklin")

# load data
tuesdata <- tidytuesdayR::tt_load(2022, week = 26)
paygap <- tuesdata$paygap

# wrangle data
paygap <- paygap %>% 
  mutate(
    employer_name = str_remove_all(employer_name, "\""),
    employer_name = str_replace_all(employer_name, ", |,", ", "),
    employer_name = str_to_title(employer_name)
    ) 

# pivot the selected variables of interest from pay-gap into a long format
paygap_sel_fbl = paygap %>% 
  select(employer_name, employer_size, female_bonus_percent, male_bonus_percent) %>% 
  mutate(bonus_diff = abs(male_bonus_percent - female_bonus_percent)) %>% 
  arrange(desc(bonus_diff)) %>% 
  filter(grepl(c('Football'), employer_name)) %>% 
  filter(grepl('Manchester|Leicester City|Chelsea|Tottenham|Wolverhampton Wanderers|
               West Ham|West Bromwich Albion|Watford|Liverpool|Arsenal|Swansea City|
               Sunderland|Stoke City|Southampton|Norwich|Newcastle United|
               Leeds United|Fulham|Everton|Burnley|Brighton And Hove Albion', employer_name)) %>% 
  pivot_longer(cols=c('female_bonus_percent', 'male_bonus_percent'),
               names_to='gender',
               values_to='bonus_percent') %>% 
  mutate(gender = as_factor(gender))

# EPL logos scrapped from ESPN website
logo <- import("logo.xlsx", sheet = "logo")

# Join 
paygap_sel_fbl_n <- full_join(paygap_sel_fbl, logo, by = "employer_name")

paygap_sel_fbl_n <- paygap_sel_fbl_n  %>% 
  mutate(image_epl = paste0("<img src ='", url_logo_espn, "' width='20'>"))

paygap_mean_bonus_pct <- paygap_sel_fbl_n  %>% 
  group_by(team, gender, image_epl) %>% 
  summarise(mean_bonus_pct = mean(bonus_percent))

rlb <- paygap_mean_bonus_pct %>%
  group_by(team) %>%
  arrange(desc(mean_bonus_pct)) %>%
  top_n(1)

llb <- paygap_mean_bonus_pct %>%
  group_by(team) %>%
  arrange(desc(mean_bonus_pct)) %>%
  slice(2)

# Plot
ggplot(paygap_mean_bonus_pct, aes(mean_bonus_pct, image_epl)) +
  geom_line(aes(group = image_epl), alpha = 0.9, size = 0.7, color="#f0c808") +
  geom_point(aes(color = gender), size = 3.5, shape = 21, fill = "white", 
             stroke = 1, show.legend = FALSE) +
  geom_richtext(aes(x = -5, y = image_epl, label = image_epl),
                hjust = 0, label.color = NA,
  ) +
  geom_text(aes(x = -17, y = image_epl, label = team),
            family = "Libre Franklin",
            hjust = 0, size = 3, color = "#14213d"
  ) +
  
  geom_text(data = rlb, aes(color = gender, label = round(mean_bonus_pct, 1)),
            size = 2.0, hjust = -.9) +
  geom_text(data = llb, aes(color = gender, label = round(mean_bonus_pct, 1)),
            size = 2.0, hjust = 2.5) +
  scale_color_manual(values=c("#dd1c1a","#07a0c3")) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                     labels = function(x) paste0(x, "%"),
                     limits = c(-17, 100)) +
  labs(x="Mean Bonuses Percent",
       title = "Gender Pay Gap Bonus Differences 'mongst the UK Football Teams",
       subtitle= "The majority of the EPL Teams pay <span style='color:#07a0c3;'>**Male employees**</span> more bonuses compared to <span style='color:#dd1c1a;'>**Female employees**</span>",
       caption="Viz by: Manasseh | #TidyTuesday | Source: gender-pay-gap.service.gov.uk") + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(color = "#000000", family = "Merriweather"),
        axis.title.y = element_blank(),
        legend.position = "none",
        axis.line.x = element_line(colour = "#000000", size = 0.3),
        plot.title = element_text(family = "Merriweather", face = "bold",
                                  size = 14, hjust = 0.5),
        plot.subtitle = element_markdown(size = 10, vjust = 0.2, hjust = 0.6,
                                     family = "Merriweather", lineheight = 1.1, 
                                     face = "italic", colour = "#14213d"),
        plot.caption = element_text(family = "Merriweather", size = 8,
                                    margin = margin(20, 0, 0, 0), hjust = 0.9))

ggsave("pay_gap.png", height=8, width=8, bg= "white")


