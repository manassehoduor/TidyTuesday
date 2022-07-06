#Libraries
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggtext)
library(ggthemes)



# show text
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# add font
font_add_google("Merriweather")

# load data
tuesdata <- tidytuesdayR::tt_load(2022, week = 27)
rent <- tuesdata$rent

# clean data
rent_df <- rent |>
  filter(county == "san francisco") |> 
  select(year, price) |> 
  mutate(year = as.factor(year)) |>
  group_by(year)|>
  summarise(med_rent = median(price, na.rm = TRUE)) 
  

sum(is.na(rent_df)) # check if NAs available

# color pallets
clr_palletes <- c("#FF1700", "#363062", "#4D4C7D", "#827397", "#9A86A4", "#CDC2AE",
             "#CC9544", "#1C0A00", "#361500", "#603601", "#D92027", "#8CA1A5",
             "#6D8299", "#6B7AA1", "#316B83", "#480032", "#480032", "#3F4E4F", "#7D9D9C")
# Plot
rent_df |>
  ggplot(aes(x = year, y = med_rent, label = med_rent)) +
  theme_minimal() +
  geom_segment(aes(x = year, xend = year, y = 1500, yend = med_rent,
                   colour = clr_palletes), size = 2.5) +
  geom_point(aes(colour = clr_palletes), size = 9) +
  geom_text(colour = "white", size = 2.5) +
  scale_colour_identity() +
  scale_y_continuous(labels=scales::dollar_format()) +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9)) +
  labs(y="Median Rental Prices",
       title =  "*<span style = 'color: #8843F2;'>Median Rental Prices </span> in San Francisco*",
       subtitle= "Last two decades <span style='color:#FF1700;'>**2000**</span>:<span style='color:#7D9D9C;'>**2018**</span>",
       caption="Viz by: Manasseh O. | #TidyTuesday | Source: data.sfgov.org") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(color = "#000000", family = "Merriweather", size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "#000000", family = "Merriweather", size = 11),
        legend.position = "none",
        axis.line.x = element_line(colour = "#000000", size = 0.3),
        plot.title = element_markdown(face = "bold", family = "Merriweather",
                                  size = 25, hjust = 0.5),
        plot.subtitle = element_markdown(size = 20, vjust = 0.2, hjust = 0.5,
                                         family = "Merriweather", lineheight = 1.1, 
                                         face = "italic", colour = "#14213d"),
        plot.caption = element_text(family = "Merriweather", size = 8, hjust = 0.9))
  

ggsave("sanF_rent.png", height=7, width=9, bg= "white")


