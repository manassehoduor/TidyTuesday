
# Upload the packages
pacman::p_load(tidytuesdayR, tidyverse, usmap, usdata, janitor,
               showtext, lubridate, gganimate, gifski, transformr)

# load data
tuesdata <- tidytuesdayR::tt_load(2022, week = 24)
drought <- tuesdata$drought
fips <- tuesdata$`drought-fips`

# Wrangle data
fips_df <- fips %>% 
  clean_names() %>% 
  mutate(date = ymd(date)) %>%
  mutate(month = format(date, "%b"),
         year = format(date, "%Y"),
         state = abbr2state(state)) %>%
  group_by(year, state) %>%
  summarise(mean_dsci = mean(dsci))

fips_df$fips = fips(fips_df$state)
dplyr::sample_n(fips_df, 5)

# Plot
out <- plot_usmap(data = fips_df, values = "mean_dsci", color = "black") + 
  scale_fill_continuous(
    low = "#FFFF00", high = "#6E260E", 
    name = "Mean DSCI Scores", 
    label = scales::comma) + 
  labs(title = "Drought Severity in the U.S",
       subtitle = "Drought Severity and Coverage Index (DSCI) scores for U.S between 2000-2022. \n A DSCI value of 0 means that none of the area is abnormally dry or in drought. \n A DSCI value of 500 means that all of the area is exceptional drought. \n \n Year: {closest_state}",
       caption = "Viz Credits: @_Manasseh__ | Inspiration: @jamie_bio | Source: National Integrated Drought Information System") +
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5,
                               barwidth = unit(25, 'lines'), barheight = unit(1, 'lines'))) +
  theme(legend.position = "bottom",
        legend.justification = "bottom",
        legend.direction = "horizontal",   
        panel.background = element_rect(fill = "white", color = "white"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 3),
        legend.title = element_text(size = 4),
        plot.title = element_text(face = "bold",
                                  size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 4, vjust = 0.1, hjust = 0.5, lineheight = 3),
        plot.caption = element_text(size = 4, margin=margin(t=13)),
        plot.margin=margin(.5,.5,.3,.5, unit = "cm"))
      
anim <- out +
    transition_states(year, transition_length = 1,
                      state_length = 5)

animate(anim, 
        height = 7, width = 8, 
        units = "in", res = 150, end_pause = 10, 
        renderer = gifski_renderer(here::here("US_Droughts.gif")))



