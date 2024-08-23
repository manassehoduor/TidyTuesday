# load libraries
pacman::p_load(tidytuesdayR, tidyverse, janitor, ggtext, showtext, patchwork, glue)

# Font
font_add_google(name = "Roboto Condensed")
font_add_google(name = "Ubuntu Condensed")
font_add_google(name = "Salsa")
font_add_google(name = "Chicle")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<img src='C:/Users/Admin/OneDrive/Documents/R/#TidyTuesday/2024/TidyTuesday/X_logo_2023.svg.png' width='12'/> Manasseh_6 | #TidyTuesday #wk 34 2024")

# load data # Option 1
tuesdata <- tidytuesdayR::tt_load(2024, week = 34)
english_monarchs_marriages_df <- tuesdata$english_monarchs_marriages_df

# option 2: Read directly from GitHub
english_monarchs_marriages_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-20/english_monarchs_marriages_df.csv')

# data wrangle
monarchs_df <- english_monarchs_marriages_df |>
  clean_names() |>
  remove_empty() |>
  filter_all(all_vars(!is.na(.))) |>  # Remove rows with any NA
  filter_all(all_vars(!grepl("\\?|\\–", .))) |>  # Remove rows with ? or -
  filter_all(all_vars(!grepl("\\d+\\(\\?\\)", .))) |> #  # Remove rows with digits embedded with (?)
  arrange(king_name) |>
  mutate(across(c(king_age, consort_age, year_of_marriage), as.numeric))

# Identify more than 1 wed
monarchs_df |> 
  filter(duplicated(year_of_marriage) | duplicated(year_of_marriage, fromLast = TRUE))

# Delete duplicate
monarchs_df <-monarchs_df |>
  filter(!(king_name == "Mary II" & king_age == 15 & consort_name == "William III" & consort_age == 27 & year_of_marriage == 1677))

# Categorize the rulers into royal dynasty
monarchs_df <- monarchs_df |>
  mutate(dynasty = case_when(
    king_name %in% c("Alfred the Great", "Æthelbald", "Æthelred the Unready", "Edgar the Peaceful", "Edward the Confessor") ~ "House of Wessex",
    king_name %in% c("William I", "Henry I", "Stephen") ~ "House of Normandy",
    king_name %in% c("Henry II", "Richard I", "John", "Henry III", "Henry the Young King", "Edward I", 
                           "Edward II", "Edward III", "Richard II") ~ "House of Plantagenet",
    king_name %in% c("Henry IV", "Henry V", "Henry VI") ~ "House of Lancaster",
    king_name %in% c("Edward IV", "Richard III") ~ "House of York",
    king_name %in% c("Henry VII", "Henry VIII", "Mary I", "Elizabeth I") ~ "House of Tudor",
    king_name %in% c("James I", "Charles I", "Charles II", "James II") ~ "House of Stuart",
    king_name %in% c("William III", "Mary II", "Anne") ~ "House of Stuart (Orange-Nassau connection)",
    king_name %in% c("George I", "George II", "George III", "George IV", "William IV") ~ "House of Hanover",
    king_name %in% c("Victoria", "Edward VII", "George V", "Edward VIII", "George VI") ~ "House of Saxe-Coburg and Gotha (Windsor)",
    king_name == "Elizabeth II" ~ "House of Windsor",
    TRUE ~ "Unknown"
  ))

# Define groups of dynasties
## early medieval period and the development of the English monarchy
group1 <- c("House of Wessex", "House of Normandy", "House of Plantagenet")

## late medieval period through the Tudor era, highlighting major dynastic conflicts and transitions.
group2 <- c("House of Lancaster", "House of York", "House of Tudor")

## modern era of the British monarchy
group3 <- c("House of Stuart", "House of Stuart (Orange-Nassau connection)", "House of Hanover", 
            "House of Saxe-Coburg and Gotha (Windsor)", "House of Windsor")

# Highest Age Difference in each dynasty group
g1 <- monarchs_df |>
  filter(dynasty %in% group1) |>
  mutate(age_difference = abs(king_age - consort_age)) |>
  arrange(desc(age_difference)) |>
  slice_max(order_by = age_difference, n = 1)

g2 <- monarchs_df |>
  filter(dynasty %in% group2) |>
  mutate(age_difference = abs(king_age - consort_age)) |>
  arrange(desc(age_difference)) |>
  slice_max(order_by = age_difference, n = 1)

g3 <- monarchs_df |>
  filter(dynasty %in% group3) |>
  mutate(age_difference = abs(king_age - consort_age)) |>
  arrange(desc(age_difference)) |>
  slice_max(order_by = age_difference, n = 1)

# Median Age difference in each dynasty group
gg1 <- monarchs_df |>
  filter(dynasty %in% group1) |>
  mutate(age_difference = abs(king_age - consort_age)) |>
  summarise(med_age_diff = round(median(age_difference), 1))

gg2 <- monarchs_df |>
  filter(dynasty %in% group2) |>
  mutate(age_difference = abs(king_age - consort_age)) |>
  summarise(med_age_diff = round(median(age_difference), 1))

gg3 <- monarchs_df |>
  filter(dynasty %in% group3) |>
  mutate(age_difference = abs(king_age - consort_age)) |>
  summarise(med_age_diff = round(median(age_difference), 1))

# Plot
p1 <- ggplot(monarchs_df |>
               filter(dynasty %in% group1)) +
  geom_segment( aes(x=year_of_marriage, xend=year_of_marriage, y=king_age, yend=consort_age),  color="#F9E400", linewidth = 1) +
  geom_point( aes(x=year_of_marriage, y=king_age), color = "#CD5C08", size=3) +
  geom_point( aes(x=year_of_marriage, y=consort_age), color = "#0079FF", size=3) +
  geom_text(data = g1, aes(year_of_marriage, king_age, label= king_name),size=3, color = "#CD5C08", vjust=-1, hjust=1.1, family="Roboto Condensed", fontface="italic") +
  geom_text(data = g1, aes(year_of_marriage, consort_age, label= consort_name),size=3, color = "#0079FF", vjust=1.5, family="Roboto Condensed", fontface="italic") +
  coord_flip() +
  facet_grid(cols = vars(dynasty)) +
  cowplot::theme_minimal_grid(9.5,line_size = 0.3) +
  labs(title = glue("Early Dynasties ({gg1$med_age_diff})"),
       y = "Consort/Ruler's age") +
  theme(text=element_text(family="Roboto Condensed"),
        panel.grid.major.y=element_line(linetype = "dotted"),
        axis.title.x = element_text(size = 16, color = "#435055", face = "bold", family = "Roboto Condensed"),
        axis.title.y = element_blank(),
        axis.text.y=element_markdown(hjust=0,color="#1A3636", size = 15),
        axis.text.x = element_text(color="#1A3636", size = 13),
        strip.text = element_text(size = 16, color = "#435055", family = "Roboto Condensed"),
        plot.margin=margin(.5,.75,.5,.5, unit="cm"),
        plot.title=element_text(size=20, hjust=1, colour = "#1E201E"),
        plot.background = element_rect(fill="#FDFAF6")
  )

p2 <- ggplot(monarchs_df |>
               filter(dynasty %in% group2)) +
  geom_segment( aes(x=year_of_marriage, xend=year_of_marriage, y=king_age, yend=consort_age),  color="#F9E400", linewidth = 1) +
  geom_point( aes(x=year_of_marriage, y=king_age), color = "#CD5C08", size=3) +
  geom_point( aes(x=year_of_marriage, y=consort_age), color = "#0079FF", size=3) +
  geom_text(data = g2, aes(year_of_marriage, king_age, label= king_name),size=3, color = "#CD5C08", vjust=-1.5, family="Roboto Condensed", fontface="italic") +
  geom_text(data = g2, aes(year_of_marriage, consort_age, label= consort_name),size=3, color = "#0079FF", vjust=2, family="Roboto Condensed", fontface="italic") +
  coord_flip() +
  facet_grid(cols = vars(dynasty)) +
  cowplot::theme_minimal_grid(9.5,line_size = 0.3) +
  labs(title = glue("Medieval to Tudor Dynasties ({gg2$med_age_diff})"),
       x = "Historical year of marriage") +
  theme(text=element_text(family="Roboto Condensed"),
        panel.grid.major.y=element_line(linetype = "dotted"),
        axis.title = element_text(size = 16, color = "#435055", face = "bold", family = "Roboto Condensed"),
        axis.title.x = element_blank(),
        axis.text.y= element_markdown(hjust=0,color="#1A3636", size = 15),
        axis.text.x = element_text(color="#1A3636", size = 13),
        strip.text = element_text(size = 16, color = "#435055", family = "Roboto Condensed"),
        plot.margin = margin(.5,.75,.5,.5, unit="cm"),
        plot.title = element_text(size=20, hjust=1, colour = "#3C3D37"),
        plot.background = element_rect(fill="#FDFAF6")
  )

p3 <- ggplot(monarchs_df |>
               filter(dynasty %in% group3)) +
  geom_segment( aes(x=year_of_marriage, xend=year_of_marriage, y=king_age, yend=consort_age),  color="#F9E400", linewidth = 1) +
  geom_point( aes(x=year_of_marriage, y=king_age), color = "#CD5C08", size=3) +
  geom_point( aes(x=year_of_marriage, y=consort_age), color = "#0079FF", size=3) +
  geom_text(data = g3, aes(year_of_marriage, king_age, label= king_name),size=3, color = "#CD5C08", vjust=-1, hjust = 1, family="Roboto Condensed", fontface="italic") +
  geom_text(data = g3, aes(year_of_marriage, consort_age, label= consort_name),size=3, color = "#0079FF", vjust=-2, hjust =0.4, family="Roboto Condensed", fontface="italic") +
  coord_flip() +
  #facet_grid(~dynasty, scales = "free_x") +
  facet_grid(cols = vars(dynasty)) +
  cowplot::theme_minimal_grid(8, line_size = 0.3) +
  labs(title = glue("Stuart to Windsor Dynasties ({gg3$med_age_diff})")) +
  theme(text=element_text(family="Roboto Condensed"),
        panel.grid.major.y=element_line(linetype = "dotted"),
        axis.title = element_blank(),
        axis.text.y=element_markdown(hjust=0,color="#1A3636", size = 15),
        axis.text.x = element_text(color="#1A3636", size = 13),
        strip.text = element_text(size = 16, color = "#435055", family = "Roboto Condensed"),
        plot.margin=margin(.5,.75,.5,.5, unit="cm"),
        plot.title=element_text(size=20, hjust=1, colour = "#697565"),
        plot.background = element_rect(fill="#FDFAF6")
  )

# Combine the plots using patchwork
ppp <- p3 / p2 / p1 + 
  plot_layout(ncol = 1) +
  plot_annotation(title = "Comparative Ages of British Monarchs at Marriage Across Dynasties",
                  subtitle = glue(
                    "The median age difference between British <span style='color:#CD5C08'>**Monarchs**</span> and their <span style='color:#0079FF'>**Consorts**</span> at marriage is ({gg3$med_age_diff}) yrs in the <span style='color:#697565'>**Stuart to Windsor Dynasties**</span>. <br> This is lower compared to the ({gg1$med_age_diff}) yr difference observed in both the <span style='color:#1E201E'>**Early Dynasties**</span> and <span style='color:#3C3D37'>**Medieval to Tudor Dynasties**</span>. <br> Possible Social and Cultural Shifts?"),
                  caption = cap,
                  theme = theme(
                    axis.title = element_text(color="grey20", size = 15),
                    plot.background = element_rect(fill = "#F9F9F9"),
                    panel.background = element_rect(fill = "#F9F9F9"),
                    plot.title = element_text(family = "Chicle", colour = "#2C3333", face = "bold",
                                              size = 26, hjust = 0, margin = margin(t = 5, b = 5)),
                    plot.subtitle = element_markdown(family = "Salsa", colour = "black",
                                                     size = 14, hjust = 0, margin = margin(t = 5, b = 5)),
                    plot.caption = element_markdown(colour = 'black', hjust = 0.5, size = 12,
                                                    family = 'Rosario', margin = margin(t = 10)),
                    plot.margin = margin(b=20, t=20, r=20, l=20)))
  
# Save plot
ggsave("Monarch.png", width=12, height=14, bg = "#F9F9F9")

