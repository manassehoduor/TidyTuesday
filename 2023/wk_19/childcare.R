
# load libraries
pacman::p_load(tidyverse,scales,hrbrthemes,viridis,ggtext,showtext,extrafont,shadowtext)

# Import fonts
font_add_google(name = "Raleway")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'>  Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | Source: NDCP | #TidyTuesday wk:19")


# load data
childcare_costs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/childcare_costs.csv')
counties <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/counties.csv')

# data wrangle
childcare_costs_df <- childcare_costs |>
  select(study_year,total_pop,mcsa,mfccsa,mc_infant,mc_toddler,mc_preschool,mfcc_infant,mfcc_toddler,mfcc_preschool) |>
  drop_na()

# pivot longer
childcare_costs_df_long <- childcare_costs_df |> 
  pivot_longer(cols = c(3:10), 
                        names_to = "childcare", 
                        values_to = "weekly_childcare_price") |>
  rename("tot_pop_size" = "total_pop") |>
  mutate(
    childcare = case_when(
    childcare == "mcsa" ~ "School-age center-based",
    childcare == "mc_infant" ~ "Infant center-based",
    childcare == "mc_toddler" ~ "Toddler center-based",
    childcare == "mc_preschool" ~ "Preschool center-based",
    childcare == "mfcc_infant" ~ "Infant home-based",
    childcare == "mfcc_toddler" ~ "Toddler home-based",
    childcare == "mfcc_preschool" ~ "Preschool home-based",
    childcare == "mfccsa" ~ "School-age home-based",
    TRUE ~ childcare
  ),
  tot_pop_size = case_when(
    tot_pop_size <= 99999 ~ "Pop Size: 1-99,999",
    tot_pop_size > 99999 & tot_pop_size <= 499999 ~ "Pop Size: 100,000-499,999",
    tot_pop_size > 499999 & tot_pop_size <= 999999 ~ "Pop Size: 500,000-999,999",
    TRUE ~ "Pop Size: 1,000,000+",
  ))

lbl <-  data.frame(y = 2008:2018) |> 
  mutate(x = "Toddler home-based")

# # Circular heat map plot
ggplot(childcare_costs_df_long) +
  geom_tile(aes(childcare, study_year, fill = weekly_childcare_price), color = "grey99") +
  geom_text(aes(childcare, 2014, label = childcare), family = "Raleway", 
            size = 4, stat = "unique", colour = "black", angle = 90) +
  shadowtext::geom_shadowtext(data = lbl, aes(x, y, label = y, 
                                              color = if_else(y == 2008, "white", "white")), 
                              size = 4, family = "Raleway", bg.color = "#add50d") +
  facet_wrap(~tot_pop_size) +
  scale_color_identity() +
  scale_fill_viridis_c(option = "mako", direction = -1,
                       name = "Weekly Median Childcare Price") +
  #coord_polar(clip = "off") +
  theme_minimal() +
  theme(text = element_text(family = "Raleway"),
        plot.title = element_markdown(colour = "black",face = "bold",
                                      size = 25, hjust = 0.5, family = "Raleway"),
        plot.subtitle = element_markdown(colour = "black",size = 14,
                                         hjust = 0, family = "Raleway"),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_text(face = "bold", size=12),
        legend.position = "top",
        legend.title.align = 0.5,
        legend.box.background = element_blank(),
        legend.title = element_text(family = "Raleway", face = "bold", size = 11),
        plot.caption = element_markdown(colour = 'black', hjust = 0.5, size = 11,
                                        family = 'Rosario', margin = margin(t = 10, b=10))) +
  labs(
    y = "",
    x = "",
    caption = cap,
    subtitle = "Weekly Median Prices of Center and Home Based Chilcare: Typically, home-based childcare is less costly<br>than center-based childcare. It's worth noting that the cost of center-based childcare for infants and toddlers<br>has been on the rise in recent years for population size of 1M +",
    title = "<br>CHILDCARE") +
  guides(fill = guide_colourbar(direction = 'horizontal', ticks.linewidth=2,
                                barwidth = 15, barheight = 0.3, title.position = "top"))

ggsave("childcare.png", height=11, width=12, bg = "#f3f1ff")

