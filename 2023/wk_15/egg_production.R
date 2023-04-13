rm(list = ls())

# libraries
pacman::p_load(waffle, tidyverse, ggplot2, extrafont, ggtext, showtext, patchwork)

# Import fonts
font_add_google(name = "Roboto Condensed", family = "Roboto Condensed")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# load data
eggproduction <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/egg-production.csv')

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'>.</span>",
               "<span style='font-family:sans;'> Manasseh Oduor </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | #TidyTuesday: wk 15 | Source: Samara Mendez | Data coverage: July 2016 : Feb 2021")
# data wrangle
eggprod <- eggproduction |>
  select(observed_month, prod_type, n_eggs) |>
  mutate(year = lubridate::year(observed_month),
         month = lubridate::month(observed_month),
         day = lubridate::day(observed_month),
         tray = n_eggs/30)

eggprod_month_year <- eggprod |>
  select(prod_type, tray, month, year) |>
  mutate(month = factor(month.name[month], levels = month.name)) |>
  group_by(month, year, prod_type) |>
  summarise(av_tray = round(median(tray)/1000000,0), .groups = "drop")

eggprod_month_year_table_eggs <- eggprod_month_year |>
  filter(prod_type == 'table eggs')

eggprod_month_year_hatching_eggs <- eggprod_month_year |>
  filter(prod_type == 'hatching eggs')

# Plot 1
P1 <- eggprod_month_year_table_eggs |> 
  ggplot(aes(y = month, x = year)) +
  geom_point(size = 22, color="#a16207", data = eggprod_month_year_table_eggs |> filter(av_tray >= 100)) +
  geom_point(size = 18, color="#ca8a04", data = eggprod_month_year_table_eggs |> filter(av_tray >= 50)) +  
  geom_point(size = 16, color="#eab308", data = eggprod_month_year_table_eggs |> filter(av_tray >= 40)) + 
  geom_point(size = 14, color="#facc15", data = eggprod_month_year_table_eggs |> filter(av_tray >= 30)) +
  geom_point(size = 12, color="#fde047", data = eggprod_month_year_table_eggs |> filter(av_tray >= 20)) +
  geom_point(size = 10, color="#fef08a", data = eggprod_month_year_table_eggs |> filter(av_tray < 20)) +
  geom_text(aes(label = paste0(av_tray, " M")), color = "#0a0a0a", size = 3, data = eggprod_month_year_table_eggs) + 
  coord_equal(ratio = 0.5) +
  scale_y_discrete(limits = rev(month.name)) +
  labs(x = "Year",
       y = "Month",
       title = "The Annual Median Production of Table Eggs in the US") +
  theme(
    text = element_text(family = "Roboto Condensed"),
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 16,
                              hjust = 0.5,
                              family = "Roboto Condensed"),
    axis.title.x = element_text(family = "Roboto Condensed", size = 15, vjust = -3, face = "bold"),
    axis.title.y = element_text(family = "Roboto Condensed", size = 15, face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(family = "Roboto Condensed", size = 14, color = "#000000"),
    axis.text.y = element_text(family = "Roboto Condensed", size = 14, color = "#000000"),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    strip.text.x = element_blank(),
    plot.background = element_rect(fill = "#f7f7f7", color = NA), # color removes the border
    #plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  )


# Plot 2
P2 <-  eggprod_month_year_hatching_eggs |> 
  ggplot(aes(y = month, x = year)) +
  geom_point(size = 22, color="#b91c1c", data = eggprod_month_year_hatching_eggs |> filter(av_tray >= 40)) +
  geom_point(size = 18, color="#dc2626", data = eggprod_month_year_hatching_eggs |> filter(av_tray >= 38)) +  
  geom_point(size = 16, color="#ef4444", data = eggprod_month_year_hatching_eggs |> filter(av_tray >= 36)) + 
  geom_point(size = 14, color="#f87171", data = eggprod_month_year_hatching_eggs |> filter(av_tray >= 34)) +
  geom_point(size = 10, color="#fca5a5", data = eggprod_month_year_hatching_eggs |> filter(av_tray < 33)) +
  geom_text(aes(label = paste0(av_tray, " M")), color = "#0c0a09", size = 3, data = eggprod_month_year_hatching_eggs) + 
  coord_equal(ratio = 0.5) +
  scale_y_discrete(limits = rev(month.name)) +
  labs(x = "Year",
       y = "Month",
       title = "The Annual Median Production of Hatching Eggs in the US") +
  theme(
    text = element_text(family = "Roboto Condensed"),
    plot.title = element_text(margin = margin(b = 8), 
                              color = "#22222b",face = "bold",size = 16,
                              hjust = 0.5,
                              family = "Roboto Condensed"),
    axis.title.x = element_text(family = "Roboto Condensed", size = 15, vjust = -3, face = "bold"),
    axis.title.y = element_text(family = "Roboto Condensed", size = 15, face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(family = "Roboto Condensed", size = 14, color = "#000000"),
    axis.text.y = element_text(family = "Roboto Condensed", size = 14, color = "#000000"),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    strip.text.x = element_blank(),
    plot.background = element_rect(fill = "#f7f7f7", color = NA), # color removes the border
    #plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.ticks = element_blank()
  )


# Patching the two plots
P1 + P2 +
  plot_annotation(
    title = 'Production of Eggs in the US',
    subtitle = "Table eggs production is greater than hatching eggs production in the US. However, there has been a steady rise/demand of Table eggs compared to Hatching eggs products over the years.<br>In July 2016, the highest median production of Table eggs was recorded, with 245 million trays produced",
    caption = cap,
    theme = theme(plot.title = element_text(face = "bold", colour = "black",
                                            family = "Roboto Condensed", size = 40, hjust = 0.5),
                  plot.subtitle = element_markdown(family = "Roboto Condensed", colour = "#1e3a8a",
                                                   size = 18),
                  plot.caption = element_textbox_simple(
                    lineheight = 1, colour = "black", size = 14, hjust = 0.5, margin = margin(t=30,b=5), 
                    family = "Rosario"))
  )

ggsave("eGG_Prod.png", height=12, width=22, bg = "#f7f7f7")



