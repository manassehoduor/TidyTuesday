rm(list = ls())

library("tidytuesdayR")
library("tidyverse")
library("dplyr")
library("gapminder")
library('maptools')
library('sf')
library('ggplot2')
library("viridis")
library("cartogram")
library("ggtext")
library("maptools")

# Load dataset
tuesdata <- tidytuesdayR::tt_load(2022, week = 21)
data_7s = tuesdata$sevens

# countries with wins by year
sevens_win <- data_7s  %>% 
  group_by(winner) %>% 
  rename(country = winner) %>% 
  summarise(wins = n()) %>% 
  arrange(desc(wins))

# Clean name string
sevens_win[sevens_win == "Cote d'Ivorie"] <-  "Cote d'Ivoire"

# load gapminder dataset
data("gapminder")

# Select country and continent from gapminder
cont_data <- gapminder %>% 
  select(country, continent) %>% 
  distinct()

# join two data frames (sevens_win & cont_data)
merged_data = sevens_win %>% 
  left_join(cont_data, by = "country") %>% 
  drop_na() # Missing data confirmed not to be in Africa Continent

# Filter Winners from Africa Continent only
Africa_data <- merged_data %>% 
  filter(continent == "Africa") %>% 
  select(country, wins)

# Get the shape file of Africa
data(wrld_simpl)
afr=wrld_simpl[wrld_simpl$REGION==2,]

# Visualize the region's boundaries with the plot function
plot(afr)

# Set the Coordinate Reference System for Africa
sfno <- st_as_sf(afr)
st_crs(sfno)
sfproj <- st_transform(sfno, crs = 3857)
st_crs(sfproj)

a = sfproj 

Africa_data_ <- Africa_data %>% 
  rename(NAME = country)

new_merged_data = a %>% 
  left_join(Africa_data_, by = "NAME") %>% 
  mutate(wins = ifelse(is.na(wins), 0, wins))

# construct a cartogram using the number of all-time 7s tournament wins 
afr_cartogram <- cartogram_cont(new_merged_data, "wins", itermax = 5)

# Final Cartogram and choropleth Plot
caption <- "Data from ScrumQueens by way of Jacquie Tran.<br>
Tidytuesday Week-21 2022: Created by: Manasseh Oduor<br>"

png("tidytuesday_2022_w21.png", units = "in", width = 11, height = 7, res = 300, bg = "azure")
p = ggplot() +
  geom_sf(data = afr_cartogram, aes(fill = wins), 
          size=0, alpha=0.9) +
            theme_void() +
  labs(title = "Number of Wins from African Teams in all-time 7s Women’s Rugby Tournaments",
       subtitle = "South Africa, Kenya, and Tunisia leads the Women's 7s campaign in the African Continent.
       The three Giants have  surpassed 50 WINS in all the world 7s tournaments collectively.
       Duration: 15th-03-1997 to 28th-11-2022.",
       caption = caption) +
  theme(
    plot.caption = ggtext::element_markdown(size= 10, family = "Gotham Book")) +
            scale_fill_viridis(name="Number of Wins", breaks=c(1,20,40, 60, 80, 100))

p
dev.off()
