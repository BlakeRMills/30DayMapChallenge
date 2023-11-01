# Libraries
library(tidyverse)
library(showtext)
library(sysfonts)
library(readr)
library(cowplot)
library(geojsonio)
library(sf)
library(RColorBrewer)


# Data

LL84_2020 <- read_csv("~/Desktop/Day 7 (Green)/Data/LL84_2020.csv")  
borMap <- geojson_sf("~/Desktop/Day 7 (Green)/Data/Borough Boundaries.geojson")
Central <- geojson_sf("~/Desktop/Day 7 (Green)/Data/Parks Nyc.geojson") %>% filter(park_name=="Central Park")


# Wrangling

LL84_2020 <- LL84_2020 %>% 
  filter(Borough=="MANHATTAN",
         `ENERGY STAR Score` != "Not Available") %>%  # Only get buildings withe Energy Scores
  dplyr::rename(c("BuildingType" = "Primary Property Type - Self Selected")) %>% # Make Building Type a shortner column name
  mutate(EnergyScore = as.numeric(`ENERGY STAR Score`)) %>% # Changes Energy Score to Numeric
  select(EnergyScore, Borough, Latitude, Longitude, BuildingType) %>% #Only Keep Useful Variables
  mutate(BuildingType = ifelse(BuildingType %in% c("Multifamily Housing", "Office", "Hotel", "K-12 School"),
                               BuildingType, "Other")) #Categorizes Building Types

borMap <- borMap %>% filter(boro_name=="Manhattan")

# Aes
showtext_auto()
font_add_google("Josefin Sans")
font_add_google("Barlow Condensed")
font1 <- "Josefin Sans"
font2 <- "Barlow Condensed"

# Plots

plotter <- function(Building){
  ggplot() +
    geom_sf(data=borMap, color="#f1d1b3", fill ="#f1d1b3") +
    geom_sf(data=Central, color="#fdf9f5", fill ="#fdf9f5") +
    geom_point(data=LL84_2020 %>% filter(BuildingType==Building), 
               aes(x=Longitude, y=Latitude, fill=EnergyScore, color=EnergyScore), size=2.86, alpha=0.8, shape=24) +
    scale_color_gradientn(colors=c(brewer.pal(9, "Greens")[3:9], "#002b11")) +
    scale_fill_gradientn(colors=c(brewer.pal(9, "Greens")[3:9], "#002b11")) +
    coord_sf() +
    theme_void() + 
    theme(legend.position = "none") +
    annotate(geom="text", label="Central Park", x=-73.9665, y=40.782, size=16, family=font1, fontface="bold", color="#00441B", angle=62) 
  
}

Hotel <- ggplot() +
  geom_sf(data=borMap, color="#f1d1b3", fill ="#f1d1b3") +
  geom_sf(data=Central, color="#fdf9f5", fill ="#fdf9f5") +
  geom_point(data=LL84_2020 %>% filter(BuildingType=="Hotel"), 
             aes(x=Longitude, y=Latitude, fill=EnergyScore, color=EnergyScore), size=2.6, alpha=0.8, shape=24) +
  scale_color_gradientn(colors=c(brewer.pal(9, "Greens")[3:9], "#002b11"),
                        breaks = c(1, 25, 50, 75, 100)) +
  scale_fill_gradientn(colors=c(brewer.pal(9, "Greens")[3:9], "#002b11"),
                       breaks = c(1, 25, 50, 75, 100)) +
  coord_sf() +
  theme_void() + 
  theme(legend.position = c(0.4, 1.1),
        legend.key.width = unit(4.5, "cm"),
        legend.key.height = unit(2.5, "cm"),
        legend.direction="horizontal",
        legend.title = element_blank(),
        legend.text = element_blank()) +
  annotate(geom="text", label="Central Park", x=-73.9665, y=40.782, size=16, family=font1, fontface="bold", color="#00441B", angle=62) 
  

# All Plots and Annotations

plot_grid(plotter("Multifamily Housing"),
        plotter("Office"),
        Hotel, 
        plotter("K-12 School"),
        plotter("Other"),
        nrow=1) +
  theme(plot.margin = margin(13, 2, 2, 2, "cm"),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
        panel.background = element_rect(fill="#fdf9f5", color="#fdf9f5")) +
  annotate(geom="text", label="Energy Star Scores of Buildings in Manhattan", x=0.5, y=1.42, size=125, family=font1, fontface="bold", color="#002b11") +
  annotate(geom="text", label="Maps show the Energy Star Scores of buildings covered under Local Law 84 in Manhattan. Energy Star Scores show efficiency of energy consumption on a scale of 1 (low) to 100 (high).", x=0.5, y=1.295, size=40, family=font2, color="#00441B") +
  annotate(geom="text", label="Twitter: @BlakeRobMills | Source: NYC Open Data | GitHub: BlakeRMills", x=0.5, y=-0.05, size=23, family=font1, fontface="bold", color="#00441B") +
  annotate(geom="text", label="More Efficient", x=0.567, y=1.205, size=25, family=font2, color="#00441B") +
  annotate(geom="text", label="Less Efficient", x=0.397, y=1.205, size=25, family=font2, color="#238B45") +
  annotate(geom="text", label="Energy Star Score", x=0.483, y=1.205, size=32, family=font1, fontface="bold", color="#00441B") +
  annotate(geom="text", label="1", x=0.351, y=1.03, size=28, family=font2, color="#A1D99B") +
  annotate(geom="text", label="25", x=0.417, y=1.03, size=28, family=font2, color="#74C476") +
  annotate(geom="text", label="50", x=0.483, y=1.03, size=28, family=font2, color="#238B45") +
  annotate(geom="text", label="75", x=0.55, y=1.03, size=28, family=font2, color="#006D2C") +
  annotate(geom="text", label="100", x=0.616, y=1.03, size=28, family=font2, color="#002b11") +
  annotate(geom="text", label="Multifamily Housing", x=0.085, y=0.7, size=45, family=font1, angle=64, fontface="bold", color="#00441B") +
  annotate(geom="text", label="Offices", x=0.2875, y=0.7, size=45, family=font1, angle=64, fontface="bold", color="#00441B") +
  annotate(geom="text", label="Hotels", x=0.49, y=0.7, size=45, family=font1, angle=64, fontface="bold", color="#00441B") +
  annotate(geom="text", label="K-12 Schools", x=0.6925, y=0.7, size=45, family=font1, angle=64, fontface="bold", color="#00441B") +
  annotate(geom="text", label="Other Buildings", x=0.895, y=0.7, size=45, family=font1, angle=64, fontface="bold", color="#00441B") +
  annotate(geom="segment", x=0.545, xend=0.615, y=1.175, yend=1.175, size=1.25, arrow = arrow(length = unit(5, "mm")), color="#00441B") +
  annotate(geom="segment", x=0.421, xend=0.351, y=1.175, yend=1.175, size=1.25, arrow = arrow(length = unit(5, "mm")), color="#74C476") 


# Save
ggsave("~/Desktop/BuildingScores.png", height = 16, width=35)


