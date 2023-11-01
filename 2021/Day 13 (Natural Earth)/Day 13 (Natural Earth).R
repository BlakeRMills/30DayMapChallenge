# Libraries
library(geojsonio)
library(tidyverse)
library(sf)
library(cowplot)
library(showtext)
library(sysfonts)

# Aes
showtext_auto()
font_add_google("Federo")
font1 <- "Federo"
background <- "#0e513a"
color1 <- "#ffb744"
color2 <- "#cf9943"

# Data
roads <- geojson_sf("~/Desktop/ne_10m_roads")
population <- geojson_sf("~/Desktop/ne_10m_populated_places") %>% filter(POP2020 !=0)


# Plot
plot <- ggplot() +
  geom_sf(data=roads, size=0.1, color=color2) + 
  geom_sf(data=population, aes(size=POP2020), color= color1, shape=18) +
  coord_sf(crs = "+proj=wintri") +
  theme_void() +
  ggtitle("Cities and Roads of the World",
         subtitle = "\n(Size of diamonds corresponds with city population)") +
  labs(caption = "Twitter: @BlakeRobMills | Source: Natural Earth | GitHub: BlakeRMills") +
  theme(panel.background = element_rect(fill=background, color=background),
        plot.background = element_rect(fill=background, color=background),
        legend.position = "none",
        plot.title = element_text(size=200, hjust=0.5, color=color1, family=font1, face="bold"),
        plot.subtitle = element_text(size=55, hjust=0.5, color=color2, family=font1, lineheight = 0.28),
        plot.caption = element_text(size=50, hjust=0.5, face="bold", color=color1, family = font1, vjust-10),
        plot.margin = margin(0.75, 0, 0.5, 0, "cm"))

# Lines
ggdraw(plot) +
  draw_line(x=c(0, 0.22), y=c(0.91, 0.91), color=color1, size=2) +
  draw_line(x=c(0.78, 1), y=c(0.91, 0.91), color=color1, size=2) +
  draw_line(x=c(0, 0.335), y=c(0.051, 0.051), color=color1, size=0.5) +
  draw_line(x=c(0.665, 1), y=c(0.051, 0.051), color=color1, size=0.5) 
  

ggsave("~/Desktop/Day 13 (Natural Earth) .png", height = 11, width = 20)

