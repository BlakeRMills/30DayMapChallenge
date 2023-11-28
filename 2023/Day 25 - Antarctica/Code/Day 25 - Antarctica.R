# Libraries
library(cowplot)
library(geojsonio)
library(MoMAColors)
library(osmdata)
library(sf)
library(showtext)
library(sysfonts)
library(tidyverse)

# Aes
showtext_auto()
font <- "Advent Pro"
font_add_google(font)

# Data
ant <- geojson_sf("~/Desktop/Countries.geojson") %>%
  filter(ADMIN == "Antarctica") %>%
  st_transform("+proj=laea +lat_0=-90 +lon_0=0")

earthquake <- geojson_sf("~/30DayMapChallenge/2023/Day 25 - Antarctica/Data/Earthquake.geojson") %>%
  arrange(mag)

# Map
m <- ggplot() +
  geom_sf(data=ant, color="transparent", fill="grey35") + 
  geom_sf(data=earthquake, aes(size = mag, color=mag), alpha =0.7)  +
  scale_size_continuous(range=c(2, 6)) +
  scale_color_moma_c("Exter", direction=-1, limits=c(4, 7)) +
  theme_void() +
  theme(plot.margin = margin(1.5, 0, 0.5, 0, "cm"),
        legend.key.width = unit(1., "cm"),
        legend.text = element_text(color="grey10"),
        legend.title = element_blank(),
        legend.key.height = unit(1.5, "cm"),
        legend.position = c(0.025, 0.28)) +
  guides(size=guide_none())

ggdraw(m) +
  theme(plot.background = element_rect(fill="grey10", color="transparent")) +
  draw_text(text="Twitter: @BlakeRobMills | Source: U.S. Geological Survey | GitHub: BlakeRMills", 
             color="white", x=0.5, y=0.015, size=40, fontface = "bold", family = font)  +
  draw_text(text="Antarctica Earthquakes", x=0.975, y=0.95, hjust=1, size=150, family=font, fontface="bold",
            color="white") +
  draw_text(text="Map displays the location of earthquakes near Antarctica (Jan 2020 - Nov 2023).\nPoints are sized and colored according to earthquake magnitude.", 
            x=0.975, y=0.885, hjust=1, size=40, family=font, color="white", lineheight=0.4) +
  draw_text(text="4", x=0.1425, y=0.1275, hjust=0, size=45, color="white", family=font, fontface="bold") +
  draw_text(text="5", x=0.1425, y=0.225, hjust=0, size=45, color="white", family=font, fontface="bold") +
  draw_text(text="6", x=0.1425, y=0.3225, hjust=0, size=45, color="white", family=font, fontface="bold") +
  draw_text(text="7", x=0.1425, y=0.42, hjust=0, size=45, color="white", family=font, fontface="bold") +
  draw_text(text="Earthquake Magnitude", x=0.075, y=0.28, size=50, color="white", family=font, fontface="bold", angle=90)

ggsave("~/30DayMapChallenge/2023/Day 25 - Antarctica/Day 25 - Antarctica.png", width = 10, height = 10)

