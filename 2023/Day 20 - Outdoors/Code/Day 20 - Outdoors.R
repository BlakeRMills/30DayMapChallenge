# Libraries
library(cowplot)
library(elevatr)
library(geojsonio)
library(osmdata)
library(raster)
library(sf)
library(showtext)
library(sysfonts)
library(tidyverse)

# Aes
showtext_auto()
font <- "Advent Pro"
font_add_google(font)
pal <- moma.colors("Dali", 13)

# Data
Veg <- geojson_sf("~/Desktop/GRSM_VEGETATION.geojson")

# Map
m <- ggplot() +
  geom_sf(data = Veg, aes(fill=VitalName), color="transparent") +
  scale_fill_moma_d("Dali") +
  theme_void() + 
  theme(legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = c(0.5, -0.1),
        legend.key.width = unit(2.25, "cm"),
        plot.margin = margin(-1, 0, 0.5, 0, "cm")) +
  guides(fill = guide_legend(nrow=1))

ggdraw(m) +
  theme(plot.background = element_rect(fill="#fdf9f5", color="transparent")) +
  draw_text(text="Great Smoky Mountains", color=pal[4], x=0.025, y=0.925, family=font, fontface="bold", size=250, hjust=0) + 
  draw_text(text="Map displays the vegetation of Great Smoky Mountains National Park.", color=pal[4], x=0.025, y=0.85, family=font,  size=50, hjust=0) + 
  draw_text(text="Twitter: @BlakeRobMills | National Park Service | GitHub: BlakeRMills", color=pal[4], x=0.5, y=0.02, size=50, fontface = "bold", family = font) +
  draw_text(text= "Cove", x=0.055, y=0.1325, color=pal[1], family=font, fontface="bold", size=45) +
  draw_text(text= "Dead", x=0.12925, y=0.1325, color=pal[2], family=font, fontface="bold", size=45) +
  draw_text(text= "Grass", x=0.2035, y=0.1325, color=pal[3], family=font, fontface="bold", size=45) +
  draw_text(text= "Heath", x=0.27775, y=0.1325, color=pal[4], family=font, fontface="bold", size=45) +
  draw_text(text= "Hemlock", x=0.352, y=0.1325, color=pal[5], family=font, fontface="bold", size=45) +
  draw_text(text= "High\nHardwood", x=0.42625, y=0.1225, color=pal[6], family=font, fontface="bold", size=45, lineheight=0.3) +
  draw_text(text= "Montane\nAlluvial", x=0.5005, y=0.1225, color=pal[7], family=font, fontface="bold", size=45, lineheight=0.3) +
  draw_text(text= "Oak", x=0.57475, y=0.1325, color=pal[8], family=font, fontface="bold", size=45) +
  draw_text(text= "Pine/Oak", x=0.649, y=0.1325, color=pal[9], family=font, fontface="bold", size=45) +
  draw_text(text= "Rock", x=0.72325, y=0.1325, color=pal[10], family=font, fontface="bold", size=45) +
  draw_text(text= "Spruce/Fir", x=0.7975, y=0.1325, color=pal[11], family=font, fontface="bold", size=45) +
  draw_text(text= "Success.", x=0.87175, y=0.1325, color=pal[12], family=font, fontface="bold", size=45) +
  draw_text(text= "Water", x=0.94600, y=0.1325, color=pal[13], family=font, fontface="bold", size=45) 
  
ggsave("~/30DayMapChallenge/2023/Day 20 - Outdoors/Day 20 - Outdoors.png", height = 11, width = 14)
