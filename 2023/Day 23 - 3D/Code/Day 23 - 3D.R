# Libraries
library(cowplot)
library(geojsonio)
library(MoMAColors)
library(osmdata)
library(raster)
library(rayshader)
library(sf)
library(showtext)
library(sysfonts)
library(tidyverse)

# Aes
font <- "Advent Pro"
font_add_google(font)
showtext_auto()
showtext_end()

# Data
pop <- read_sf("~/Desktop/NYCDen.geojson") %>%
  st_transform(crs =  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# Map
p <- 
  ggplot() +
  geom_sf(data=nyc_den, aes(fill=population), color="transparent") +
    scale_fill_moma_c("ustwo", direction = -1) +
   # ggtitle("Population Density of\nNew York City") +
  theme_void() +
    theme(legend.position = "none", 
          plot.background = element_rect(fill="#fdf9f5", color="transparent")
        #  plot.title = element_text(size=20, hjust=0, face = "bold", lineheight = 0.4)
          )

plot_gg(p, multicore = TRUE,  width=8, height=6.285714, zoom=.75, sunangle = 220, theta = 30, phi = 50, solidcolor='#fdf9f5', 
        verbose = T, windowsize = c(1400, 866), scale = 500, raytrace = T, triangulate = T)
render_snapshot("~/30DayMapChallenge/2023/Day 23 - 3D/Day 23 - 3D", width=14, height = 11)
