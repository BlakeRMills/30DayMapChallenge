# Libraries
library(sf)
library(cowplot)
library(elevatr)
library(tidyverse)
library(raster)
library(geojsonio)
library(showtext)
library(sysfonts)

# Aes
showtext_auto()
font <- "Advent Pro"
font_add_google(font)

# Data

get_elev <- function(Area){
  Country <- geojson_sf("~/Desktop/north-america.geojson") %>%
    filter(sovereignt == Area)
  
  Elev <- get_elev_raster(locations = Country, z = 2, clip = "bbox")
  Poly <- rasterToPolygons(Elev) %>% st_as_sf()
  colnames(Poly) <- c("Elevation", "geometry")
  Int <- st_intersection(Poly, Country)
  
  return(Int)
}

# Data
NorthAmerica <- geojson_sf("~/Desktop/north-america.geojson") %>%
  filter(sovereignt != "Denmark")



usa <- get_elev("United States of America")
mex <- get_elev("Mexico")
can <- get_elev("Canada")

NorthAmerica <- rbind(usa, mex, can) %>%
  filter(Elevation > -250)

# Map 

p <- ggplot() + 
  geom_sf(data = NorthAmerica, aes(fill = Elevation), color="transparent") +
  theme_void() + 
  scale_fill_moma_c("Exter", direction = -1) +
  theme(plot.background = element_rect(fill = "grey5"),
        legend.position = "none",
        plot.margin = margin(0, 0.5, 0, 0, "cm"))

ggdraw(p) +
  theme(plot.background = element_rect(fill = "grey5"),
        plot.margin = margin(0, 0, -1.5, 0, "cm")) +
  draw_label(label="North America", x=0.025, y=0.95, size=500, fontface = "bold", fontfamily = font, color = moma.colors("Exter")[1], hjust=0) +
  draw_label(label="Map shows the elevation of North America.", x=0.0285, y=0.895, size=100,  fontfamily = font, color = moma.colors("Exter")[1], hjust=0) +
  draw_label(label="Twitter: @BlakeRobMills | GitHub: BlakeRMills", color=moma.colors("Exter")[1], x=0.5, y=0.09, size=100, fontface = "bold", fontfamily = font)  
  
ggsave("~/30DayMapChallenge/2023/Day 10 - North America/Day 10 - North America.png", width = 7, height = 7.5, dpi=1000)

