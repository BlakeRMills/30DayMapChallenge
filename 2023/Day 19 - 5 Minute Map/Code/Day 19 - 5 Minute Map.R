# Libraries
library(cowplot)
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
pal <- moma.colors("Liu")

# Data
Harlem <- geojson_sf("~/30DayMapChallenge/2023/Day 19 - 5 Minute Map/Data/NYC_Neighborhoods_BetaNYC.geojson") %>% 
  filter(neighborhood == "Harlem")

Roads <- opq(bbox=st_bbox(Harlem)) %>%
  add_osm_features(features = list(
    "highway" = "primary",
    "highway" = "secondary",
    "highway" = "tertiary",
    "highway" = "motorway",
    "highway" = "residential",
    "highway" = "road")) %>%
  osmdata_sf() %>% 
  .$osm_lines %>%
  select(geometry, highway)

parks <- opq(bbox=st_bbox(Harlem)) %>%
  add_osm_features(features = list(
    "leisure" = "garden",
    "leisure" = "park",
    "landuse" = "grass",
    "landuse" = "recreation_ground")) %>%
  osmdata_sf()  %>%
  .$osm_polygons


# Cleaning
Harlem_Streets <- st_intersection(Roads, Harlem)
Harlem_Parks <- st_intersection(parks, Harlem)

# Plot
p <- ggplot() + 
  geom_sf(data=Harlem_Streets, aes(color=highway), lwd=1.25) +
  geom_sf(data=Harlem_Parks, fill=pal[4], color="transparent") +
  scale_color_manual(values = c(pal[1:3], pal[5:6])) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(0.5, 0, 0.5, 0, "cm"))

ggdraw(p) +
  theme(plot.background =element_rect(fill="#fdf9f5", color="#fdf9f5")) + 
  draw_text(text="Harlem", x=0.05, y=0.95, size=275, family=font, fontface="bold", hjust=0, color=pal[2]) +
  draw_text(text="Twitter: @BlakeRobMills | Source: BetaNYC & OpenStreetMap | GitHub: BlakeRMills",color=pal[2], x=0.5, y=0.01, size=50, family=font, fontface = "bold")
  
ggsave("~/30DayMapChallenge/2023/Day 19 - 5 Minute Map/Day 19 - 5 Minute Map.png", height = 14, width = 11)
  