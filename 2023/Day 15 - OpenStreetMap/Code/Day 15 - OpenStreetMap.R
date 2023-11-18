# Libraries
library(cowplot)
library(MetBrewer)
library(MoMAColors)
library(osmdata)
library(tidyverse)
library(sf)
library(showtext)
library(sysfonts)

# Aes
set_overpass_url('http://overpass-api.de/api/interpreter')
sf_use_s2(FALSE)
showtext_auto()
font <- "Advent Pro"
font_add_google(font)

# Data
## Borough Outline
Man <- geojson_sf("~/30DayMapChallenge/2023/Day 01 - Points/Data/Borough Boundaries.geojson") %>% 
  filter(boro_name == "Manhattan") %>%
  st_cast("POLYGON") %>%
  mutate(Area = st_area(.)) %>%
  arrange(-Area) %>%
  .[c(1,5), ] %>%
  st_combine()

## Roads 
city_roads <- opq(bbox="New York") %>%
  add_osm_features(features = list(
    "highway" = "primary",
    "highway" = "secondary",
    "highway" = "tertiary",
    "highway" = "motorway",
    "highway" = "residential")) %>%
  osmdata_sf() %>% 
  .$osm_lines %>%
  st_intersection(., Man) %>% 
  mutate(feature = highway)

## Parks
city_parks <- opq(bbox="New York") %>%
  add_osm_features(features = list(
    "leisure" = "garden",
    "leisure" = "park",
    "landuse" = "grass",
    "landuse" = "recreation_ground")) %>%
  osmdata_sf() 

city_parks <- rbind(city_parks %>% .$osm_polygons %>% select(name, geometry),
                    city_parks %>% .$osm_multipolygons %>% select(name, geometry)) %>%
  filter(st_is_valid(.)==T) %>%
  st_union() %>%
  st_intersection(., Man) 

## Water
water <- opq(bbox="New York") %>%
  add_osm_features(features = list(
    "water" = "river",
    "water" = "canal",
    "water" = "reservoir")) %>%
  osmdata_sf() 

water <- rbind(water %>% .$osm_polygons %>% select(name, geometry),
               water %>% .$osm_multipolygons %>% select(name, geometry)) %>%
  filter(st_is_valid(.)==T) %>%
  st_union() %>%
  st_intersection(., Man) 

## Other
letters <- "MANHATTAN" %>% str_split_1("")

# Plot
title_c <- c()
back_c <- c()
for(i in 1:9){
  pal <- met.brewer("Signac") %>% sample(8)
  while(pal[8] %in% back_c | pal[2] %in% title_c){
    pal <- met.brewer("Signac") %>% sample(8)
  }
  
  bottom <- ifelse(i >= 7, 0.75, 0)
  let_pos <- ifelse(i >= 7, 0.815, 0.8)
  p <- ggplot() +   
    geom_sf(data=city_parks, fill=pal[1], color="transparent") +
    geom_sf(data=water, fill=pal[2], color="transparent") +
    geom_sf(data=city_roads, aes(color=feature), lwd=0.2) + 
    scale_color_manual(values = pal[3:7]) +
    theme_void() +
    theme(legend.position = "none",
          plot.margin = margin(0, 0, bottom, 0, "cm")) 
  
  d <- ggdraw(p) +
    theme(plot.background = element_rect(fill=pal[8], color="transparent")) +
    draw_text(text=letters[i], x=0.2, y=let_pos, fontface="bold", family=font, size=1200, color=pal[2])
  
  assign(paste("fin", i, sep=""), d)
  title_c <- c(title_c, pal[2])
  back_c <- c(back_c, pal[8])
}

grid <- plot_grid(fin1, fin2, fin3, fin4, fin5, fin6, fin7, fin8, fin9,
                  nrow=3, ncol=3, rel_heights = c(14, 14, 14.9))

ggdraw(grid) +
  draw_label(label="Twitter: @BlakeRobMills | Source: OpenStreetMap | GitHub: BlakeRMills",color="white",  x=0.5, y=0.01, size=100, 
             fontface = "bold", fontfamily = font) 

ggsave("~/30DayMapChallenge/2023/Day 15 - OpenStreetMap/Day 15 - OpenStreetMap.png", height = 14, width = 11, dpi = 800)
