# Libraries
library(cowplot)
library(ggtext)
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
pal <- moma.colors("Liu", 3)
sf_use_s2(FALSE)
city_name <- "Lagos"

# Cairo - 1992, 2003, 2013
# Kinshasa  - 1994, 2000, 2013
# Lagos - 1984, 2000, 2013

# Margins for the subtitle to align properly
l_margin_t <- -0.25 # Lagos
l_margin_b <- 0.25 # Lagos
k_margin_t <- -1 # Kinshasa
k_margin_b <- 2 # Kinshasa
c_margin_t <- 0.5 # Cairo
c_margin_b <- 0 # Cairo

# Data
city_1 <- st_read(paste("~/30DayMapChallenge/2023/Day 08 - Africa/Data/", city_name, "/urban_edge_t1.shp", sep="")) %>% 
  st_transform(crs =  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
city_2 <- st_read(paste("~/30DayMapChallenge/2023/Day 08 - Africa/Data/", city_name, "/urban_edge_t2.shp", sep="")) %>% 
  st_transform(crs =  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
city_3 <- st_read(paste("~/30DayMapChallenge/2023/Day 08 - Africa/Data/", city_name, "/urban_edge_t3.shp", sep="")) %>% 
  st_transform(crs =  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

city_roads <- opq(bbox=st_bbox(city_3)) %>%
  add_osm_features(features = list(
    "highway" = "primary",
    "highway" = "secondary",
    "highway" = "tertiary",
    "highway" = "motorway",
    "highway" = "residential",
    "highway" = "road")) %>%
  osmdata_sf() %>% 
  .$osm_lines %>%
  select(geometry) %>% 
  st_transform(crs =  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

city_1_r <- st_intersection(city_roads, city_1)
city_2_r <- st_intersection(city_roads, city_2)
city_3_r <- st_intersection(city_roads, city_3)

# Plot

plot <- ggplot() +
  geom_sf(data=city_3_r, color=pal[1], alpha=1, linewidth=0.1) +
  geom_sf(data=city_2_r, color=pal[2], alpha=1, linewidth=0.1) +
  geom_sf(data=city_1_r, color=pal[3], alpha=1, linewidth=0.1) +
  theme_void() +
  ggtitle(paste("Map displays the roads of ", city_name, " colored by the urban extent of city in <span style = 'color:#97c124'>**1994**</span>,  <span style = 'color:#9b5c1c'>**2000**</span>, and <span style = 'color:#9fd7bd'>**2013**</span>.", sep="")) +
  theme(plot.margin = margin(0, 0, -2, 0, "cm"),
        plot.title = element_textbox(hjust=0.5, color="white", size=28, family=font, lineheight = 0.4, margin = margin(k_margin_t, 0, k_margin_b, 0, "cm")))

ggdraw(plot) +
  theme(plot.background = element_rect(fill="#111509", color="#111509")) +
  draw_text(x=0.5, y=0.925, hjust=0.5, text=city_name, size=250, family = font, fontface="bold", color = pal[3]) +
  draw_label(label="Twitter: @BlakeRobMills | Source: Atlas of Urban Expansion & Open Street Maps | GitHub: BlakeRMills", color=pal[3], x=0.5, y=0.0225, size=28, fontface = "bold", fontfamily = font) 


ggsave(paste("~/30DayMapChallenge/2023/Day 08 - Africa/", city_name, ".png", sep=""), width = 7, height = 7)

