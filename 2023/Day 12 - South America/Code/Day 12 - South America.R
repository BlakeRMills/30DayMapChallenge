# Libraries
library(cowplot)
library(geojsonio)
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
city_name <- "Buenos Aires"

# Buenos Aires - 1989, 2001, 2014
# Bogota - 1989, 2001, 2010
# Santiago - 1990, 2000, 2014
# Sao Paulo 1988, 2000, 2014

# Data
city_1 <- st_read(paste("~/30DayMapChallenge/2023/Day 12 - South America/Data/", city_name, "/urban_edge_t1.shp", sep="")) %>% 
  st_transform(crs =  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
city_2 <- st_read(paste("~/30DayMapChallenge/2023/Day 12 - South America/Data/", city_name, "/urban_edge_t2.shp", sep="")) %>% 
  st_transform(crs =  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
city_3 <- st_read(paste("~/30DayMapChallenge/2023/Day 12 - South America/Data/", city_name, "/urban_edge_t3.shp", sep="")) %>% 
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
## Buenos Aires
plot_BA <- ggplot() +
  geom_sf(data=city_3_r, color=pal[1], alpha=1, linewidth=0.05) +
  geom_sf(data=city_2_r, color=pal[2], alpha=1, linewidth=0.05) +
  geom_sf(data=city_1_r, color=pal[3], alpha=1, linewidth=0.05) +
  theme_void() +
  ggtitle(paste("Map displays the roads of ", city_name, " colored by the urban extent of city in <span style = 'color:#97c124'>**1989**</span>,  <span style = 'color:#9b5c1c'>**2001**</span>, and <span style = 'color:#9fd7bd'>**2014**</span>.", sep="")) +
  theme(plot.margin = margin(0.5, 0, 0.25, 0, "cm"),
        plot.title = element_textbox(hjust=0.5, color="white", size=28, family=font, lineheight = 0.4, margin = margin(2.5, 0, 0, 0, "cm")))

ggdraw(plot_BA) +
  theme(plot.background = element_rect(fill="#111509", color="#111509")) +
  draw_text(x=0.5, y=0.925, hjust=0.5, text=city_name, size=250, family = font, fontface="bold", color = pal[3]) +
  draw_label(label="Twitter: @BlakeRobMills | Source: Atlas of Urban Expansion & Open Street Maps | GitHub: BlakeRMills", color=pal[3], x=0.5, y=0.0225, size=28, fontface = "bold", fontfamily = font) 

ggsave(paste("~/30DayMapChallenge/2023/Day 12 - South America/", city_name, ".png", sep=""), width = 7, height = 7)

## Bogota
plot_Bog <- ggplot() +
  geom_sf(data=city_3_r, color=pal[1], alpha=1, linewidth=0.075) +
  geom_sf(data=city_2_r, color=pal[2], alpha=1, linewidth=0.075) +
  geom_sf(data=city_1_r, color=pal[3], alpha=1, linewidth=0.075) +
  theme_void() +
  ggtitle(paste("Map displays the roads of ", "Bogotá", " colored by the urban extent of city in <span style = 'color:#97c124'>**1989**</span>,  <span style = 'color:#9b5c1c'>**2001**</span>, and <span style = 'color:#9fd7bd'>**2010**</span>.", sep="")) +
  theme(plot.margin = margin(0.75, 0, 0.5, 0, "cm"),
        plot.title = element_textbox(hjust=0.5, color="white", size=28, family=font, lineheight = 0.4, margin = margin(2.5, 0, 0, 0, "cm")))

ggdraw(plot_Bog) +
  theme(plot.background = element_rect(fill="#111509", color="#111509")) +
  draw_text(x=0.5, y=0.925, hjust=0.5, text="Bogotá", size=250, family = font, fontface="bold", color = pal[3]) +
  draw_label(label="Twitter: @BlakeRobMills | Source: Atlas of Urban Expansion & Open Street Maps | GitHub: BlakeRMills", color=pal[3], x=0.5, y=0.0225, size=28, fontface = "bold", fontfamily = font) 

ggsave(paste("~/30DayMapChallenge/2023/Day 12 - South America/", city_name, ".png", sep=""), width = 7, height = 7)

## Santiago
plot_San <- ggplot() +
  geom_sf(data=city_3_r, color=pal[1], alpha=1, linewidth=0.075) +
  geom_sf(data=city_2_r, color=pal[2], alpha=1, linewidth=0.075) +
  geom_sf(data=city_1_r, color=pal[3], alpha=1, linewidth=0.075) +
  theme_void() +
  ggtitle(paste("Map displays the roads of ", city_name, " colored by the urban extent of city in <span style = 'color:#97c124'>**1990**</span>,  <span style = 'color:#9b5c1c'>**2000**</span>, and <span style = 'color:#9fd7bd'>**2014**</span>.", sep="")) +
  theme(plot.margin = margin(0.75, 0, 0.5, 0, "cm"),
        plot.title = element_textbox(hjust=0.5, color="white", size=28, family=font, lineheight = 0.4, margin = margin(2.5, 0, 0, 0, "cm")))

ggdraw(plot_San) +
  theme(plot.background = element_rect(fill="#111509", color="#111509")) +
  draw_text(x=0.5, y=0.925, hjust=0.5, text=city_name, size=250, family = font, fontface="bold", color = pal[3]) +
  draw_label(label="Twitter: @BlakeRobMills | Source: Atlas of Urban Expansion & Open Street Maps | GitHub: BlakeRMills", color=pal[3], x=0.5, y=0.0225, size=28, fontface = "bold", fontfamily = font) 

ggsave(paste("~/30DayMapChallenge/2023/Day 12 - South America/", city_name, ".png", sep=""), width = 7, height = 7)

## Sao Paulo
plot_SP <- ggplot() +
  geom_sf(data=city_3_r, color=pal[1], alpha=1, linewidth=0.05) +
  geom_sf(data=city_2_r, color=pal[2], alpha=1, linewidth=0.05) +
  geom_sf(data=city_1_r, color=pal[3], alpha=1, linewidth=0.05) +
  theme_void() +
  ggtitle(paste("Map displays the roads of ", "São Paulo", " colored by the urban extent of city in <span style = 'color:#97c124'>**1988**</span>,  <span style = 'color:#9b5c1c'>**2000**</span>, and <span style = 'color:#9fd7bd'>**2014**</span>.", sep="")) +
  theme(plot.margin = margin(1, 0, -0.5, 0, "cm"),
        plot.title = element_textbox(hjust=0.5, color="white", size=28, family=font, lineheight = 0.4, margin = margin(0, 0, 0, 0, "cm")))

ggdraw(plot_SP) +
  theme(plot.background = element_rect(fill="#111509", color="#111509")) +
  draw_text(x=0.5, y=0.925, hjust=0.5, text="São Paulo", size=250, family = font, fontface="bold", color = pal[3]) +
  draw_label(label="Twitter: @BlakeRobMills | Source: Atlas of Urban Expansion & Open Street Maps | GitHub: BlakeRMills", color=pal[3], x=0.5, y=0.0225, size=28, fontface = "bold", fontfamily = font) 

ggsave(paste("~/30DayMapChallenge/2023/Day 12 - South America/", city_name, ".png", sep=""), width = 7, height = 7)

