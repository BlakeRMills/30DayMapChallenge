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
font <- "Advent Pro"
font_add_google(font)
showtext_auto()

# Data
NYC <- geojson_sf("~/30DayMapChallenge/2023/Day 01 - Points/Data/Borough Boundaries.geojson") 
Man <- NYC %>% 
  filter(boro_name == "Manhattan") %>%
  st_cast("POLYGON") %>%
  mutate(Area = st_area(.)) %>%
  arrange(-Area) %>%
  .[c(1,5), ] %>%
  st_combine()

city_roads <- opq(bbox="New York") %>%
  add_osm_features(features = list(
    "highway" = "primary",
    "highway" = "secondary",
    "highway" = "tertiary",
    "highway" = "motorway",
    "highway" = "residential")) %>%
  osmdata_sf() %>% 
  .$osm_lines %>%
  mutate(feature = highway)

water <- opq(bbox="New York") %>%
  add_osm_features(features = list(
    "water" = "river",
    "water" = "canal",
    "water" = "reservoir")) %>%
  osmdata_sf() 

water <- rbind(water %>% .$osm_polygons %>% dplyr::select(name, geometry),
               water %>% .$osm_multipolygons %>% dplyr::select(name, geometry)) %>%
  filter(st_is_valid(.)==T) %>%
  st_union() 

# Cleaning
mn_roads <- st_intersection(city_roads, Man)
bk_roads <- st_intersection(city_roads, NYC %>% filter(boro_name == "Brooklyn"))
bx_roads <- st_intersection(city_roads, NYC %>% filter(boro_name == "Bronx"))
qn_roads <- st_intersection(city_roads, NYC %>% filter(boro_name == "Queens"))
si_roads <- st_intersection(city_roads, NYC %>% filter(boro_name == "Staten Island"))

mn_water <- st_intersection(water, Man)
bk_water <- st_intersection(water, NYC %>% filter(boro_name == "Brooklyn"))
bx_water <- st_intersection(water, NYC %>% filter(boro_name == "Bronx"))
qn_water <- st_intersection(water, NYC %>% filter(boro_name == "Queens"))
si_water <- st_intersection(water, NYC %>% filter(boro_name == "Staten Island"))

# Map 
## Manhattan
m <- ggplot() + 
  geom_sf(data=mn_roads, color="white", alpha=0.9, size=0.5) +
  geom_sf(data=mn_water, fill="white", color="transparent", alpha=0.9) +
  theme_void() 

ggdraw(m) + 
  draw_text(text="M\nA\nN\nH\nA\nT\nT\nA\nN", x=0.05, y=0.5, lineheight=0.4, color="white", size =225, family=font) +   
  draw_text(text="Twitter: @BlakeRobMills | Source: OpenStreetMap | GitHub: BlakeRMills",color="white",  x=0.44, y=0.01, size=45, family=font) + 
  theme(plot.background = element_rect(fill="black", color="transparent"), 
        plot.margin = margin(0, -1.5, 0, 1.5, unit="cm"))

ggsave("~/30DayMapChallenge/2023/Day 24 - Black and White/Day 24 - Black and White - Manhattan.png", height = 14, width=9)

## Brooklyn
m <- ggplot() + 
  geom_sf(data=bk_roads, color="white", alpha=0.9, size=0.5) +
  geom_sf(data=bk_water, fill="white", color="transparent", alpha=0.9) +
  theme_void() 

ggdraw(m) + 
  draw_text(text="B\nR\nO\nO\nK\nL\nY\nN", x=-0.05, y=0.5, lineheight=0.4, color="white", size =200, family=font) +   
  draw_text(text="Twitter: @BlakeRobMills | Source: OpenStreetMap | GitHub: BlakeRMills",color="white",  x=0.44, y=0.01, size=40, family=font) + 
  theme(plot.background = element_rect(fill="black", color="transparent"),
        plot.margin = margin(0, 0, 0, 3, unit="cm")
        )

ggsave("~/30DayMapChallenge/2023/Day 24 - Black and White/Day 24 - Black and White - Brooklyn.png", height = 11, width=10)


## Queens
m <- ggplot() + 
  geom_sf(data=qn_roads, color="white", alpha=0.9, size=0.5) +
  geom_sf(data=qn_water, fill="white", color="transparent", alpha=0.9) +
  theme_void() 

ggdraw(m) + 
  draw_text(text="Q\nU\nE\nE\nN\nS", x=-0.05, y=0.5, lineheight=0.525, color="white", size =200, family=font) +   
  draw_text(text="Twitter: @BlakeRobMills | Source: OpenStreetMap | GitHub: BlakeRMills",color="white",  x=0.44, y=0.01, size=40, family=font) + 
  theme(plot.background = element_rect(fill="black", color="transparent"),
        plot.margin = margin(0, 0, 0, 3, unit="cm")
  )

ggsave("~/30DayMapChallenge/2023/Day 24 - Black and White/Day 24 - Black and White - Queens.png", height = 11, width=10)

## Bronx
m <- ggplot() + 
  geom_sf(data=bx_roads, color="white", alpha=0.9, size=0.5) +
  geom_sf(data=bx_water, fill="white", color="transparent", alpha=0.9) +
  theme_void() 

ggdraw(m) + 
  draw_text(text="B\nR\nO\nN\nX", x=-0.075, y=0.5, lineheight=0.55, color="white", size =200, family=font) +   
  draw_text(text="Twitter: @BlakeRobMills | Source: OpenStreetMap | GitHub: BlakeRMills",color="white",  x=0.44, y=0.01, size=40, family=font) + 
  theme(plot.background = element_rect(fill="black", color="transparent"),
        plot.margin = margin(0, 0, 0, 3.5, unit="cm")
  )

ggsave("~/30DayMapChallenge/2023/Day 24 - Black and White/Day 24 - Black and White - Bronx.png", height = 11, width=10)

## Staten Island
m <- ggplot() + 
  geom_sf(data=si_roads, color="white", alpha=0.9, size=0.5) +
  geom_sf(data=si_water, fill="white", color="transparent", alpha=0.9) +
  theme_void() 

ggdraw(m) + 
  draw_text(text="S\nT\nA\nT\nE\nN", x=-0.04, y=0.5, lineheight=0.55, color="white", size =200, family=font) +   
  draw_text(text="I\nS\nL\nA\nN\nD", x=1.04, y=0.5, lineheight=0.55, color="white", size =200, family=font) +  
  draw_text(text="Twitter: @BlakeRobMills | Source: OpenStreetMap | GitHub: BlakeRMills",color="white",  x=0.44, y=0.01, size=40, family=font) + 
  theme(plot.background = element_rect(fill="black", color="transparent"),
        plot.margin = margin(0, 2.5, 0, 2.5, unit="cm")
  )

ggsave("~/30DayMapChallenge/2023/Day 24 - Black and White/Day 24 - Black and White - Staten Island.png", height = 11, width=10)
