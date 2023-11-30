# Libraries
library(cowplot)
library(geojsonio)
library(MoMAColors)
library(osmdata)
library(sf)
library(showtext)
library(sysfonts)
library(tidycensus)
library(tidyverse)

# Aes
showtext_auto()
font <- "Advent Pro"
font_add_google(font)
sf_use_s2(FALSE)

# Data
squ <- read_csv("~/Downloads/2018_Central_Park_Squirrel_Census_-_Squirrel_Data_20231129.csv") %>%
  st_as_sf(wkt = "Lat/Long", crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

water <- opq(bbox=st_bbox(squ)) %>%
  add_osm_features(features = list(
    "water" = "river",
    "water" = "canal",
    "water" = "reservoir")) %>%
  osmdata_sf() %>%
  .$osm_polygons %>% 
  select(name, geometry)

# Cleaning
grid <-  st_make_grid(squ, n=c(32, 50)) %>%
  st_as_sf()

grid <- st_as_sf(grid)[squ, ] %>%
  mutate(center = st_centroid(x))

grid$squ_count <- st_intersects(grid, squ %>% st_intersection(., Man)) %>% 
  lengths()

# Map 
m <- ggplot() +
  geom_sf(data=water, fill="grey25", color="transparent", alpha=0.75) +
  geom_sf(data=grid, aes(geometry = center, size=squ_count, color=squ_count)) + 
  scale_color_moma_c("OKeeffe", direction = - 1, limits=c(0, 30)) +
  scale_size_continuous(range=c(2.5, 8)) +
  theme_void() +
  theme(legend.position = c(0.85, 0.3),
        legend.key.width = unit(1.75, "cm"),
        legend.key.height = unit(2.25, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(color="white", hjust = 0.5, size=80, family=font, margin=margin(0, 0, 0, -2.5, "cm"), face="bold")) +
  guides(size = guide_none())

ggdraw(m) +
  theme(plot.background = element_rect(color="transparent", fill="grey10")) +
  draw_text(text="Central Park\nSquirrel Census", x=0.05, y=0.915, hjust=0, size=200, color="white", family=font, fontface="bold", lineheight = 0.3) +
  draw_text(text="Jacqueline Kennedy\nOnassis Reservoir", x=0.5775, y=0.595, hjust=0.5, size=50, color="white", family=font, lineheight = 0.3) +
  draw_text(text="Map displays the number of squirrels in Central Park (2018).\nDots are sized and colored to represent the number of\nsquirrels in the surrounding area.", 
            x=0.05, y=0.805, hjust=0, size=50, color="white", family=font, lineheight = 0.325) +
  draw_text(text="Number of Squirrels", color="white", x=0.73, y=0.3, size=75, fontface = "bold", family = font, angle=90) +
  draw_text(text="Twitter: @BlakeRobMills | Central Park Squirrel Census | GitHub: BlakeRMills", color="white", x=0.5, y=0.015, size=45, fontface = "bold", family = font) 

ggsave("~/30DayMapChallenge/2023/Day 29 - Population/Day 29 - Population.png", height = 14, width = 11) 

