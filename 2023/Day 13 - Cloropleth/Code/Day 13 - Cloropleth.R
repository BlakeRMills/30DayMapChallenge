# Libraries
library(cowplot)
library(geojsonio)
library(MetBrewer)
library(osmdata)
library(sf)
library(showtext)
library(sysfonts)
library(tidyverse)

# Aes
showtext_auto()
font <- "Advent Pro"
font_add_google(font)
set_overpass_url('http://overpass-api.de/api/interpreter')

# Data 
Rents <- geojson_sf("~/30DayMapChallenge/2023/Day 13 - Cloropleth/Data/Manhattan Rent 1940 Con Ed.geojson") 

Man <- geojson_sf("~/30DayMapChallenge/2023/Day 01 - Points/Data/Borough Boundaries.geojson") %>% 
  filter(boro_name == "Manhattan") %>%
  st_cast("POLYGON") %>%
  mutate(Area = st_area(.)) %>%
  arrange(-Area) %>%
  .[c(1,5), ] %>%
  st_combine()

Roads <- opq(bbox="New York, New York") %>%
  add_osm_features(features = list(
    "highway" = "primary",
    "highway" = "secondary",
    "highway" = "tertiary",
    "highway" = "motorway",
    "highway" = "residential",
    "highway" = "road")) %>%
  osmdata_sf() %>% 
  .$osm_lines %>%
  select(geometry)

# Cleaning 
Roads <- st_intersection(Roads, Man)

Rents <- Rents %>%
  mutate(Rent = factor(Rent,
                       levels = c("$0 to $19", "$20 to $29", "$30 to $39", "$40 to $49", 
                                  "$50 to $74", "$75 to $99", "$100 and over")))

# Map
p <- ggplot() +
  geom_sf(data=Roads, color="grey50", alpha=0.5) +
  geom_sf(data=Rents, aes(fill = Rent), color="transparent") +
  scale_fill_met_d("Renoir", direction = -1) +
  theme_void() +
  theme(legend.position = c(0.20, 0.675),
        legend.text = element_text(color="white", size=50, family=font, margin = margin(-0.2, 0, -0.2, 0, "cm")),
        legend.key.width = unit(2, "cm")) +
  guides(fill = guide_legend(override.aes = list(color="grey15")))

ggdraw(p) +
  theme(plot.background = element_rect(fill="grey15")) +
  draw_text(text="Manhattan Monthly\nRent in 1940", x=0.04, y=0.925, fontface="bold", family=font, size=190, color="white", hjust=0, lineheight = 0.3) +
  draw_text(text="Map displays Manhattan blocks colors by the average monthly rent in 1940.", x=0.04, y=0.84, family=font, size=45, color="white", hjust=0, lineheight = 0.3) +
  draw_text(text="Average Monthly Rent", x=0.05, y=0.67, family=font, fontface="bold", size=80, color="white", angle = 90, hjust=0.5) +
  draw_label(label="Twitter: @BlakeRobMills | Source: Data from ConEd (1940), Map through BrooklynHistory.org | GitHub: BlakeRMills", color="white", x=0.5, y=0.0175, size=50, fontface = "bold", fontfamily = font) 

ggsave("~/30DayMapChallenge/2023/Day 13 - Cloropleth/Day 13 - Cloropleth.png", width = 11, height = 14)
  