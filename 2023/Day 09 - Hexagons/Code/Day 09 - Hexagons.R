# Libraries
library(cowplot)
library(geojsonio)
library(MoMAColors)
library(osmdata)
library(tidyverse)
library(sf)
library(showtext)
library(sysfonts)

# Aes
showtext_auto()
font <- "Advent Pro"
font_add_google(font)
sf_use_s2(FALSE)

# Data
Bars <- opq(bbox="New York, New York") %>%
  add_osm_features(features = list("amenity" = "bar",
                                   "amenity" = "pub")) %>%
  osmdata_sf() %>% 
  .$osm_points

Man <- geojson_sf("~/30DayMapChallenge/2023/Day 01 - Points/Data/Borough Boundaries.geojson") 

Central <- geojson_sf("~/30DayMapChallenge/2023/Day 01 - Points/Data/Parks Properties_20231101.geojson") %>%
  filter(name311 == "Central Park")

# Cleaning

Man_Hex <- Man %>% 
  filter(boro_name == "Manhattan") %>%
  st_cast("POLYGON") %>%
  mutate(Area = st_area(.)) %>%
  arrange(-Area) %>%
  .[1, ]

Man_Plot <- st_intersection(st_make_grid(Man_Hex, square = F, n = 25), Man_Hex) %>%
  st_as_sf

Man_Plot$Bars <- st_intersects(Man_Plot, Bars) %>% lengths()

# Plot
plot <- ggplot() +
  geom_sf(data = Man_Plot, aes(fill = Bars), color="transparent") +
  geom_sf(data = Central, fill="grey12", color="transparent") +
  scale_fill_moma_c("Exter", direction = -1) +
  theme_void() +
  theme(plot.margin = margin(0.75, 0, 0.5, 0, "cm"),
        legend.position = c(0.95, 0.25),
        legend.key.height = unit(1.25, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(color="white", size=35, margin = margin(0, 0, 0, -0.4, "cm"), family = font)) +
  guides(fill = guide_colorbar(ticks.colour = "transparent"))

ggdraw(plot) +
  theme(plot.background = element_rect(fill="grey12", color="grey12"))+
  draw_label(label="Twitter: @BlakeRobMills | Source: Open Street Maps | GitHub: BlakeRMills", color=moma.colors("Exter")[1], x=0.5, y=0.0175, size=30, fontface = "bold", fontfamily = font) + 
  draw_text(x=0.05, y=0.875, text="Mahattan\nBars", family=font, color=moma.colors("Exter")[1], fontface="bold", size=200, hjust=0, lineheight=0.3) +
  draw_text(x=0.05, y=0.75, text="Map display the number of bars in Manhattan.", color=moma.colors("Exter")[1], fontface="bold", family=font, size=35, hjust=0) +
  draw_text(x=0.7, y=0.25, text="Number of Bars", color=moma.colors("Exter")[1], fontface="bold", family=font, size=50, hjust=0.5, angle=90)

ggsave("~/30DayMapChallenge/2023/Day 09 - Hexagons/Day 09 - Hexagons.png", height = 9, width = 6)

