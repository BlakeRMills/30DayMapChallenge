# Libraries
library(cowplot)
library(geojsonio)
library(MetBrewer)
library(sf)
library(showtext)
library(sysfonts)
library(tidyverse)
library(vroom)

# Aes
showtext_auto()
font <- "Advent Pro"
font_add_google(font)

# Data
foot <- geojson_sf("~/Desktop/Building-2.geojson")
Man <- geojson_sf("~/30DayMapChallenge/2023/Day 01 - Points/Data/Borough Boundaries.geojson") 
pluto <- vroom("~/Desktop/pluto_23v1.csv") 

# Cleaning
sf_use_s2(FALSE)

pluto <- pluto %>% 
  select(latitude, longitude, bbl, unitsres) %>%
  mutate(bbl = as.character(bbl))

Man <- Man %>% 
  filter(boro_name == "Manhattan") %>%
  st_cast("POLYGON") %>%
  mutate(Area = st_area(.)) %>%
  arrange(-Area) %>%
  .[1, ]

Man_foot <- st_intersection(foot, Man)

Man_foot <- Man_foot %>%
  left_join(pluto, by = c("base_bbl" = "bbl")) %>%
  mutate(unitres_cap = ifelse(unitsres > 200, 200, unitsres)) %>%
  filter(is.na(unitres_cap)== FALSE,
         unitres_cap != 0 )

# Map
plot <- ggplot() + 
  geom_sf(data = Man_foot, aes(fill=unitres_cap), color="transparent") + 
  scale_fill_met_c("Hokusai3", direction = -1) +
  theme_void() +
  theme(legend.position = c(0.71, 0.15),
        legend.key.width = unit(1.25, "cm"),
        legend.key.height = unit(0.75, "cm"),
        legend.direction = "horizontal") 

ggdraw(plot) +
  theme(plot.background = element_rect(fill="grey8", color="grey8"),
        plot.margin = margin(1, 0, 0, 0, "cm")) +
  draw_label(label="Twitter: @BlakeRobMills | Source: NYC Open Data | GitHub: BlakeRMills", color=met.brewer("Hokusai3")[1], x=0.5, y=0.0175, size=30, fontface = "bold", fontfamily = font) +
  draw_label(label = "Residential Units", x=0.71, y=0.195, fontfamily = font, size=40, fontface="bold",  color=met.brewer("Hokusai3")[1]) +
  draw_label(label = "50", x=0.6075, y=0.13, fontfamily = font, size=35,  color=met.brewer("Hokusai3")[1]) +
  draw_label(label = "100", x=0.71, y=0.13, fontfamily = font, size=35,  color=met.brewer("Hokusai3")[1]) +
  draw_label(label = "150", x=0.8125, y=0.13, fontfamily = font, size=35,  color=met.brewer("Hokusai3")[1]) +
  draw_label(label = "200+", x=0.915, y=0.13, fontfamily = font, size=35,  color=met.brewer("Hokusai3")[1]) +
  draw_label(label = "Manhattan Residential Density", x=0.5, y=1, hjust=0.5, size=105, color=met.brewer("Hokusai3")[1], fontfamily=font, fontface="bold") +
  draw_label(label = "Plot displays the number of residential units per building in Manhattan.", x=0.035, y=0.96, hjust=0, size=32, color=met.brewer("Hokusai3")[1], fontfamily=font, fontface="bold")

ggsave("~/30DayMapChallenge/2023/Day 03 - Polygons/Day 03 - Polygons.png", height = 9, width = 6)

