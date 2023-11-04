# Libraries
library(cowplot)
library(geojsonio)
library(MoMAColors)
library(osmdata)
library(RSocrata)
library(sf)
library(showtext)
library(sysfonts)
library(tidyverse)

# Aes
showtext_auto()
font <- "Sedgwick Ave"
font_add_google(font)

# Data
Graf <- read.socrata("https://data.cityofnewyork.us/resource/erm2-nwe9.csv?complaint_type=Graffiti")
Man <- geojson_sf("~/30DayMapChallenge/2023/Day 01 - Points/Data/Borough Boundaries.geojson")
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

grid <- st_make_grid(Roads, n = 400, square = F) %>%
  st_as_sf()

# Cleaning
sf_use_s2(FALSE)

Man <- Man %>% 
  filter(boro_name == "Manhattan") %>%
  st_cast("POLYGON") %>%
  mutate(Area = st_area(.)) %>%
  arrange(-Area) %>%
  .[1, ]

Graf_Clean <- Graf %>%
  filter(closed_date < Sys.Date(),
         created_date > Sys.Date() - years(10),
         is.na(latitude) == FALSE) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
  st_intersection(., Man)

grid$graf_points <- st_intersects(grid, Graf_Clean) %>% lengths()

grid_Roads <- st_intersection(Roads, Man) %>%
  st_intersection(., grid) %>%
  mutate(graf_points = ifelse(graf_points > 35, 35, graf_points))

# Plot

plot <- ggplot() + 
  geom_sf(data = grid_Roads, aes(color=graf_points), size=0.1) +
  scale_color_moma_c("OKeeffe") +
  theme_void() +
  guides(color=guide_colorbar(ticks.colour = "transparent")) +   
  theme(legend.position = c(0.9, 0.3),
        legend.key.width = unit(1.25, "cm"),
        legend.key.height = unit(1.5, "cm")) 

ggdraw(plot) +
  theme(plot.background = element_rect(fill="black"),
        plot.margin = margin(0, 0.5, 0, 0, "cm")) +
  draw_label(label="Twitter: @BlakeRobMills | Source: NYC Open Data | GitHub: BlakeRMills", color=moma.colors("OKeeffe")[1], x=0.5, y=0.0175, size=30, fontface = "bold", fontfamily = font) +
  draw_label(label = "A Bad Kids'\nGuide to\nManhattan", x=0.05, y=0.875, hjust=0, size=125, color=moma.colors("OKeeffe")[1], fontfamily=font, fontface="bold", lineheight = 0.3) +
  draw_label(label = "Areas yet to be graffitied", x=0.79, y=0.075, hjust=1, size=32,color=moma.colors("OKeeffe")[1], fontfamily=font) +
  draw_label(label = "Areas frequently graffitied", x=0.72, y=0.495, hjust=0, size=32,color=moma.colors("OKeeffe")[6], fontfamily=font) +
  draw_label(label = "Frequency of Graffiti", x=0.68, y=0.3, angle=90, size=50,color=moma.colors("OKeeffe")[1], fontfamily=font) +
  draw_label(label = "Plot displays Manhattan streets colored by\nthe number of 311 calls of reported graffiti\nin the last 10 years (2013-2023).", x=0.05, y=0.72, hjust=0, size=30, color=moma.colors("OKeeffe")[1], fontfamily=font, lineheight = 0.4) +
  annotate(geom = "curve", x=0.8, xend=0.81, y=0.075, yend=0.125, color=moma.colors("OKeeffe")[1], arrow = arrow(length = unit(3, "mm")), curvature = 0.85, linewidth=0.75) +
  annotate(geom = "curve", x=0.71, xend=0.7, y=0.495, yend=0.45, color=moma.colors("OKeeffe")[6], arrow = arrow(length = unit(3, "mm")), curvature = 0.85, linewidth=0.75)

ggsave("~/30DayMapChallenge/2023/Day 04 - A bad map/Day 04 - A bad map.png", height = 9, width = 6)
