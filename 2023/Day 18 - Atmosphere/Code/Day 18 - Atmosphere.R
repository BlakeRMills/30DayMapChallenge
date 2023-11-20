# Libraries
library(cowplot)
library(MoMAColors)
library(geojsonio)
library(osmdata)
library(raster)
library(sf)
library(showtext)
library(sysfonts)
library(tidyverse)

# Aes
sf_use_s2(FALSE)
showtext_auto()
font <- "Advent Pro"
font_add_google(font)

# Data
pm25 <- raster("~/Desktop/aa13_pm300m/w001001.adf") %>%
  rasterToPolygons() %>% 
  st_as_sf()

NYC <- geojson_sf("~/30DayMapChallenge/2023/Day 01 - Points/Data/Borough Boundaries.geojson")
Man <- NYC %>%
  filter(boro_name == "Manhattan") %>%
  st_cast("POLYGON") %>%
  mutate(Area = st_area(.)) %>%
  arrange(-Area) %>%
  .[c(1,5), ] %>%
  st_combine()

Roads <- opq(bbox=st_bbox(NYC)) %>%
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
pm25_manhattan <- pm25 %>%
  st_transform(crs =  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  rename(c("predicted" = "w001001")) %>%
  st_intersection(., Man) %>%
  st_intersection(Roads, .)

pm25_brooklyn <- pm25 %>%
  st_transform(crs =  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  rename(c("predicted" = "w001001")) %>%
  st_intersection(., NYC %>% filter(boro_name == "Brooklyn")) %>%
  st_intersection(Roads, .)

pm25_queens <- pm25 %>%
  st_transform(crs =  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  rename(c("predicted" = "w001001")) %>%
  st_intersection(., NYC %>% filter(boro_name == "Queens")) %>%
  st_intersection(Roads, .)

pm25_bronx <- pm25 %>%
  st_transform(crs =  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  rename(c("predicted" = "w001001")) %>%
  st_intersection(., NYC %>% filter(boro_name == "Bronx")) %>%
  st_intersection(Roads, .)

pm25_staten <- pm25 %>%
  st_transform(crs =  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  rename(c("predicted" = "w001001")) %>%
  st_intersection(., NYC %>% filter(boro_name == "Staten Island")) %>%
  st_intersection(Roads, .)

# Map
## Manhattan
plot_mn <- ggplot(data=pm25_manhattan) +
  geom_sf(aes(color=predicted)) + 
  scale_color_moma_c("OKeeffe", direction = -1) +
  theme_void() +
  theme(legend.title = element_blank(),
        plot.background =element_rect(fill="grey5", color="grey5"),
        legend.position = c(0.925, 0.3),
        legend.key.width = unit(1.75, "cm"),
        legend.key.height = unit(2.25, "cm")) 

final <- ggdraw(plot_mn) +
  theme(plot.background = element_rect(fill="grey5", color="grey5"),
        plot.margin = margin(0, 0.5, 0, 0, "cm")) +
  draw_label(label = "Manhattan Fine\nParticulate Matter", x=0.05, y=0.91, hjust=0, size=175, color="white", fontfamily=font, fontface="bold", lineheight = 0.3) +
  draw_text(text="7", x=0.795, y=0.1925, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="8", x=0.795, y=0.2775, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="9", x=0.795, y=0.3625, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="10", x=0.795, y=0.4475, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="Predicted Annual Average PM2.5 (ug/m3)", x=0.695, y=0.3, family=font, fontface="bold", color="white", hjust=0.5, size=55, angle=90) +
  draw_label(label = "Map displays Manhattan streets colored by the\npredicted annual average of fine particulate matter (PM2.5).\n(Dec 2020 - Dec 2021)", 
             x=0.05, y=0.8, hjust=0, size=55, color="white", fontfamily=font, lineheight = 0.4) + 
  draw_label(label="Twitter: @BlakeRobMills | Source: NYC Department of Health and Mental Hygiene | GitHub: BlakeRMills", 
             color="white", x=0.5, y=0.015, size=40, fontface = "bold", fontfamily = font)  

ggsave("~/30DayMapChallenge/2023/Day 18 - Atmosphere/Day 18 - Atmosphere - Manhattan.png", width = 11, height = 14)

## Brooklyn
plot_bk <- ggplot(data=pm25_brooklyn) +
  geom_sf(aes(color=predicted)) + 
  scale_color_moma_c("OKeeffe", direction = -1) +
  theme_void() +
  theme(legend.title = element_blank(),
        plot.background =element_rect(fill="grey5", color="grey5"),
        legend.position = c(0.435, -0.04),
        legend.key.width = unit(2.25, "cm"),
        legend.key.height = unit(1.25, "cm"),
        legend.direction = "horizontal",
        plot.margin = margin(4, -1, 4, 2, "cm")) 

final <- ggdraw(plot_bk) +
  theme(plot.background = element_rect(fill="grey5", color="grey5"),
        plot.margin = margin(0, 0.5, 0, 0, "cm")) +
  draw_label(label = "Brooklyn Fine\nParticulate Matter", x=0.05, y=0.92, hjust=0, size=175, color="white", fontfamily=font, fontface="bold", lineheight = 0.3) +
  draw_text(text="6", x=0.3, y=0.055, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="7", x=0.375, y=0.055, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="8", x=0.45, y=0.055, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="9", x=0.525, y=0.055, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="10", x=0.6, y=0.055, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="11", x=0.675, y=0.055, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="Predicted Annual Average PM2.5 (ug/m3)", x=0.5, y=0.12, family=font, fontface="bold", color="white", hjust=0.5, size=55) +
  draw_label(label = "Map displays Brooklyn streets colored by the\npredicted annual average of fine particulate\nmatter (PM2.5). (Dec 2020 - Dec 2021)", 
             x=0.05, y=0.8, hjust=0, size=55, color="white", fontfamily=font, lineheight = 0.4) + 
  draw_label(label="Twitter: @BlakeRobMills | Source: NYC Department of Health and Mental Hygiene | GitHub: BlakeRMills", 
             color="white", x=0.5, y=0.015, size=40, fontface = "bold", fontfamily = font)  

ggsave("~/30DayMapChallenge/2023/Day 18 - Atmosphere/Day 18 - Atmosphere - Brooklyn.png", width = 11, height = 14)

## Queens
plot_qn <- ggplot(data=pm25_queens) +
  geom_sf(aes(color=predicted)) + 
  scale_color_moma_c("OKeeffe", direction = -1) +
  theme_void() +
  theme(legend.title = element_blank(),
        plot.background =element_rect(fill="grey5", color="grey5"),
        legend.position = c(0.05, 0.41),
        legend.key.width = unit(1.75, "cm"),
        legend.key.height = unit(2.25, "cm"),
        plot.margin = margin(6, -0.5, 0, 3.5, "cm")) 

final <- ggdraw(plot_qn) +
  theme(plot.background = element_rect(fill="grey5", color="grey5"),
        plot.margin = margin(0, 0.5, 0, 0, "cm")) +
  draw_label(label = "Queens Fine\nParticulate Matter", x=0.05, y=0.92, hjust=0, size=175, color="white", fontfamily=font, fontface="bold", lineheight = 0.3) +
  draw_text(text="6", x=0.22, y=0.188, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="8", x=0.22, y=0.277, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="10", x=0.22, y=0.366, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="12", x=0.22, y=0.455, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="Predicted Annual Average PM2.5 (ug/m3)", x=0.12, y=0.3375, family=font, fontface="bold", color="white", hjust=0.5, size=55, angle=90) +
  draw_label(label = "Map displays Queens streets colored by the\npredicted annual average of fine particulate matter (PM2.5).\n(Dec 2020 - Dec 2021)", 
             x=0.05, y=0.82, hjust=0, size=55, color="white", fontfamily=font, lineheight = 0.4) + 
  draw_label(label="Twitter: @BlakeRobMills | Source: NYC Department of Health and Mental Hygiene | GitHub: BlakeRMills", 
             color="white", x=0.5, y=0.015, size=40, fontface = "bold", fontfamily = font)  

ggsave("~/30DayMapChallenge/2023/Day 18 - Atmosphere/Day 18 - Atmosphere - Queens.png", width = 11, height = 14)

## The Bronx
plot_bx <- ggplot(data=pm25_bronx) +
  geom_sf(aes(color=predicted)) + 
  scale_color_moma_c("OKeeffe", direction = -1) +
  theme_void() +
  theme(legend.title = element_blank(),
        plot.background =element_rect(fill="grey5", color="grey5"),
        legend.position = c(0.65, 0.06),
        legend.direction = "horizontal", 
        legend.key.width = unit(2.25, "cm"),
        legend.key.height = unit(1.25, "cm"),
        plot.margin = margin(6, 0, 2, 0, "cm")) 

final <- ggdraw(plot_bx) +
  theme(plot.background = element_rect(fill="grey5", color="grey5"),
        plot.margin = margin(0, 0.5, 0, 0, "cm")) +
  draw_label(label = "Bronx Fine\nParticulate Matter", x=0.05, y=0.92, hjust=0, size=175, color="white", fontfamily=font, fontface="bold", lineheight = 0.3) +
  draw_text(text="7", x=0.56, y=0.0775, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="8", x=0.718, y=0.0775, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="Predicted Annual Average PM2.5 (ug/m3)", x=0.638, y=0.1425, family=font, fontface="bold", color="white", hjust=0.5, size=55) +
  draw_label(label = "Map displays Bronx streets colored by the predicted annual\naverage of fine particulate matter (PM2.5). (Dec 2020 - Dec 2021)", 
             x=0.05, y=0.83, hjust=0, size=55, color="white", fontfamily=font, lineheight = 0.4) + 
  draw_label(label="Twitter: @BlakeRobMills | Source: NYC Department of Health and Mental Hygiene | GitHub: BlakeRMills", 
             color="white", x=0.5, y=0.015, size=40, fontface = "bold", fontfamily = font)  

ggsave("~/30DayMapChallenge/2023/Day 18 - Atmosphere/Day 18 - Atmosphere - Bronx.png", width = 11, height = 14)

## Staten Island
plot_si <- ggplot(data=pm25_staten) +
  geom_sf(aes(color=predicted)) + 
  scale_color_moma_c("OKeeffe", direction = -1, breaks=c(6, 6.25, 6.5, 6.75)) +
  theme_void() +
  theme(legend.title = element_blank(),
        plot.background =element_rect(fill="grey5", color="grey5"),
        legend.position = c(0.7, 0.05),
        legend.key.width = unit(2.25, "cm"),
        legend.key.height = unit(1.5, "cm"),
        legend.direction = "horizontal",
        plot.margin = margin(5, 0, 0, 2, "cm")) 

final <- ggdraw(plot_si) +
  theme(plot.background = element_rect(fill="grey5", color="grey5"),
        plot.margin = margin(0, 0.5, 0, 0, "cm")) +
  draw_label(label = "Staten Island Fine\nParticulate Matter", x=0.05, y=0.92, hjust=0, size=175, color="white", fontfamily=font, fontface="bold", lineheight = 0.3) +
  draw_text(text="6", x=0.57, y=0.085, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="6.25", x=0.68, y=0.085, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="6.5", x=0.8, y=0.085, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="6.75", x=0.91, y=0.085, family=font, fontface="bold", color="white", hjust=0, size=55) +
  draw_text(text="Predicted Annual Average PM2.5 (ug/m3)", x=0.725, y=0.157, family=font, fontface="bold", color="white", hjust=0.5, size=55) +
  draw_label(label = "Map displays Staten Island streets colored by the predicted annual\naverage of fine particulate matter (PM2.5). (Dec 2020 - Dec 2021)", 
             x=0.05, y=0.83, hjust=0, size=55, color="white", fontfamily=font, lineheight = 0.4) + 
  draw_label(label="Twitter: @BlakeRobMills | Source: NYC Department of Health and Mental Hygiene | GitHub: BlakeRMills", 
             color="white", x=0.5, y=0.015, size=40, fontface = "bold", fontfamily = font)  

ggsave("~/30DayMapChallenge/2023/Day 18 - Atmosphere/Day 18 - Atmosphere - Staten Island.png", width = 11, height = 14)

