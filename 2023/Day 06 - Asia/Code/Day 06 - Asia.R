# Libraries
library(cowplot)
library(ggrepel)
library(osmdata)
library(sf)
library(showtext)
library(sysfonts)
library(tidyverse)

# Aes
showtext_auto()
font <- "Advent Pro"
font_add_google(font)
pal <- c("#FFB3B3", "#FF9694", "#F15477", "#EB275F", "#D23049","#FEA13D", "#FFC180")

# Data
japan <-  st_read("~/Desktop/japan_ver84/japan_ver84.shp", 
                  crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

Roads <- opq(bbox=c(139.2929, 35.5233, 140.1389, 36.0868)) %>%
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
sf_use_s2(FALSE)

tokyo <-japan %>%
  filter(KEN == "東京都", 
         grepl("-ku", CITY_ENG)) %>%
  select(geometry, CITY_ENG) %>%
  mutate(label = str_remove_all(CITY_ENG, "-ku"))
  
tokyo_roads <- st_intersection(Roads, tokyo)

# Map
plot <- ggplot() + 
  geom_sf(data=tokyo_roads, aes(color = CITY_ENG), linewidth=0.35) +
  geom_label_repel(data=tokyo, aes(geometry=geometry, label = label, color=CITY_ENG),  stat = "sf_coordinates",
                   size=16, fontface="bold", family=font, label.padding = unit(0.04, "cm"), label.size=0, fill="#FFFAF5") +
  scale_color_manual(values=c( pal[4], # Adachi
                               pal[2], # Arakawa
                               pal[6], # Bunkyo
                               pal[7], # Chiyoda
                               pal[5], # Chuo
                               pal[6], # Edogawa
                               pal[5], # Itabashi
                               pal[1], # Katsushika
                               pal[7], # Kita
                               pal[3], # Koto
                               pal[4], # Mehuro
                               pal[2], # Minato
                               pal[6], # Nakano
                               pal[7], # Nerima
                               pal[1], # Ota
                               pal[5], # Setagaya
                               pal[3], # Shibuya
                               pal[6], # Shinagawa
                               pal[5], # Shinjuku
                               pal[2], # Suginami
                               pal[7], # Sumida
                               pal[3], # Taito
                               pal[3]  # Toshima
                               )) +
  theme_void() +
  theme(legend.position = "none") 

ggdraw(plot) +
  theme(plot.background = element_rect(fill="#FFFAF5", color="#FFFAF5"),
        plot.margin = margin(1.5, 0.25, 0, 0.25, "cm")) +
  draw_label(label="Tokyo", color=pal[6], x=0.025, y=1, size=250, fontface = "bold", fontfamily = font, hjust=0) + 
  draw_label(label="Twitter: @BlakeRobMills | Source: ESRI Japan & Open Street Maps | GitHub: BlakeRMills", color=pal[6], x=0.5, y=0.0175, size=35, fontface = "bold", fontfamily = font) 
  
ggsave("~/30DayMapChallenge/2023/Day 06 - Asia/Day 06 - Asia.png", height = 9, width = 8.5)
