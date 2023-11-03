# Libraries
library(cowplot)
library(ggrepel)
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

# Data
Neigh <- opq(bbox="New York, New York") %>%
  add_osm_feature(key="boundary", value="place") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  mutate(label_name = str_replace_all(name, " ", "\n"))

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
  
Roads_Neigh <- data.frame() 
for(i in 1:nrow(Neigh)){
  Cur_Neigh <- st_intersection(Roads, Neigh[i, ])
  Roads_Neigh <- rbind(Roads_Neigh, Cur_Neigh)
}

# Functions 
`%notin%` <- Negate(`%in%`)

# Cleaning 
darknames <- sort(Neigh$name)[46:55]
Neigh <- Neigh %>%
  mutate(label_back = ifelse(name %in% darknames, "dark", "light"))


# Map
set.seed(10)
plot <- ggplot() +
  geom_sf(data=Roads_Neigh, aes(color=name), alpha=0.8)  +
  geom_label_repel(data=Neigh, aes(label=label_name, geometry = geometry, color=name, fill = label_back), size=6.25, fontface="bold", family=font, 
                   lineheight=0.3, label.size=0, stat = "sf_coordinates", label.padding = unit(0.04, "cm")) +
  scale_fill_manual(values = c("grey60", "grey35")) + 
  scale_color_moma_d("vonHeyl") +
  theme_void() + 
  theme(legend.position = "none")

ggdraw(plot) +
  theme(plot.background = element_rect(fill="grey35", color="grey35")) +
  draw_label(label="Twitter: @BlakeRobMills | Source: Open Street Maps | GitHub: BlakeRMills", color=moma.colors("vonHeyl")[3], x=0.5, y=0.0175, size=30, fontface = "bold", fontfamily = font) +
  draw_label(label = "Manhattan", x=0.05, y=0.9, hjust=0, size=165, color=moma.colors("vonHeyl")[3], fontfamily=font, fontface="bold")


ggsave("~/Desktop/Day 02 - Lines.png", height = 9, width = 6)

