library(osmdata)
library(sf)
library(tidyverse)
library(showtext)
library(cowplot)
library(sysfonts)

# Aes
showtext_auto()
font_add_google("Roboto")
font_add_google("Comfortaa")
font1 <- "Roboto"
font2 <- "Comfortaa"

allLanes <- function(Park, FilterName){
  # Park and Border
  poly <- getbb(Park, featuretype = "national_park", format_out = "polygon")
  poly <- if(FilterName== "Rocky Mountain National Park"){poly[[2]]}else{poly}
    
  border <-  opq(bbox=poly) %>%
    add_osm_feature(key="boundary", value="national_park") %>%
    osmdata_sf() %>%
    .$osm_multipolygons %>%
    dplyr::select(osm_id, name, geometry) %>%
    dplyr::filter(name==FilterName)
  
  
  # Hiking
  hike <- opq(bbox=poly) %>%
    add_osm_feature(key="route", value="hiking") %>%
    osmdata_sf() %>%
    .$osm_lines %>%
    dplyr::select(osm_id, name, geometry) 
  hikeInt <- st_intersection(hike, border) %>% mutate(shape="Hike")
  
  # Biking
  bike <-  opq(bbox=poly) %>%
    add_osm_features(c("\"route\"=\"bicycle\"",
                       "\"route\"=\"mtb\"",
                       "\"highway\"=\"cycleway\"")) %>%
    osmdata_sf() %>%
    .$osm_lines %>%
    dplyr::select(osm_id, name, geometry) 
  bikeInt <- st_intersection(bike, border) %>% mutate(shape="Bike")
  
  # Walking
  walk <-  opq(bbox=poly) %>%
    add_osm_feature(key="highway", value=c("path", "footway", "pedestrian")) %>%
    osmdata_sf() %>%
    .$osm_lines %>%
    dplyr::select(osm_id, name, geometry) 
  walkInt <- st_intersection(walk, border) %>% mutate(shape="Walk")
  
  #Water
  Water1 <- opq(bbox=poly) %>%
    add_osm_features(features=c("\"water\"=\"pond\"",
                                "\"water\"=\"lake\"",
                                "\"water\"=\"reservoir\"",
                                "\"water\"=\"river\"",
                                "\"water\"=\"canal\"",
                                "\"water\"=\"lock\"")) %>%
    osmdata_sf() 
  Water1 <- rbind(Water1 %>% .$osm_polygons %>% dplyr::select(osm_id, name, geometry),
                  Water1 %>% .$osm_multipolygons %>% dplyr::select(osm_id, name, geometry))
  Water2 <- opq(bbox=poly) %>%
    add_osm_features(features=c("\"waterway\"=\"river\"",
                                "\"waterway\"=\"stream\"",
                                "\"waterway\"=\"canal\"",
                                "\"landuse\"=\"reservoir\"",
                                "\"natural\"=\"water\"",
                                "\"natural\"=\"strait\"",
                                "\"natural\"=\"bay\""))  %>%
    osmdata_sf() 
  Water2 <- rbind(Water2 %>% .$osm_polygons %>% dplyr::select(osm_id, name, geometry),
                  Water2 %>% .$osm_multipolygons %>% dplyr::select(osm_id, name, geometry))
  Water <- rbind(Water2, Water2) %>% dplyr::filter(st_is_valid(.)==TRUE)
  waterInt <- st_intersection(Water, border) %>% mutate(shape="Water")
  
  #Other Roads
  other <-  opq(bbox=poly) %>%
    add_osm_feature(key="highway", value="secondary") %>%
    osmdata_sf() %>%
    .$osm_lines %>%
    dplyr::select(osm_id, name, geometry) 
  otherInt <- st_intersection(other, border) %>% mutate(shape="Other")
  
  # Final
  FinalLanes <- rbind(hikeInt, bikeInt, waterInt, otherInt, walkInt)
  
  Final <- list(border, FinalLanes)
  return(Final)
}

allLanesNB <- function(Park, FilterName){
  # Park and Border
  poly <- getbb(Park, featuretype = "national_park", format_out = "polygon")
  border <-  opq(bbox=poly) %>%
    add_osm_feature(key="boundary", value="national_park") %>%
    osmdata_sf() %>%
    .$osm_multipolygons %>%
    dplyr::select(osm_id, name, geometry) %>%
    dplyr::filter(name==FilterName)
  
  
  # Hiking
  hike <- opq(bbox=poly) %>%
    add_osm_feature(key="route", value="hiking") %>%
    osmdata_sf() %>%
    .$osm_lines %>%
    dplyr::select(osm_id, name, geometry) 
  hikeInt <- st_intersection(hike, border) %>% mutate(shape="Hike")
  
  # Walking
  walk <-  opq(bbox=poly) %>%
    add_osm_feature(key="highway", value=c("path", "footway", "pedestrian")) %>%
    osmdata_sf() %>%
    .$osm_lines %>%
    dplyr::select(osm_id, name, geometry) 
  walkInt <- st_intersection(walk, border) %>% mutate(shape="Walk")
  
  #Water
  Water1 <- opq(bbox=poly) %>%
    add_osm_features(features=c("\"water\"=\"pond\"",
                                "\"water\"=\"lake\"",
                                "\"water\"=\"reservoir\"",
                                "\"water\"=\"river\"",
                                "\"water\"=\"canal\"",
                                "\"water\"=\"lock\"")) %>%
    osmdata_sf() 
  Water1 <- rbind(Water1 %>% .$osm_polygons %>% dplyr::select(osm_id, name, geometry),
                  Water1 %>% .$osm_multipolygons %>% dplyr::select(osm_id, name, geometry))
  Water2 <- opq(bbox=poly) %>%
    add_osm_features(features=c("\"waterway\"=\"river\"",
                                "\"waterway\"=\"stream\"",
                                "\"waterway\"=\"canal\"",
                                "\"landuse\"=\"reservoir\"",
                                "\"natural\"=\"water\"",
                                "\"natural\"=\"strait\"",
                                "\"natural\"=\"bay\""))  %>%
    osmdata_sf() 
  Water2 <- rbind(Water2 %>% .$osm_polygons %>% dplyr::select(osm_id, name, geometry),
                  Water2 %>% .$osm_multipolygons %>% dplyr::select(osm_id, name, geometry))
  Water <- rbind(Water2, Water2) %>% dplyr::filter(st_is_valid(.)==TRUE)
  waterInt <- st_intersection(Water, border) %>% mutate(shape="Water")
  
  #Other Roads
  other <-  opq(bbox=poly) %>%
    add_osm_feature(key="highway", value="secondary") %>%
    osmdata_sf() %>%
    .$osm_lines %>%
    dplyr::select(osm_id, name, geometry) 
  otherInt <- st_intersection(other, border) %>% mutate(shape="Other")
  
  # Final
  FinalLanes <- rbind(hikeInt, waterInt, otherInt, walkInt)
  
  Final <- list(border, FinalLanes)
  return(Final)
}


Glacier <- allLanes("Glacier National Park, Montana", "Glacier National Park")
GT <- allLanes("Grand Teton National Park", "Grand Teton National Park")
GSM <- allLanes("Great Smoky Mountains National Park", "Great Smoky Mountains National Park")
RM <- allLanes("Rocky Mountain National Park, Colorado", "Rocky Mountain National Park")
Sequoia <- allLanesNB("Sequoia National Park, California", "Sequoia National Park")
Yellowstone <- allLanes("Yellowstone National Park", "Yellowstone National Park")
Yosemite <- allLanes("Yosemite National Park", "Yosemite National Park")
Zion <- allLanes("Zion National Park", "Zion National Park")

"#fdf9f5"

plotter <- function(df, parkname, titlesize){
  colors <- c("#a1c0c9", "#005d72", "#4e5f24", "#ebae35", "#ac3b13")
  plot <- ggplot() +
    geom_sf(data=df[[1]], color="#f1e2d9", fill="#f1e2d9") +
    geom_sf(data=df[[2]] %>% filter(shape == "Water"), color=colors[1], fill=colors[1], size=1.1) +
    geom_sf(data=df[[2]] %>% filter(shape == "Other"), color=colors[2], fill=colors[2], size=1.8, lineend="round") +
    geom_sf(data=df[[2]] %>% filter(shape == "Walk"), color=colors[3], fill=colors[3], size=1.8, lineend="round") +
    geom_sf(data=df[[2]] %>% filter(shape == "Bike"), color=colors[4], fill=colors[4], size=1.8, lineend="round") +
    geom_sf(data=df[[2]] %>% filter(shape == "Hike"), color=colors[5], fill=colors[5], size=1.8, lineend="round") +
    coord_sf() +
    theme_void() 

  draw <- ggdraw(plot) +
    theme(plot.margin = margin(7, 0.5, 1, 0.5, "cm"),
          plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
          panel.background = element_rect(fill="#fdf9f5", color="#fdf9f5")) +
    draw_label(label=parkname, x=0.5, y=1.17, size=titlesize, fontface = "bold", fontfamily = font2, color="#4a1304") +
    draw_label(label="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills", x=0.5, y=-0.01, size=50, fontface="bold", fontfamily = font2, color="#4a1304") +
    draw_label(label="Map shows the different ways of navigating the park through ", x=0.36, y=1.07, size=75, fontfamily = font1, color="#4a1304") +
    draw_label(label="Hiking,", x=0.712, y=1.07, size=80, fontface = "bold", fontfamily = font1, color=colors[5]) +
    draw_line(x=c(0.6748, 0.743), y=c(1.048, 1.048), color=colors[5], size=1.5, lineend="round") +
    draw_label(label="Biking,", x=0.795, y=1.07, size=80, fontface = "bold", fontfamily = font1, color=colors[4]) +
    draw_line(x=c(0.76, 0.823), y=c(1.048, 1.048), color=colors[4], size=1.5, lineend="round") +
    draw_label(label="Walking,", x=0.884, y=1.07, size=80, fontface = "bold", fontfamily = font1, color=colors[3]) +
    draw_line(x=c(0.842, 0.922), y=c(1.048, 1.048), color=colors[3], size=1.5, lineend="round") +
    draw_label(label="and", x=0.148, y=1.025, size=75, fontfamily = font1, color="#4a1304") +
    draw_label(label="General Use", x=0.2385, y=1.025, size=80, fontfamily = font1, fontface = "bold", color=colors[2]) +
    draw_line(x=c(0.176, 0.303), y=c(1.006, 1.006), color=colors[2], size=1.5, lineend="round") +
    draw_label(label="trails, roads, and paths.", x=0.427, y=1.025, size=75, fontfamily = font1, color="#4a1304") +
    draw_label(label="Bodies of Water", x=0.637, y=1.025, size=80, fontfamily = font1, fontface = "bold", color=colors[1]) +
    draw_line(x=c(0.551, 0.723), y=c(1.006, 1.006), color=colors[1], size=1.5, lineend="round") +
    draw_label(label="also displayed.", x=0.805, y=1.025, size=75, fontfamily = font1, color="#4a1304") 
    
    
  return(draw)
}

GlacierPlot <- plotter(Glacier, "Glacier National Park", 260)
ggsave("~/Desktop/Day 20 (Movement)/Maps/Glacier.png", height = 15, width=15)

GTPlot <- plotter(GT, "Grand Teton National Park", 220)
ggsave("~/Desktop/Day 20 (Movement)/Maps/GT.png", height = 15, width=15)

GSMPlot <- plotter(GSM, "Great Smoky Mountains", 240)
ggsave("~/Desktop/Day 20 (Movement)/Maps/GSM.png", height = 15, width=15)

RMPlot <- plotter(RM, "Rocky Mountains", 260)
ggsave("~/Desktop/Day 20 (Movement)/Maps/RM.png", height = 15, width=15)

Seqplot <- plotter(Sequoia, "Sequoia National Park", 260)
ggsave("~/Desktop/Day 20 (Movement)/Maps/Sequoia.png", height = 15, width=15)

YellowPlot <- plotter(Yellowstone, "Yellowstone National Park", 220)
ggsave("~/Desktop/Day 20 (Movement)/Maps/Yellowstone.png", height = 15, width=15)

YosemitePlot <- plotter(Yosemite, "Yosemite National Park", 240)
ggsave("~/Desktop/Day 20 (Movement)/Maps/Yosemite.png", height = 15, width=15)

ZionPlot <- plotter(Zion, "Zion National Park", 260)
ggsave("~/Desktop/Day 20 (Movement)/Maps/Zion.png", height = 15, width=15)


