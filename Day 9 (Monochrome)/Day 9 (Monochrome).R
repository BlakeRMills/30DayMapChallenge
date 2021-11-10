# Libraries
library(osmdata)
library(tidyverse)
library(sf)
library(cowplot)
library(showtext)
library(sysfonts)

# Aes
showtext_auto()
Colors <- c("#0d0649", "#243e8a", "#3b5fa7", "#80b2d7", "#a7cae3")
font_add_google("Quicksand")
font_add_google("Fira Sans")
font1 <- "Fira Sans"
font2 <- "Quicksand"


# Border Definiton
## London
LondonBorder <- opq(bbox=c(-0.55, 51.2911, 0.25, 51.744)) %>%
  add_osm_feature(key="admin_level", value="6") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  select(osm_id, name, geometry) %>%
  filter(name %in% c("London", "City of London")) %>%
  st_union()

## Milan
MilanBorder <- opq(bbox="Milan, Italy") %>%
  add_osm_feature(key="admin_level", value="8") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  select(osm_id, name, geometry) %>%
  filter(name =="Comune di Milano") 

## New York
NycBorder <- opq(bbox="New York") %>%
  add_osm_feature(key="admin_level", value="5") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  select(osm_id, name, geometry) 

## Paris
ParisBorder <- opq(bbox="Paris, France") %>%
  add_osm_feature(key="admin_level", value="8") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  select(osm_id, name, geometry) %>%
  filter(name=="Paris")

## Sydney
SydneyBorder <- opq(bbox="Sydney, Australia") %>%
  add_osm_feature(key="admin_level", value="7") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  select(osm_id, name, geometry)


## Mexico City
MexicoCityBorder <- opq(bbox="Mexico City, Mexico") %>%
  add_osm_feature(key="admin_level", value="4") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  select(osm_id, name, geometry) %>%
  filter(name=="Ciudad de México")

## Berlin
BerlinBorder <- opq(bbox="Berlin, Germany") %>%
  add_osm_feature(key="admin_level", value="4") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  select(osm_id, name, geometry) %>%
  filter(name=="Berlin")


# Pedestrians
PedFun <- function(bbox, border){
  Pedestrian <- opq(bbox=bbox) %>%
    add_osm_features(features=c("\"highway\"=\"pedestrian\"",
                                "\"highway\"=\"footway\"",
                                "\"highway\"=\"path\"",
                                "\"highway\"=\"living_street\"",
                                "\"highway\"=\"residential\"",
                                "\"footway\"=\"sidewalk\"",
                                "\"footway\"=\"crossing\"",
                                "\"route\"=\"foot\""))  %>%
    osmdata_sf() %>%
    .$osm_lines %>%
    select(osm_id, name, geometry) %>%
    filter(st_is_valid(.)==TRUE)
  Pedestrian <- st_intersection(Pedestrian, border)
  return(Pedestrian)
}

BerlinPed <- PedFun(c(13.099, 52.6674, 13.7892, 52.337), BerlinBorder)
LondonPed <- PedFun(c(-0.55, 51.2911, 0.25, 51.744), LondonBorder)
#MexCityPed <- PedFun("Mexico City, Mexico", MexicoCityBorder)
MilanPed <- PedFun("Milan, Italy", MilanBorder)
NycPed <- PedFun("New York City", NycBorder)
#SydneyPed <- PedFun("Sydney, Australia", SydneyBorder)
ParisPed <- PedFun("Paris, France", ParisBorder)



# Subways
SubFun <- function(bbox, border){
  Subway <- opq(bbox=bbox) %>%
    add_osm_features(features=c("\"railway\"=\"light_rail\"",
                                "\"railway\"=\"subway\"",
                                "\"route\"=\"subway\"",
                                "\"railway\"=\"rail\"",
                                "\"railway\"=\"tram\"",
                                "\"route\"=\"railway\"",
                                "\"route\"=\"light_rail\""))  %>%
    osmdata_sf() %>%
    .$osm_lines %>%
    select(osm_id, name, geometry) %>%
    filter(st_is_valid(.)==TRUE)
  Subway <- st_intersection(Subway, border)
  return(Subway)
}

BerlinSub <- SubFun(c(13.099, 52.6674, 13.7892, 52.337), BerlinBorder)
LondonSub <- SubFun(c(-0.55, 51.2911, 0.25, 51.744), LondonBorder)
#MexCitySub <- SubFun("Mexico City, Mexico", MexicoCityBorder)
MilanSub <- SubFun("Milan, Italy", MilanBorder)
NycSub <- SubFun("New York City", NycBorder)
#SydneySub <- SubFun("Sydney, Australia", SydneyBorder)
ParisSub <- SubFun("Paris, France", ParisBorder)




# Greenspace 
GreenFun <- function(bbox, border){
  Greenspace1 <- opq(bbox=bbox) %>%
    add_osm_features(features=c("\"landuse\"=\"village_green\"",
                                "\"landuse\"=\"recreation_ground\"",
                                "\"landuse\"=\"forest\"",
                                "\"landuse\"=\"allotments\"",
                                "\"landuse\"=\"meadow\"",
                                "\"landuse\"=\"grass\""))  %>%
    osmdata_sf() 
  Greenspace1 <- rbind(Greenspace1 %>% .$osm_polygons %>% select(osm_id, name, geometry),
                       Greenspace1 %>% .$osm_multipolygons %>% select(osm_id, name, geometry))
  
  Greenspace2 <- opq(bbox=bbox) %>%
    add_osm_features(features=c("\"leisure\"=\"garden\"",
                                "\"leisure\"=\"park\"",
                                "\"natural\"=\"wood\"",
                                "\"natural\"=\"grassland\""))  %>%
    osmdata_sf() 
  
  Greenspace2 <- rbind(Greenspace2 %>% .$osm_polygons %>% select(osm_id, name, geometry),
                       Greenspace2 %>% .$osm_multipolygons %>% select(osm_id, name, geometry))
  
  Greenspace <- rbind(Greenspace1, Greenspace2 ) %>% filter(st_is_valid(.)==TRUE)
  Greenspace <- st_intersection(Greenspace, border)
  
  return(Greenspace)
}

BerlinGreen <- GreenFun(c(13.099, 52.6674, 13.7892, 52.337), BerlinBorder)
LondonGreen <- GreenFun(c(-0.55, 51.2911, 0.25, 51.744), LondonBorder)
#MexCityGreen <- GreenFun("Mexico City, Mexico", MexicoCityBorder)
MilanGreen <- GreenFun("Milan, Italy", MilanBorder)
NycGreen <- GreenFun("New York City", NycBorder)
#SydneyGreen <- GreenFun("Sydney, Australia", SydneyBorder)
ParisGreen <- GreenFun("Paris, France", ParisBorder)



# Bikes
BikeFun <- function(bbox, border){
  Bikes <- opq(bbox=bbox) %>%
    add_osm_features(features=c("\"highway\"=\"cycleway\"",
                                "\"cycleway\"=\"lane\"",
                                "\"cycleway\"=\"track\"",
                                "\"route\"=\"bicycle\""))  %>%
    osmdata_sf() %>%
    .$osm_lines %>%
    select(osm_id, name, geometry) %>%
    filter(st_is_valid(.)==TRUE)
  Bikes <- st_intersection(Bikes, border)
  return(Bikes)
}

BerlinBike <- BikeFun(c(13.099, 52.6674, 13.7892, 52.337), BerlinBorder)
LondonBike <- BikeFun(c(-0.55, 51.2911, 0.25, 51.744), LondonBorder)
#MexCityBike <- BikeFun("Mexico City, Mexico", MexicoCityBorder)
MilanBike <- BikeFun("Milan, Italy", MilanBorder)
NycBike <- BikeFun("New York City", NycBorder)
#SydneyBike <- BikeFun("Sydney, Australia", SydneyBorder)
ParisBike <- BikeFun("Paris, France", ParisBorder)


# Water 
waterFun <- function(bbox, border){
  Water1 <- opq(bbox=bbox) %>%
    add_osm_features(features=c("\"water\"=\"pond\"",
                                "\"water\"=\"lake\"",
                                "\"water\"=\"reservoir\"",
                                "\"water\"=\"river\"",
                                "\"water\"=\"canal\"",
                                "\"water\"=\"lock\""))  %>%
    osmdata_sf() 
  Water1 <- rbind(Water1 %>% .$osm_polygons %>% select(osm_id, name, geometry),
                  Water1 %>% .$osm_multipolygons %>% select(osm_id, name, geometry))
  
  Water2 <- opq(bbox=bbox) %>%
    add_osm_features(features=c("\"waterway\"=\"river\"",
                                "\"waterway\"=\"stream\"",
                                "\"waterway\"=\"canal\"",
                                "\"landuse\"=\"reservoir\"",
                                "\"natural\"=\"water\"",
                                "\"natural\"=\"strait\"",
                                "\"natural\"=\"bay\""))  %>%
    osmdata_sf() 
  
  Water2 <- rbind(Water2 %>% .$osm_polygons %>% select(osm_id, name, geometry),
                  Water2 %>% .$osm_multipolygons %>% select(osm_id, name, geometry))
  
  Water <- rbind(Water2, Water2) %>% filter(st_is_valid(.)==TRUE)
  Water <- st_intersection(Water, border)
  
  return(Water)
}

BerlinWater <- waterFun(c(13.099, 52.6674, 13.7892, 52.337), BerlinBorder)
LondonWater <- waterFun(c(-0.55, 51.2911, 0.25, 51.744), LondonBorder)
#MexCityBike <- waterFun("Mexico City, Mexico", MexicoCityBorder)
MilanWater <- waterFun("Milan, Italy", MilanBorder)
NycWater <- waterFun("New York City", NycBorder)
#SydneyBike <- waterFun("Sydney, Australia", SydneyBorder)
ParisWater <- waterFun("Paris, France", ParisBorder)

# Plots

plotter <- function(Frame, color, size, border){
  ggplot() +
    geom_sf(data=border, fill="#fbefe4", size=0.4, color="#fbefe4", alpha=0.8) +
    geom_sf(data=Frame, fill=color, color=color, size=size) +
    theme_void() +
    theme(plot.margin = margin(0,2,-0.5,2, "cm"))
}


## Bikes
BikeGrid <- plot_grid(plotter(BerlinBike, Colors[1], 0.8, BerlinBorder),
                      plotter(LondonBike, Colors[1], 0.8, LondonBorder),
                      plotter(MilanBike, Colors[1], 0.8, MilanBorder),
                      plotter(NycBike, Colors[1], 0.8, NycBorder),
                      plotter(ParisBike, Colors[1], 0.8, ParisBorder),
                      nrow=1) +
  theme(plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

## Green Space Grid
GreenGrid <- plot_grid(plotter(BerlinGreen, Colors[2], 0.8, BerlinBorder),
                       plotter(LondonGreen, Colors[2], 0.8, LondonBorder),
                       plotter(MilanGreen, Colors[2], 0.8, MilanBorder),
                       plotter(NycGreen, Colors[2], 0.8, NycBorder),
                       plotter(ParisGreen, Colors[2], 0.8, ParisBorder),
                       nrow=1) +
  theme(plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))



## Pedestrian
PedGrid <- plot_grid(plotter(BerlinPed, Colors[3], 0.12, BerlinBorder),
                     plotter(LondonPed, Colors[3], 0.12, LondonBorder),
                     plotter(MilanPed, Colors[3], 0.12, MilanBorder),
                     plotter(NycPed, Colors[3], 0.12, NycBorder),
                     plotter(ParisPed, Colors[3], 0.12, ParisBorder),
                     nrow=1) +
  theme(plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))


## Subways
SubwayGrid <- plot_grid(plotter(BerlinSub, Colors[4], 0.8, BerlinBorder),
                        plotter(LondonSub, Colors[4], 0.8, LondonBorder),
                        plotter(MilanSub, Colors[4], 0.8, MilanBorder),
                        plotter(NycSub, Colors[4], 0.8, NycBorder),
                        plotter(ParisSub, Colors[4], 0.8, ParisBorder),
                        nrow=1) +
  theme(plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))




## Water
WaterGrid <- plot_grid(plotter(BerlinWater, Colors[5], 0.8, BerlinBorder),
                       plotter(LondonWater, Colors[5], 0.8, LondonBorder),
                       plotter(MilanWater, Colors[5], 0.8, MilanBorder),
                       plotter(NycWater, Colors[5], 0.8, NycBorder),
                       plotter(ParisWater, Colors[5], 0.8, ParisBorder),
                       nrow=1) +
  theme(plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))



MainGrid <- plot_grid(BikeGrid, GreenGrid, PedGrid, SubwayGrid, WaterGrid, ncol=1) +
  theme(plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
        panel.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
        plot.margin = margin(8, 0, 3, 6, unit="cm"))

ggdraw(MainGrid) +
  draw_label(x=0.5, y=0.96, label = "5 Maps of 5 Cities", color="#060633", size=475, fontfamily=font1, fontface="bold") +
  draw_line(x=c(0, 0.24), y=c(0.96, 0.96), color="#060633", size=5.5) +
  draw_line(x=c(0.76, 1), y=c(0.96, 0.96), color="#060633", size=5.5) +
  #draw_label(x=0.5, y=0.93, label = "Different ways of mapping global cities", color="#060633", size=175, fontfamily=font1) +
  draw_label(x=0.5, y=0.02, label =   "Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills", color="#060639", size=140, fontfamily=font1, fontface="bold") +
  #draw_line(x=c(0, 0.18), y=c(0.02, 0.02), color="#060639", size=3.3) +
  #draw_line(x=c(0.82, 1), y=c(0.02, 0.02), color="#060639", size=3.3) +
  draw_label(x=0.15, y=0.908, label = "BERLIN", color="#060639", size=200, fontfamily=font2, fontface="bold") +
  draw_label(x=0.34, y=0.908, label = "LONDON", color="#060639", size=200, fontfamily=font2, fontface="bold") +
  draw_label(x=0.54, y=0.908, label = "MILAN", color="#060639", size=200, fontfamily=font2, fontface="bold") +
  draw_label(x=0.73, y=0.908, label = "NEW YORK", color="#060639", size=200, fontfamily=font2, fontface="bold") +
  draw_label(x=0.9, y=0.908, label = "PARIS", color="#060639", size=200, fontfamily=font2, fontface="bold") +
  draw_label(x=0.042, y=0.83, label = "BIKE\nLANES", color=Colors[1], size=200, fontfamily=font2, fontface="bold", angle = 90, lineheight = 0.28) +
  draw_label(x=0.042, y=0.65, label = "GREEN\nSPACE", color=Colors[2], size=200, fontfamily=font2, fontface="bold", angle = 90, lineheight = 0.28) +
  draw_label(x=0.042, y=0.48, label = "PEDESTRIAN\nSTREETS", color=Colors[3], size=200, fontfamily=font2, fontface="bold", angle = 90, lineheight = 0.28) +
  draw_label(x=0.042, y=0.3, label = "SUBWAY\nLINES", color=Colors[4], size=200, fontfamily=font2, fontface="bold", angle = 90, lineheight = 0.28) +
  draw_label(x=0.042, y=0.11, label = "BODIES\nOF WATER", color=Colors[5], size=200, fontfamily=font2, fontface="bold", angle = 90, lineheight = 0.28) 


ggsave("~/Desktop/Day 9 (Monochrome).png", height = 40, width = 35)



