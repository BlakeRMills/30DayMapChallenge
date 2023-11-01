# Libraries
library(osmdata)
library(sf)
library(tidyverse)
library(elevatr)
library(raster)
library(sysfonts)
library(showtext)
library(cowplot)

# Aes
showtext_auto()
sf_use_s2(FALSE)
colors1 <- c("#ffd902", "#FFAB28", "#FF751B", "#FF3E0D", "#cc0600", "#a20d46", "#5f0078", "#3f004f")
font_add_google("Fira Sans")
font1 <- "Fira Sans"

# Roads and Borders

rbFun <- function(cityname, filtername, adminlevel){
   border <- opq(bbox=cityname) %>%
     add_osm_feature(key="admin_level", value=adminlevel) %>%
     osmdata_sf() %>%
    .$osm_multipolygons %>%
     dplyr::select(osm_id, name, geometry) %>%
     dplyr::filter(name==filtername)
  
  streets <- opq(bbox=cityname) %>%
    add_osm_feature(key="highway", value=c("primary", "secondary", "tertiary", "residential")) %>%
    osmdata_sf() %>%
    .$osm_lines %>%
    dplyr::select(osm_id, name, geometry)
  
  streetsInt <- st_intersection(streets, border)
  
  final <- list(border, streetsInt)
}

# Selected Cities
Milan <- rbFun("Milan, Italy", "Milano", "8")
Nyc <- rbFun("New York City", "New York", "5")
Cdmx <- rbFun("Mexico City, Mexico", "Ciudad de México", "4")
Paris <- rbFun("Paris, France", "Paris", "8")
BuenosAires <- rbFun("Buenos Aires, Argentina", "Buenos Aires", "8")
Berlin <- rbFun(c(13.099, 52.6674, 13.7892, 52.337), "Berlin", "4")
Mumbai <- rbFun("Mumbai, India", "Mumbai", "8")

# Custom London
LondonBorder <- opq(bbox=c(-0.55, 51.2911, 0.25, 51.744)) %>%
  add_osm_feature(key="admin_level", value="6") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  dplyr::select(osm_id, name, geometry) %>%
  dplyr::filter(name %in% c("London", "City of London")) %>%
  st_union()

LondonStreets <- opq(bbox=c(-0.55, 51.2911, 0.25, 51.744)) %>%
  add_osm_feature(key="highway", value=c("primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf() %>%
  .$osm_lines %>%
  dplyr::select(osm_id, name, geometry)

LondonStreentsint <- st_intersection(LondonStreets, LondonBorder)
London <- list(LondonBorder, LondonStreentsint)
rm(LondonBorder, LondonStreentsint, LondonStreets)

# City Elevation

ElFun <- function(border, cutVec){
  Elev <- get_elev_raster(locations = border, z = 7, clip = "bbox")
  Poly <- rasterToPolygons(Elev) %>% st_as_sf()
  CutPoly <- st_intersection(Poly, st_make_valid(border)) %>% dplyr::filter(st_is_valid(.)==TRUE)
  colnames(CutPoly) <- if(ncol(CutPoly)==4){c("Elevation", "Id", "Name", "geometry")}else{c("Elevation", "geometry")}
  Final <- CutPoly %>% mutate(cut = cut(Elevation, cutVec)) %>% st_as_sf()
  Return <- list(Final, CutPoly)
}

# City Elevations
MilanEle <- ElFun(Milan[[1]], c(-100, 110, 115, 120, 125, 130, 135, 140, 2000))
NycEle <- ElFun(Nyc[[1]], c(-100, 0, 5, 10, 15, 20, 25, 30, 2000))
CdmxEle <- ElFun(Cdmx[[1]], c(-100, 2250, 2450, 2650, 2850, 3050, 3250, 3450, 5000))
ParisEle <- ElFun(Paris[[1]], c(-100, 30, 35, 40, 45, 50, 55, 60, 1000))
LondonEle <- ElFun(London[[1]], c(-100, 10, 20, 30, 40, 50, 60, 70, 5000))
MumbaiEle <- ElFun(Mumbai[[1]], c(-400, 0, 5, 10, 15, 20, 25, 30, 5000))
BuenosAiresEle <- ElFun(BuenosAires[[1]], c(-100, 0, 5, 10, 15, 20, 25, 30, 5000))
BerlinEle <- ElFun(Berlin[[1]], c(-100, 30, 35, 40, 45, 50, 55, 60, 5000))

## Use these to test for good breaks 
#mosaic::favstats(~BerlinEle[[2]]$Elevation)
#table(BerlinEle[[1]]$cut)

# Road Elevations
RoadEle <- function(ElevationFrame, Roads, Cuts){
  CityRoadEle <- NULL
  for(i in unique(Cuts)){
    FilteredDf <- ElevationFrame %>% dplyr::filter(cut==i)
    Unioned <- st_union(FilteredDf) %>% as.data.frame() %>% st_as_sf()
    RoadEle <- st_intersection(Roads, Unioned) %>% mutate(level=i)
    CityRoadEle <- rbind(CityRoadEle, RoadEle)
  }
  return(CityRoadEle)
}

# City Road Elevations (Final Step)
MilanRoads <- RoadEle(MilanEle[[1]], Milan[[2]], MilanEle[[1]]$cut)
NycRoads <- RoadEle(NycEle[[1]], Nyc[[2]], NycEle[[1]]$cut)
CdmxRoads <- RoadEle(CdmxEle[[1]], Cdmx[[2]], CdmxEle[[1]]$cut)
ParisRoads <- RoadEle(ParisEle[[1]], Paris[[2]], ParisEle[[1]]$cut)
LondonRoads <- RoadEle(LondonEle[[1]], London[[2]], LondonEle[[1]]$cut)
MumbaiRoads <- RoadEle(MumbaiEle[[1]], Mumbai[[2]], MumbaiEle[[1]]$cut)
BuenosAiresRoads <- RoadEle(BuenosAiresEle[[1]], BuenosAires[[2]], BuenosAiresEle[[1]]$cut)
BerlinRoads <- RoadEle(BerlinEle[[1]], Berlin[[2]], BerlinEle[[1]]$cut)

# Plotter
plotter <- function(df, linesize, cityname, labels, levels){
  plot <- ggplot() +
    geom_sf(data=df, aes(color=level, fill=level), size=linesize) +
    scale_color_manual(values = colors1, guide=guide_legend(nrow = 1),
                       breaks=levels) +
    scale_fill_manual(values = colors1, guide=guide_legend(nrow = 1),
                      breaks = levels) +
    theme_void() +
    coord_sf() +
    theme(legend.position = c(0.5, 1.025),
          legend.direction = "horizontal",
          legend.key.width = unit(3.5, "cm"),
          legend.key.height = unit(1, "cm"),
          legend.text = element_blank(),
          legend.title = element_blank())
  
  ggdraw(plot) +
    theme(plot.margin = margin(6.5, 0.5, 1, 0.5, "cm"),
          plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
          panel.background = element_rect(fill="#fdf9f5", color="#fdf9f5")) +
    draw_label(label=cityname, x=0.5, y=1.14, size=250, fontfamily = font1, fontface = "bold", color="#3f004f") +
    draw_label(label=paste("Map shows the elevation (in meters) of", cityname, "by street."), x=0.5, y=1.068, size=70, color="#3f004f", fontfamily = font1) +
    draw_label(label="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills", x=0.5, y=-0.01, size=50, fontface="bold", color="#3f004f", fontfamily = font1) +
    draw_label(label=labels[1], x=0.133, y=1.025, size=55, fontface="bold", color="grey15", fontfamily = font1) +
    draw_label(label=labels[2], x=0.238, y=1.025, size=55, fontface="bold", color="grey15", fontfamily = font1) +
    draw_label(label=labels[3], x=0.343, y=1.025, size=55, fontface="bold", color="grey95", fontfamily = font1) +
    draw_label(label=labels[4], x=0.448, y=1.025, size=55, fontface="bold", color="grey95", fontfamily = font1) +
    draw_label(label=labels[5], x=0.553, y=1.025, size=55, fontface="bold", color="grey95", fontfamily = font1) +
    draw_label(label=labels[6], x=0.658, y=1.025, size=55, fontface="bold", color="grey95", fontfamily = font1) +
    draw_label(label=labels[7], x=0.763, y=1.025, size=55, fontface="bold", color="grey95", fontfamily = font1) +
    draw_label(label=labels[8], x=0.868, y=1.025, size=55, fontface="bold", color="grey95", fontfamily = font1) 
}

MilanPlot <- plotter(MilanRoads, 1, "Milan", c("< 110", "110-115", "115-120", "120-125", "125-130", "130-135", "135-140", "> 140"),
                     c("(-100,110]", "(110,115]", "(115,120]", "(120,125]", "(125,130]", "(130,135]", "(135,140]", "(140,2e+03]"))
ggsave("~/Desktop/Milan.png", height = 15, width = 15)


NycPlot <- plotter(NycRoads, 0.35, "New York", c("< 0", "0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "> 30"),
                   c("(-100,0]", "(0,5]", "(5,10]", "(10,15]", "(15,20]", "(20,25]", "(25,30]", "(30,2e+03]"))
ggsave("~/Desktop/NewYork.png", height = 15, width = 15)

CdmxPlot <- plotter(CdmxRoads, 0.25, "Mexico City", c("< 2250", "2250-2450", "2450-2650", "2650-2850", "2850-3050", "3050-3250", "3250-3450", "> 3450"),
                    c("(-100,2.25e+03]", "(2.25e+03,2.45e+03]", "(2.45e+03,2.65e+03]", "(2.65e+03,2.85e+03]", "(2.85e+03,3.05e+03]", "(3.05e+03,3.25e+03]", "(3.25e+03,3.45e+03]", "(3.45e+03,5e+03]"))
ggsave("~/Desktop/Cdmx.png", height = 15, width = 15)

LondonPlot <- plotter(LondonRoads, 0.4, "London", c("< 10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "> 70"),
                      c("(-100,10]", "(10,20]", "(20,30]", "(30,40]", "(40,50]", "(50,60]", "(60,70]", "(70,5e+03]"))
ggsave("~/Desktop/London.png", height = 15, width = 15)

BuenosAiresPlot <- plotter(BuenosAiresRoads, 0.9, "Buenos Aires", c("< 0", "0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "> 30"),
                           c("(-100,0]", "(0,5]", "(5,10]", "(10,15]", "(15,20]", "(20,25]", "(25,30]", "(30,5e+03]"))
ggsave("~/Desktop/BuenosAires.png", height = 15, width = 15)

MumbaiPlot <- plotter(MumbaiRoads, 0.5, "Mumbai", c("< 0", "0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "> 30"),
                      c("(-400,0]", "(0,5]", "(5,10]", "(10,15]", "(15,20]", "(20,25]", "(25,30]", "(30,5e+03]"))
ggsave("~/Desktop/Mumbai.png", height = 15, width = 15)



# Paris amd Berlin were not working in function. This generates their plots
## Paris
ParisLabs <- c("< 30", "30-35", "35-40", "40-45", "45-50", "50-55", "55-60", "> 60")
ParisPlot <- ggplot() +
  geom_sf(data=ParisRoads, aes(color=level, fill=level), size=1) +
  scale_color_manual(values = colors1, guide=guide_legend(nrow = 1),
                     breaks=c("(-100,30]", "(30,35]", "(35,40]", "(40,45]", "(45,50]", "(50,55]", "(55,60]", "(60,1e+03]")) +
  scale_fill_manual(values = colors1, guide=guide_legend(nrow = 1),
                    breaks=c("(-100,30]", "(30,35]", "(35,40]", "(40,45]", "(45,50]", "(50,55]", "(55,60]", "(60,1e+03]")) +
  theme_void() +
  coord_sf() +
  theme(legend.position = c(0.5, 1.05),
        legend.direction = "horizontal",
        legend.key.width = unit(3.5, "cm"),
        legend.key.height = unit(1, "cm"),
        legend.text = element_blank(),
        legend.title = element_blank())

ParisPlot2 <- ggdraw(ParisPlot) +
  theme(plot.margin = margin(2, 0.5, -2, 0.5, "cm"),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
        panel.background = element_rect(fill="#fdf9f5", color="#fdf9f5")) +
  draw_label(label="Paris", x=0.5, y=0.99, size=250, fontfamily = font1, fontface = "bold", color="#3f004f") +
  draw_label(label=paste("Map shows the elevation (in meters) of Paris by street."), x=0.5, y=0.91, size=70, color="#3f004f", fontfamily = font1) +
  draw_label(label="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills", x=0.5, y=0.1, size=50, fontface="bold", color="#3f004f", fontfamily = font1) +
  draw_label(label=ParisLabs[1], x=0.133, y=0.861, size=55, fontface="bold", color="grey15", fontfamily = font1) +
  draw_label(label=ParisLabs[2], x=0.238, y=0.861, size=55, fontface="bold", color="grey15", fontfamily = font1) +
  draw_label(label=ParisLabs[3], x=0.343, y=0.861, size=55, fontface="bold", color="grey95", fontfamily = font1) +
  draw_label(label=ParisLabs[4], x=0.448, y=0.861, size=55, fontface="bold", color="grey95", fontfamily = font1) +
  draw_label(label=ParisLabs[5], x=0.553, y=0.861, size=55, fontface="bold", color="grey95", fontfamily = font1) +
  draw_label(label=ParisLabs[6], x=0.658, y=0.861, size=55, fontface="bold", color="grey95", fontfamily = font1) +
  draw_label(label=ParisLabs[7], x=0.763, y=0.861, size=55, fontface="bold", color="grey95", fontfamily = font1) +
  draw_label(label=ParisLabs[8], x=0.868, y=0.861, size=55, fontface="bold", color="grey95", fontfamily = font1) 
ggsave("~/Desktop/Paris.png", height = 12, width = 15)

## Berlin
BerlinLabs <- c("< 30", "30-35", "35-40", "40-45", "45-50", "50-55", "55-60", "> 60")
BerlinPlot <- ggplot() +
  geom_sf(data=BerlinRoads, aes(color=level, fill=level), size=0.6) +
  scale_color_manual(values = colors1, guide=guide_legend(nrow = 1),
                     breaks=c("(-100,30]", "(30,35]", "(35,40]", "(40,45]", "(45,50]", "(50,55]", "(55,60]", "(60,5e+03]")) +
  scale_fill_manual(values = colors1, guide=guide_legend(nrow = 1),
                    breaks=c("(-100,30]", "(30,35]", "(35,40]", "(40,45]", "(45,50]", "(50,55]", "(55,60]", "(60,5e+03]")) +
  theme_void() +
  coord_sf() +
  theme(legend.position = c(0.5, 1.025),
        legend.direction = "horizontal",
        legend.key.width = unit(3.5, "cm"),
        legend.key.height = unit(1, "cm"),
        legend.text = element_blank(),
        legend.title = element_blank())

BerlinPlot2 <- ggdraw(BerlinPlot) +
  theme(plot.margin = margin(6.5, 0.5, 1, 0.5, "cm"),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
        panel.background = element_rect(fill="#fdf9f5", color="#fdf9f5")) +
  draw_label(label="Berlin", x=0.5, y=1.14, size=250, fontfamily = font1, fontface = "bold", color="#3f004f") +
  draw_label(label=paste("Map shows the elevation (in meters) of Berlin by street."), x=0.5, y=1.068, size=70, color="#3f004f", fontfamily = font1) +
  draw_label(label="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills", x=0.5, y=-0.01, size=50, fontface="bold", color="#3f004f", fontfamily = font1) +
  draw_label(label=BerlinLabs[1], x=0.133, y=1.004, size=55, fontface="bold", color="grey15", fontfamily = font1) +
  draw_label(label=BerlinLabs[2], x=0.238, y=1.004, size=55, fontface="bold", color="grey15", fontfamily = font1) +
  draw_label(label=BerlinLabs[3], x=0.343, y=1.004, size=55, fontface="bold", color="grey95", fontfamily = font1) +
  draw_label(label=BerlinLabs[4], x=0.448, y=1.004, size=55, fontface="bold", color="grey95", fontfamily = font1) +
  draw_label(label=BerlinLabs[5], x=0.553, y=1.004, size=55, fontface="bold", color="grey95", fontfamily = font1) +
  draw_label(label=BerlinLabs[6], x=0.658, y=1.004, size=55, fontface="bold", color="grey95", fontfamily = font1) +
  draw_label(label=BerlinLabs[7], x=0.763, y=1.004, size=55, fontface="bold", color="grey95", fontfamily = font1) +
  draw_label(label=BerlinLabs[8], x=0.868, y=1.004, size=55, fontface="bold", color="grey95", fontfamily = font1) 
ggsave("~/Desktop/Berlin.png", height = 15, width = 15)

