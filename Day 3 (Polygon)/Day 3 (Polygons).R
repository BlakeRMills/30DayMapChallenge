# Libraries
library(geojsonio)
library(tidyverse)
library(cowplot)
library(sf)
library(utils)
library(rgdal)
library(broom)
library(showtext)
library(sysfonts)

# Data 
nyc <- geojson_sf("~/Desktop/Parks/Parks Nyc.geojson")
paris <- geojson_sf("~/Desktop/Parks/Parks Paris.geojson")
amsterdam <- geojson_sf("~/Desktop/Parks/Parks Amsterdam.geojson")[1:120, ] #Last two parks are not closed polygons
london <- geojson_sf("~/Desktop/Parks/Parks London.geojson")
chicago <- geojson_sf("~/Desktop/Parks/Parks Chicago.geojson")
milan <- geojson_sf("~/Desktop/Parks/Parks Milan.geojson")
toronto <- readOGR(dsn = "~/Desktop/Parks/Parks Toronto") %>% tidy()
la <- geojson_sf("~/Desktop/Parks/Parks La.geojson")
perth <- geojson_sf("~/Desktop/Parks/Parks Perth.geojson")
singapore <- geojson_sf("~/Desktop/Parks/Parks Singapore.geojson")

# Aes
colors <- c("#1c4831", "#586b44", "#0e5047", "#572016", "#955020", "#934839", "#1d3b71", "#406c6d", "#082253", "#082253")
showtext_auto()
font_add_google("Quicksand")
font_add_google("Fira Sans")
font1 <- "Quicksand"
font2 <- "Fira Sans"
fontsize <- 130


# Main Plots
amsterdamP <- ggplot() + 
  geom_sf(data=amsterdam, aes(geometry=geometry), fill=colors[1], color=colors[1], size=1.25) +
  theme_void()

chicagoP <- ggplot() +
  geom_sf(data=chicago, aes(geometry=geometry), fill=colors[2], color=colors[2], size=1) +
  theme_void() 

laP <- ggplot() +
  geom_sf(data=la, aes(geometry=geometry), fill=colors[3], color=colors[3], size=1.25) +
  xlim(c(-118.7, -118.12)) +
  theme_void()

londonP <- ggplot() +
  geom_sf(data=london, aes(geometry=geometry), fill=colors[4], color=colors[4], size=1.25) +
  theme_void()+
  theme(plot.margin = margin(2, 0, 0, 0, unit="cm"))

milanP <- ggplot() +
  geom_sf(data=milan, aes(geometry=geometry), fill=colors[5], color=colors[5], size=1.25) +
  theme_void()+
  theme(plot.margin = margin(2, 0, 0, 0, unit="cm"))

nycP <- ggplot() +
  geom_sf(data=nyc, aes(geometry=geometry), fill=colors[6], color=colors[6], size=1) +
  theme_void() +
  theme(plot.margin = margin(2, 0, 0, 0, unit="cm"))

parisP <- ggplot() +
  geom_sf(data=paris, aes(geometry=geometry), fill=colors[7], color=colors[7], size=1) +
  ylim(c(48.8, 48.92)) +
  theme_void() +
  theme(plot.margin = margin(-1, 0, 0, 0, unit="cm"))

perthP <- ggplot() +
  geom_sf(data=perth, aes(geometry=geometry), fill=colors[10], color=colors[10], size=1) +
  theme_void()

singaporeP <- ggplot() +
  geom_sf(data=singapore, aes(geometry=geometry), fill=colors[8], color=colors[8], size=1.25) +
  theme_void() +
  theme(plot.margin = margin(-1, 0, 0, 0, unit="cm"))


#use this for toronto
torontoP <- ggplot() +
  geom_polygon(data=toronto, aes(x=long, y=lat, group=group), fill=colors[9], color=colors[9]) +
  coord_fixed() +
  theme_void() +
  theme(plot.margin = margin(-1, 0, 0, 0, unit="cm"))


# Main Grid
PlotGrid <- plot_grid(amsterdamP, chicagoP, laP,
          londonP, milanP, nycP,
          parisP, singaporeP, torontoP, nrow=3, ncol=3) +
  theme(plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
        plot.margin = margin(8,0,0,0, unit="cm"))

# Annotations
ggdraw(PlotGrid) +
  draw_label(label="Parks and Public Spaces in Major Cities", x=0.5, y=0.965, size=230, fontfamily = font2, fontface = "bold", color = "#102f2f") +
  draw_label(label="Parks, gardens, and other public spaces according to each city's open data portal", x=0.5, y=0.925, size=100, fontfamily = font1, color = "#102f2f") +
  draw_label(label="Twitter: @BlakeRobMills | GitHub: BlakeRMills", x=0.5, y=0.015, size=90, fontfamily = font1, fontface = "bold", color="#102f2f") +
  draw_line(x=c(0, 0.27), y=c(0.015, 0.015), size=1.5, color= "#102f2f") +
  draw_line(x=c(0.73, 1), y=c(0.015, 0.015), size=1.5, color= "#102f2f") +
  draw_line(x=c(0, 0.04), y=c(0.965, 0.965), size=4, color= "#102f2f") +
  draw_line(x=c(0.96, 1), y=c(0.965, 0.965), size=4, color= "#102f2f") +
  draw_label(label="AMSTERDAM", x=0.15, y=0.883, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[1]) +
  draw_label(label="CHICAGO", x=0.5, y=0.883, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[2]) +  
  draw_label(label="LOS ANGELES", x=0.85, y=0.883, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[3]) +
  draw_label(label="LONDON", x=0.15, y=0.568, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[4]) +
  draw_label(label="MILAN", x=0.5, y=0.568, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[5]) +
  draw_label(label="NEW YORK CITY", x=0.85, y=0.568, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[6]) +
  draw_label(label="PARIS", x=0.15, y=0.268, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[7]) +
  draw_label(label="SINGAPORE", x=0.5, y=0.268, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[8]) +
  draw_label(label="TORONTO", x=0.85, y=0.268, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[9]) 


ggsave("~/Desktop/ParkPlot.png", width=20, height=25)
