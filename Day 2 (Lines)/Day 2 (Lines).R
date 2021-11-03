# Libraries
library(readxl)
library(jsonlite)
library(rgeos)
library(showtext)
library(sysfonts)
library(stringr)
library(cowplot)
library(sf)
library(broom)

# Cities
Barcelona <- geojson_sf("~/Desktop/Day 2 Lines/Data/Bike Routes Barcelona.geojson")
Berlin <- geojson_sf("~/Desktop/Day 2 Lines/Data/Bike Routes Berlin.geojson")
Chicago <- geojson_sf("~/Desktop/Day 2 LinesData//Bike Routes Chicago.geojson")
London <- geojson_sf("~/Desktop/Day 2 Lines/Data/Bike Routes London.geojson")
Melbourne <- geojson_sf("~/Desktop/Day 2 Lines/Data/Bicycle Routes Melbourne.geojson")
Milan <- geojson_sf("~/Desktop/Day 2 Lines/Data/Bike Routes Milan.geojson")
Nyc <- geojson_sf("~/Desktop/Day 2 Lines/Data/Bicycle Routes NYC.geojson") %>% filter(boro !="5")
Sydney <- geojson_sf("~/Desktop/Day 2 Lines/Data/Bike Routes Sydney.geojson")
Toronto <- geojson_sf("~/Desktop/Day 2 Lines/Data/Bike Routes Toronto.geojson")

# Aes
topmargin <- 1.75
fontsize <- 160
showtext_auto()
font_add_google("Quicksand")
font_add_google("Fira Sans")
font1 <- "Quicksand"
font2 <- "Fira Sans"
color1 <- "#1c4831"
color2 <- "#586b44"
color3 <- "#0e5047"
color4 <- "#572016"
color5 <- "#955020"
color6 <- "#934839"
color7 <- "#1d3b71"
color8 <- "#406c6d"
color9 <- "#082253"
linesize <- 1.05

# Plots
BarcelonaPlot <- ggplot() +
  geom_sf(data=Barcelona, aes(geometry=geometry), size=linesize, color = color1) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit="cm"))

BerlinPlot <- ggplot() +
  geom_sf(data=Berlin, aes(geometry=geometry), size=linesize, color = color2) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit="cm"))

ChicagoPlot <- ggplot() +
  geom_sf(data=Chicago, aes(geometry=geometry), size=linesize, color = color3) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit="cm"))

LondonPlot <- ggplot() +
  geom_sf(data=London, aes(geometry=geometry), size=linesize, color = color4) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit="cm"))

MelbournePlot <- ggplot() +
  geom_sf(data=Melbourne, aes(geometry=geometry), size=linesize,color = color5) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit="cm"))

MilanPlot <- ggplot() +
  geom_sf(data=Milan, aes(geometry=geometry), size=linesize, color = color6) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit="cm"))

NycPlot <- ggplot() +
  geom_sf(data=Nyc, aes(geometry=geometry), size=linesize, color = color7) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit="cm"))

SydneyPlot <- ggplot() +
  geom_sf(data=Sydney, aes(geometry=geometry), size=linesize, color = color8) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit="cm"))

TorontoPlot <- ggplot() +
  geom_sf(data=Toronto, aes(geometry=geometry), size=linesize, color = color9) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit="cm"))

# Makes Main grid
GridPlot <- grid <- plot_grid(BarcelonaPlot, BerlinPlot, ChicagoPlot, 
                              LondonPlot, MelbournePlot, MilanPlot, 
                              NycPlot, SydneyPlot, TorontoPlot,
                              nrow = 3, ncol = 3) +
  theme(plot.margin = margin(8, 0, 2, 0, unit="cm"),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

# Annotations
ggdraw(GridPlot) +
  draw_label(label="Bike Lanes of Major Cities", x=0.5, y=0.965, size=300, fontfamily = font2, fontface = "bold", color = "#102f2f") +
  draw_label(label="All available bike lines according to each city's open data portal", x=0.5, y=0.925, size=135, fontfamily = font1, color = "#102f2f") +
  draw_line(x=c(0, 0.15), y=c(0.965, 0.965), size=4, color= "#102f2f") +
  draw_line(x=c(0.85, 1), y=c(0.965, 0.965), size=4, color= "#102f2f") +
  draw_label(label="Twitter: @BlakeRobMills | GitHub: BlakeRMills", x=0.5, y=0.015, size=100, fontfamily = font1, fontface = "bold", color="#102f2f") +
  draw_line(x=c(0, 0.28), y=c(0.015, 0.015), size=1.5, color= "#102f2f") +
  draw_line(x=c(0.72, 1), y=c(0.015, 0.015), size=1.5, color= "#102f2f") +
  draw_label(label="SYDNEY", x=0.5, y=0.3, size=fontsize, fontfamily = font1, fontface = "bold", color = color8) +
  draw_label(label="NEW YORK CITY", x=0.2, y=0.3, size=fontsize, fontfamily = font1, fontface = "bold", color = color7) +
  draw_label(label="TORONTO", x=0.8, y=0.3, size=fontsize, fontfamily = font1, fontface = "bold", color = color9) +
  draw_label(label="MILAN", x=0.8, y=0.585, size=fontsize, fontfamily = font1, fontface = "bold", color = color6) +
  draw_label(label="LONDON", x=0.2, y=0.585, size=fontsize, fontfamily = font1, fontface = "bold", color = color4) +
  draw_label(label="MELBOURNE", x=0.5, y=0.585, size=fontsize, fontfamily = font1, fontface = "bold", color = color5) +
  draw_label(label="BERLIN", x=0.5, y=0.875, size=fontsize, fontfamily = font1, fontface = "bold", color = color2) +
  draw_label(label="BARCELONA", x=0.2, y=0.875, size=fontsize, fontfamily = font1, fontface = "bold", color = color1) +
  draw_label(label="CHICAGO", x=0.8, y=0.875, size=fontsize, fontfamily = font1, fontface = "bold", color = color3)


ggsave("~/Desktop/Day 2 Lines.png", width = 23, height = 28)
