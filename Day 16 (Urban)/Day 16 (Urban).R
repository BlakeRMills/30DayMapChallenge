# Libraries
library(osmdata)
library(sf)
library(tidyverse)
library(readr)
library(geosphere)
library(showtext)
library(sysfonts)
library(stringr)
library(cowplot)
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

# Aes
colors1 <- c("#FFE135", "#FFAB28", "#FF751B", "#FF3E0D", "#cc0600", "#a20d46", "#5f0078", "#3f004f")
showtext_auto()
font_add_google("Fira Sans")
font1 <- "Fira Sans"

# Labels
levels <- c("(0,50]", "(50,75]", "(75,100]", "(100,125]", "(125,150]", "(150,175]", "(175,200]", "(200,Inf]")
labels <- c("< $50", "$50-$75", "$75-$100", "$100-$125", "$125-$150", "$150-$175", "$175-$200", "$200+")

# Conversion rates adjusted as of November 16th 2021
# Data
AmPrices <- read_csv("~/Desktop/Day 16 (Urban)/Data/Amsterdam.csv") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>% dplyr::select(price, geometry, bedrooms) %>%
  mutate(price = price %>% str_remove_all("\\$|,") %>% as.numeric(),
         price=price*1.13, 
         ppb = price/bedrooms) %>% filter(is.na(ppb)==FALSE)

BerlinPrices <- read_csv("~/Desktop/Day 16 (Urban)/Data/Berlin.csv") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>% dplyr::select(price, geometry, bedrooms) %>%
  mutate(price = price %>% str_remove_all("\\$|,") %>% as.numeric(),
         price=price*1.13, 
         ppb = price/bedrooms) %>% filter(is.na(ppb)==FALSE)

LondonPrices <- read_csv("~/Desktop/Day 16 (Urban)/Data/London.csv") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>% dplyr::select(price, geometry, bedrooms) %>%
  mutate(price = price %>% str_remove_all("\\$|,") %>% as.numeric(),
         price=price*1.34, 
         ppb = price/bedrooms) %>% filter(is.na(ppb)==FALSE)

MexicoPrices <- read_csv("~/Desktop/Day 16 (Urban)/Data/Cdmx.csv") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>% dplyr::select(price, geometry, bedrooms) %>%
  mutate(price = price %>% str_remove_all("\\$|,") %>% as.numeric(),
         price = price*0.048,
         ppb = price/bedrooms) %>% filter(is.na(ppb)==FALSE)

MilanPrices <- read_csv("~/Desktop/Day 16 (Urban)/Data/Milan.csv") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>% dplyr::select(price, geometry, bedrooms) %>%
  mutate(price = price %>% str_remove_all("\\$|,") %>% as.numeric(),
         price=price*1.13, 
         ppb = price/bedrooms) %>% filter(is.na(ppb)==FALSE)

NycPrices <- read_csv("~/Desktop/Day 16 (Urban)/Data/Nyc.csv") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>% dplyr::select(price, geometry, bedrooms) %>%
  mutate(price = price %>% str_remove_all("\\$|,") %>% as.numeric(),
         ppb = price/bedrooms) %>% filter(is.na(ppb)==FALSE)

ParisPrices <- read_csv("~/Desktop/Day 16 (Urban)/Data/Paris.csv") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>% dplyr::select(price, geometry, bedrooms) %>%
  mutate(price = price %>% str_remove_all("\\$|,") %>% as.numeric(),
         price=price*1.13, 
         ppb = price/bedrooms) %>% filter(is.na(ppb)==FALSE)

RioPrices <- read_csv("~/Desktop/Day 16 (Urban)/Data/Rio.csv") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>% dplyr::select(price, geometry, bedrooms) %>%
  mutate(price = price %>% str_remove_all("\\$|,") %>% as.numeric(),
         price=price*0.18, 
         ppb = price/bedrooms) %>% filter(is.na(ppb)==FALSE)

SydneyPrices <- read_csv("~/Desktop/Day 16 (Urban)/Data/Sydney.csv") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>% dplyr::select(price, geometry, bedrooms) %>%
  mutate(price = price %>% str_remove_all("\\$|,") %>% as.numeric(),
         price=price*0.73, 
         ppb = price/bedrooms) %>% filter(is.na(ppb)==FALSE)


# Borders
## Amsterdam
AmsterdamBorder <- opq(bbox="Amsterdam, Netherlands") %>%
  add_osm_feature(key="admin_level", value="8") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  select(osm_id, name, geometry) %>%
  filter(name=="Amsterdam")

## Berlin
BerlinBorder <- opq(bbox="Berlin, Germany") %>%
  add_osm_feature(key="admin_level", value="4") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  select(osm_id, name, geometry) %>%
  filter(name=="Berlin")

## London
LondonBorder <- opq(bbox=c(-0.55, 51.2911, 0.25, 51.744)) %>%
  add_osm_feature(key="admin_level", value="6") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  select(osm_id, name, geometry) %>%
  filter(name %in% c("London", "City of London")) %>%
  st_union()

## Mexico City
MexicoCityBorder <- opq(bbox="Mexico City, Mexico") %>%
  add_osm_feature(key="admin_level", value="4") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  select(osm_id, name, geometry) %>%
  filter(name=="Ciudad de México")

## Milan
MilanBorder <- opq(bbox="Milan, Italy") %>%
  add_osm_feature(key="admin_level", value="8") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  select(osm_id, name, geometry) %>%
  filter(name =="Milano") 

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

## Rio
RioBorder <- opq(bbox="Rio de Janeiro, Brazil") %>%
  add_osm_feature(key="admin_level", value="8") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  select(osm_id, name, geometry) %>%
  filter(name=="Rio de Janeiro")

## Sydney
SydneyBorder <- opq(bbox="Sydney, Australia") %>%
  add_osm_feature(key="admin_level", value="7") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  select(osm_id, name, geometry)


# Roads
Roads <- function(Border, BBox){
  Roads <- opq(bbox=BBox) %>%
    add_osm_feature(key="highway", value=c("motorway", "truck", "primary", "secondary", "tertiary", 
                                           "residential", "pedestrian", "service")) %>%
    osmdata_sf() %>%
    .$osm_lines %>%
    dplyr::select(osm_id, name, geometry)
  
  intersection <- st_intersection(Roads, Border)
  
  return(intersection)
}

AmsterdamRoads <- Roads(AmsterdamBorder, "Amsterdam, Netherlands")
BerlinRoads <- Roads(BerlinBorder, c(13.099, 52.6674, 13.7892, 52.337))
LondonRoads <- Roads(LondonBorder, c(-0.55, 51.2911, 0.25, 51.744))
#MexCityRoads <- Roads(MexicoCityBorder, "Mexico City, Mexico")
MilanRoads <- Roads(MilanBorder, "Milan, Italy")
NycRoads <- Roads(NycBorder, "New York")
ParisRoads <- Roads(ParisBorder, "Paris, France")
#RioRoads <- Roads(RioBorder, "Rio de Janeiro, Brazil")
#SydneyRoads <- Roads(SydneyBorder, "Sydney, Australia")

# Roads by Prices
ColorGrid <- function(Roads, Prices, n){
  grid <- st_make_grid(Roads, n=n) %>%  # Higher n will create more cells and make lines look more smooth, larger n increases run time
    st_as_sf(crs=4326) %>% mutate(geometry = x) %>% 
    dplyr::mutate(row = row_number())
  
  joins <- grid %>% st_join(., Prices, join=st_contains) %>% group_by(row) %>%
    dplyr::summarize(mean=mean(ppb))
  
  NoColor <- st_union(subset(joins, is.na(joins$mean)==TRUE))
  NoColor <- st_intersection(Roads, NoColor)
  
  Color <- subset(joins, is.na(joins$mean)==FALSE) %>% 
    mutate(MeanCat = cut(mean, c(0, 50, 75, 100, 125, 150, 175, 200, Inf)))
  
  ColorWhole <- c()
  for(j in unique(Color$MeanCat)){
    ColorInt <- st_union(subset(Color, Color$MeanCat==j))
    ColorInt <- st_intersection(Roads, ColorInt) %>% mutate(MeanCat=j)
    ColorWhole <-  rbind(ColorWhole, ColorInt)
  }
  
  list <- list(NoColor, ColorWhole)
  
  return(list)
}

AmsterdamColor <- ColorGrid(AmsterdamRoads, AmPrices, n=50)
BerlinColor <- ColorGrid(BerlinRoads, BerlinPrices, n=100)
LondonColor <- ColorGrid(LondonRoads, LondonPrices, n=125)
#MexCityColor <- ColorGrid(MexCityRoads, MexicoPrices, n=100)
MilanColor <- ColorGrid(MilanRoads, MilanPrices, n=50)
NycColor <- ColorGrid(NycRoads, NycPrices, n=150)
ParisColor <- ColorGrid(ParisRoads, ParisPrices, n=50)
#RioColor <- ColorGrid(RioRoads, RioPrices, n=150)
#SydneyColor <- ColorGrid(SydneyRoads, SydneyPrices, n=100)



# Plots
plotter <- function(list, linesize){
  plot <- ggplot() +
    geom_sf(data=list[[1]], alpha=0.2, size=linesize) +
    geom_sf(data=list[[2]], aes(color=factor(MeanCat, levels=levels, labels=labels), 
                                fill=factor(MeanCat, levels=levels, labels=labels)), size=linesize) +
    scale_color_manual(values = colors1) +
    scale_fill_manual(values = colors1) +
    theme_void() +
    theme(legend.position = "none",
          plot.margin=margin(2, 0, 1, 0, "cm"))
  
  return(plot)
}

## Plot with Legend
AmsterdamPlot <- ggplot() +
  geom_sf(data=AmsterdamColor[[1]], alpha=0.2, size=0.5) +
  geom_sf(data=AmsterdamColor[[2]], aes(color=factor(MeanCat, levels=levels, labels=labels), 
                              fill=factor(MeanCat, levels=levels, labels=labels)), size=0.5) +
  scale_color_manual(values = colors1,
                     guide=guide_legend(nrow = 1)) +
  scale_fill_manual(values = colors1,
                    guide=guide_legend(nrow = 1)) +
  theme_void() +
  theme(legend.position = c(1, 1.28),
        legend.direction = "horizontal",
        legend.text = element_blank(),
        legend.key.width = unit(7.5, "cm"),
        legend.key.height = unit(2.5, "cm"),
        legend.title = element_blank(),
        plot.margin=margin(2, 0, 1, 0, "cm"))

## Homogenous Plots
BerlinPlot <- plotter(BerlinColor, 0.3)
LondonPlot <- plotter(LondonColor, 0.25)
#MexicoPlot <- plotter(MexCityColor, 0.23)
MilanPlot <- plotter(MilanColor, 0.6)
NycPlot <- plotter(NycColor, 0.2)
ParisPlot <- plotter(ParisColor, 0.6)
#SydneyPlot <- plotter(SydneyColor, 0.2)

## Main Grid
grid <- plot_grid(AmsterdamPlot, BerlinPlot, 
                  LondonPlot, MilanPlot,
                  NycPlot, ParisPlot,
                  nrow=3, ncol=2) +
  theme(panel.background = element_rect(color="#fdf9f5", fill="#fdf9f5"),
        plot.background = element_rect(color="#fdf9f5", fill="#fdf9f5"),
        plot.margin = margin(20, 0.5, 3, 0.5, "cm"))

## Final Plot
Final <- ggdraw(grid) +
  draw_label(label="Twitter: @BlakeRobMills | Source: Inside Airbnb | GitHub: BlakeRMills", x=0.5, y=0.015, size=125, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="Staying on My Street", x=0.5, y=0.965, size=410, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="Average cost per bedroom, per night (in USD) of Airbnbs in major urban cities. Streets are\ncolored to represent the daily costs. Light grey streets represent no Airbnbs in the area.",
             x=0.5, y=0.92, size=155, fontfamily = font1, lineheight = 0.3, color="grey25")  +
  draw_line(x=c(0, 0.2), y=c(0.965, 0.965), size=6, color="#3f004f") +
  draw_line(x=c(0.8, 1), y=c(0.965, 0.965), size=6, color="#3f004f") +
  draw_label(label="AMSTERDAM", x=0.25, y=0.83, size=200, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="BERLIN", x=0.75, y=0.83, size=200, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="LONDON", x=0.25, y=0.565, size=200, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="MILAN", x=0.75, y=0.565, size=200, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="NEW YORK", x=0.25, y=0.285, size=200, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="PARIS", x=0.75, y=0.285, size=200, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label=">$50", x=0.14, y=0.874, size=125, fontface="bold", fontfamily = font1, color="grey15") +
  draw_label(label="$50-$75", x=0.241, y=0.874, size=125, fontface="bold", fontfamily = font1, color="grey15") +
  draw_label(label="$75-$100", x=0.344, y=0.874, size=125, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$100-$125", x=0.447, y=0.874, size=125, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$125-$150", x=0.551, y=0.874, size=125, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$150-$175", x=0.655, y=0.874, size=125, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$175-$200", x=0.758, y=0.874, size=125, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$200+", x=0.863, y=0.874, size=125, fontface="bold", fontfamily = font1, color="grey95") 
  
ggsave("~/Desktop/Final.png", height = 45, width = 30)

## Amsterdam Final
AmsterdamPlot2 <- ggplot() +
  geom_sf(data=AmsterdamColor[[1]], alpha=0.2, size=0.5) +
  geom_sf(data=AmsterdamColor[[2]], aes(color=factor(MeanCat, levels=levels, labels=labels), 
                                        fill=factor(MeanCat, levels=levels, labels=labels)), size=0.5) +
  scale_color_manual(values = colors1,
                     guide=guide_legend(nrow = 1)) +
  scale_fill_manual(values = colors1,
                    guide=guide_legend(nrow = 1)) +
  theme_void() +
  theme(legend.position = c(0.5, 1),
        legend.direction = "horizontal",
        legend.text = element_blank(),
        legend.key.width = unit(2.75, "cm"),
        legend.key.height = unit(0.75, "cm"),
        legend.title = element_blank(),
        plot.margin=margin(3, 0, 0, 0, "cm"))

AmsterdamFinal <- ggdraw(AmsterdamPlot2) +
  draw_label(label="Twitter: @BlakeRobMills | Source: Inside Airbnb | GitHub: BlakeRMills", x=0.5, y=0.05, size=45, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="Amsterdam", x=0.5, y=0.945, size=250, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="Average cost per bedroom, per night (in USD) of Airbnbs in Amsterdam. Streets are\ncolored to represent the daily costs. Light grey streets represent no Airbnbs in the area.",
             x=0.5, y=0.88, size=60, fontfamily = font1, lineheight = 0.325, color="grey25") +
  draw_label(label=">$50", x=0.214, y=0.833, size=45, fontface="bold", fontfamily = font1, color="grey15") +
  draw_label(label="$50-$75", x=0.295, y=0.833, size=45, fontface="bold", fontfamily = font1, color="grey15") +
  draw_label(label="$75-$100", x=0.377, y=0.833, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$100-$125", x=0.459, y=0.833, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$125-$150", x=0.541, y=0.833, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$150-$175", x=0.623, y=0.833, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$175-$200", x=0.706, y=0.833, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$200+", x=0.791, y=0.833, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  theme(panel.background = element_rect(color="#fdf9f5", fill="#fdf9f5"),
        plot.background = element_rect(color="#fdf9f5", fill="#fdf9f5"),
        plot.margin=margin(-0.5, 0, -1, 0, "cm")) 

ggsave("~/Desktop/Amsterdam.png", height = 14, width=15)


## Berlin Final
BerlinPlot2 <- ggplot() +
  geom_sf(data=BerlinColor[[1]], alpha=0.2, size=0.5) +
  geom_sf(data=BerlinColor[[2]], aes(color=factor(MeanCat, levels=levels, labels=labels), 
                                        fill=factor(MeanCat, levels=levels, labels=labels)), size=0.5) +
  scale_color_manual(values = colors1,
                     guide=guide_legend(nrow = 1)) +
  scale_fill_manual(values = colors1,
                    guide=guide_legend(nrow = 1)) +
  theme_void() +
  theme(legend.position = c(0.5, 1),
        legend.direction = "horizontal",
        legend.text = element_blank(),
        legend.key.width = unit(2.75, "cm"),
        legend.key.height = unit(0.75, "cm"),
        legend.title = element_blank(),
        plot.margin=margin(3, 0, 0, 0, "cm"))

BerlinFinal <- ggdraw(BerlinPlot2) +
  draw_label(label="Twitter: @BlakeRobMills | Source: Inside Airbnb | GitHub: BlakeRMills", x=0.5, y=0.05, size=45, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="Berlin", x=0.5, y=0.96, size=250, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="Average cost per bedroom, per night (in USD) of Airbnbs in Berlin. Streets are\ncolored to represent the daily costs. Light grey streets represent no Airbnbs in the area.",
             x=0.5, y=0.897, size=60, fontfamily = font1, lineheight = 0.325, color="grey25") +
  draw_label(label=">$50", x=0.214, y=0.853, size=45, fontface="bold", fontfamily = font1, color="grey15") +
  draw_label(label="$50-$75", x=0.295, y=0.853, size=45, fontface="bold", fontfamily = font1, color="grey15") +
  draw_label(label="$75-$100", x=0.377, y=0.853, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$100-$125", x=0.459, y=0.853, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$125-$150", x=0.541, y=0.853, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$150-$175", x=0.623, y=0.853, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$175-$200", x=0.706, y=0.853, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$200+", x=0.791, y=0.853, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  theme(panel.background = element_rect(color="#fdf9f5", fill="#fdf9f5"),
        plot.background = element_rect(color="#fdf9f5", fill="#fdf9f5"),
        plot.margin=margin(0.5, 0, -1, 0, "cm")) 

ggsave("~/Desktop/Berlin.png", height = 15, width=15)


## London Final
LondonPlot2 <- ggplot() +
  geom_sf(data=LondonColor[[1]], alpha=0.2, size=0.3) +
  geom_sf(data=LondonColor[[2]], aes(color=factor(MeanCat, levels=levels, labels=labels), 
                                     fill=factor(MeanCat, levels=levels, labels=labels)), size=0.3) +
  scale_color_manual(values = colors1,
                     guide=guide_legend(nrow = 1)) +
  scale_fill_manual(values = colors1,
                    guide=guide_legend(nrow = 1)) +
  theme_void() +
  theme(legend.position = c(0.5, 1),
        legend.direction = "horizontal",
        legend.text = element_blank(),
        legend.key.width = unit(2.75, "cm"),
        legend.key.height = unit(0.75, "cm"),
        legend.title = element_blank(),
        plot.margin=margin(3, 0, 0, 0, "cm"))

LondonFinal <- ggdraw(LondonPlot2) +
  draw_label(label="Twitter: @BlakeRobMills | Source: Inside Airbnb | GitHub: BlakeRMills", x=0.5, y=0.02, size=50, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="London", x=0.5, y=1.03, size=250, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="Average cost per bedroom, per night (in USD) of Airbnbs in London. Streets are\ncolored to represent the daily costs. Light grey streets represent no Airbnbs in the area.",
             x=0.5, y=0.962, size=60, fontfamily = font1, lineheight = 0.325, color="grey25") +
  draw_label(label=">$50", x=0.214, y=0.9125, size=45, fontface="bold", fontfamily = font1, color="grey15") +
  draw_label(label="$50-$75", x=0.295, y=0.9125, size=45, fontface="bold", fontfamily = font1, color="grey15") +
  draw_label(label="$75-$100", x=0.377, y=0.9125, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$100-$125", x=0.459, y=0.9125, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$125-$150", x=0.541, y=0.9125, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$150-$175", x=0.623, y=0.9125, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$175-$200", x=0.706, y=0.9125, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$200+", x=0.791, y=0.9125, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  theme(panel.background = element_rect(color="#fdf9f5", fill="#fdf9f5"),
        plot.background = element_rect(color="#fdf9f5", fill="#fdf9f5"),
        plot.margin=margin(3, 0, 0, 0, "cm")) 

ggsave("~/Desktop/London.png", height = 15, width=15)

## Milan Final
MilanPlot2 <- ggplot() +
  geom_sf(data=MilanColor[[1]], alpha=0.2, size=0.5) +
  geom_sf(data=MilanColor[[2]], aes(color=factor(MeanCat, levels=levels, labels=labels), 
                                     fill=factor(MeanCat, levels=levels, labels=labels)), size=0.5) +
  scale_color_manual(values = colors1,
                     guide=guide_legend(nrow = 1)) +
  scale_fill_manual(values = colors1,
                    guide=guide_legend(nrow = 1)) +
  theme_void() +
  theme(legend.position = c(0.5, 1),
        legend.direction = "horizontal",
        legend.text = element_blank(),
        legend.key.width = unit(2.75, "cm"),
        legend.key.height = unit(0.75, "cm"),
        legend.title = element_blank(),
        plot.margin=margin(3, 0, 0, 0, "cm"))

MilanFinal <- ggdraw(MilanPlot2) +
  draw_label(label="Twitter: @BlakeRobMills | Source: Inside Airbnb | GitHub: BlakeRMills", x=0.5, y=0.02, size=50, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="Milan", x=0.5, y=1.03, size=250, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="Average cost per bedroom, per night (in USD) of Airbnbs in Milan. Streets are\ncolored to represent the daily costs. Light grey streets represent no Airbnbs in the area.",
             x=0.5, y=0.962, size=60, fontfamily = font1, lineheight = 0.325, color="grey25") +
  draw_label(label=">$50", x=0.214, y=0.915, size=45, fontface="bold", fontfamily = font1, color="grey15") +
  draw_label(label="$50-$75", x=0.295, y=0.915, size=45, fontface="bold", fontfamily = font1, color="grey15") +
  draw_label(label="$75-$100", x=0.377, y=0.915, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$100-$125", x=0.459, y=0.915, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$125-$150", x=0.541, y=0.915, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$150-$175", x=0.623, y=0.915, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$175-$200", x=0.706, y=0.915, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$200+", x=0.791, y=0.915, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  theme(panel.background = element_rect(color="#fdf9f5", fill="#fdf9f5"),
        plot.background = element_rect(color="#fdf9f5", fill="#fdf9f5"),
        plot.margin=margin(3, 0, 0, 0, "cm")) 

ggsave("~/Desktop/Milan.png", height = 15, width=15)


## Nyc Final
NycPlot2 <- ggplot() +
  geom_sf(data=NycColor[[1]], alpha=0.2, size=0.25) +
  geom_sf(data=NycColor[[2]], aes(color=factor(MeanCat, levels=levels, labels=labels), 
                                     fill=factor(MeanCat, levels=levels, labels=labels)), size=0.25) +
  scale_color_manual(values = colors1,
                     guide=guide_legend(nrow = 1)) +
  scale_fill_manual(values = colors1,
                    guide=guide_legend(nrow = 1)) +
  theme_void() +
  theme(legend.position = c(0.5, 1),
        legend.direction = "horizontal",
        legend.text = element_blank(),
        legend.key.width = unit(2.75, "cm"),
        legend.key.height = unit(0.75, "cm"),
        legend.title = element_blank(),
        plot.margin=margin(3, 0, 0, 0, "cm"))

NycFinal <- ggdraw(NycPlot2) +
  draw_label(label="Twitter: @BlakeRobMills | Source: Inside Airbnb | GitHub: BlakeRMills", x=0.5, y=0.02, size=50, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="New York", x=0.5, y=1.03, size=250, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="Average cost per bedroom, per night (in USD) of Airbnbs in New York. Streets are\ncolored to represent the daily costs. Light grey streets represent no Airbnbs in the area.",
             x=0.5, y=0.962, size=60, fontfamily = font1, lineheight = 0.325, color="grey25") +
  draw_label(label=">$50", x=0.214, y=0.9145, size=45, fontface="bold", fontfamily = font1, color="grey15") +
  draw_label(label="$50-$75", x=0.295, y=0.9145, size=45, fontface="bold", fontfamily = font1, color="grey15") +
  draw_label(label="$75-$100", x=0.377, y=0.9145, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$100-$125", x=0.459, y=0.9145, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$125-$150", x=0.541, y=0.9145, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$150-$175", x=0.623, y=0.9145, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$175-$200", x=0.706, y=0.9145, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$200+", x=0.791, y=0.9145, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  theme(panel.background = element_rect(color="#fdf9f5", fill="#fdf9f5"),
        plot.background = element_rect(color="#fdf9f5", fill="#fdf9f5"),
        plot.margin=margin(3, 0, 0, 0, "cm")) 

ggsave("~/Desktop/Nyc.png", height = 15, width=15)

## Paris Final
ParisPlot2 <- ggplot() +
  geom_sf(data=ParisColor[[1]], alpha=0.2, size=0.5) +
  geom_sf(data=ParisColor[[2]], aes(color=factor(MeanCat, levels=levels, labels=labels), 
                                     fill=factor(MeanCat, levels=levels, labels=labels)), size=0.5) +
  scale_color_manual(values = colors1,
                     guide=guide_legend(nrow = 1)) +
  scale_fill_manual(values = colors1,
                    guide=guide_legend(nrow = 1)) +
  theme_void() +
  theme(legend.position = c(0.5, 1.05),
        legend.direction = "horizontal",
        legend.text = element_blank(),
        legend.key.width = unit(2.75, "cm"),
        legend.key.height = unit(0.75, "cm"),
        legend.title = element_blank(),
        plot.margin=margin(0, 0, 0, 0, "cm"))

ParisFinal <- ggdraw(ParisPlot2) +
  draw_label(label="Twitter: @BlakeRobMills | Source: Inside Airbnb | GitHub: BlakeRMills", x=0.5, y=0.16, size=45, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="Paris", x=0.5, y=0.92, size=250, fontface="bold", fontfamily = font1, color="#3f004f") +
  draw_label(label="Average cost per bedroom, per night (in USD) of Airbnbs in Paris. Streets are\ncolored to represent the daily costs. Light grey streets represent no Airbnbs in the area.",
             x=0.5, y=0.855, size=60, fontfamily = font1, lineheight = 0.325, color="grey25") +
  draw_label(label=">$50", x=0.214, y=0.809, size=45, fontface="bold", fontfamily = font1, color="grey15") +
  draw_label(label="$50-$75", x=0.295, y=0.809, size=45, fontface="bold", fontfamily = font1, color="grey15") +
  draw_label(label="$75-$100", x=0.377, y=0.809, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$100-$125", x=0.459, y=0.809, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$125-$150", x=0.541, y=0.809, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$150-$175", x=0.623, y=0.809, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$175-$200", x=0.706, y=0.809, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  draw_label(label="$200+", x=0.791, y=0.809, size=45, fontface="bold", fontfamily = font1, color="grey95") +
  theme(panel.background = element_rect(color="#fdf9f5", fill="#fdf9f5"),
        plot.background = element_rect(color="#fdf9f5", fill="#fdf9f5"),
        plot.margin=margin(-1,  0, -5, 0, "cm")) 

ggsave("~/Desktop/Paris.png", height = 12, width=15)

