#NOTE:
#Due to the size of the map, it had to be put together in pieces

# Libraries
library(osmdata)
library(tidyverse)
library(sf)
library(cowplot)
library(showtext)
library(sysfonts)

# Aes
showtext_auto()
colors <- c("#7b4135", "#925b54", "#9f403d", "#9e4f3d", "#af5740", "#c07569", "#ae9769", "#af8f57", "#bc9033", "#6a7660",
            "#4b5c50", "#868d74", "#1e1e47", "#424569", "#2d3e62", "#947f8f", "#61555b", "#7c696e", "#936c78", "#a58a8c", 
            "#c58d9b")
font_add_google("Quicksand")
font_add_google("Fira Sans")
font1 <- "Quicksand"
font2 <- "Fira Sans"
fontsize <- 140

# Functions
CityStreets <- function(cityname, filtername, adminlevel){
  border <- opq(bbox=cityname) %>%
    add_osm_feature(key="admin_level", value=adminlevel) %>%
    osmdata_sf() %>%
    .$osm_multipolygons %>%
    select(osm_id, name, geometry) %>%
    filter(name==filtername)
  
  lines <- opq(bbox=cityname) %>%
    add_osm_feature(key="highway", value=c("motorway", "truck", "primary", "secondary", "tertiary", 
                                           "residential", "pedestrian", "service")) %>%
    osmdata_sf() %>%
    .$osm_lines %>%
    select(osm_id, name, geometry)
  
  intersection <- st_intersection(lines, border)
  
  list <- list(border, intersection)
  return(list)
}

plotter <- function(df, color){ 
  plot <- ggplot() +
    geom_sf(data=df[[2]], size=0.25, color=color) +
    theme_void() +
    theme(plot.margin = margin(1, 0, 1.5, 0, "cm"))
  return(plot)
}

# Data 
Paris <- CityStreets("Paris, France", "Paris", "8")
Milan <- CityStreets("Milan, Italy", "Milano", "8")
Amsterdam <- CityStreets("Amsterdam, Netherlands", "Amsterdam", "8")
Barcelona <- CityStreets("Barcelona, Spain", "Barcelona", "8")
BuenosAires <- CityStreets("Buenos Aires, Argentina", "Buenos Aires", "8")
Helsinki <- CityStreets("Helsinki, Finland", "Helsinki", "8")
Vienna <- CityStreets("Vienna, Austria", "Wien", "4") 
NewYork <- CityStreets("New York, New York", "New York", adminlevel = "5")
London <- CityStreets(c(-0.7278, 51.2911, 0.3419, 51.744), "London", adminlevel="6")
Oslo <- CityStreets("Oslo, Norway", "Oslo", adminlevel="4") 
Bogota <- CityStreets("Bogota, Colombia", "Bogotá", adminlevel = "7")
Rome <- CityStreets("Rome, Italy", "Roma", adminlevel = "8")
Caracas <- CityStreets("Caracas, Venezuela", "Caracas", adminlevel = "8")
MexicoCity <- CityStreets("Mexico City, Mexico", "Ciudad de México", adminlevel = "4")
WashingtonDC <- CityStreets("Washington, District of Columbia", "Washington", adminlevel="6")
Kolkata <- CityStreets("Kolkata, India", "Kolkata", adminlevel="6")
Montreal <- CityStreets("Montreal, Canada", "Montréal (06)", adminlevel="5")
Mumbai <- CityStreets("Mumbai, India", "Mumbai", adminlevel="8")
Havana <- CityStreets("Havana, Cuba", "La Habana", adminlevel="4")
Cairo <- CityStreets("Cairo, Egypt", "القاهرة", adminlevel = "4")
Tehran <- CityStreets("Tehran, Iran", "شهر تهران", adminlevel="7")

# Indvidual plots
AmsterdamP <- plotter(Amsterdam, colors[1])
BarcelonaP <- plotter(Barcelona, colors[2])
BogotaP <- plotter(Bogota, colors[3])
BuenosAiresP <- plotter(BuenosAires, colors[4])
CairoP <- plotter(Cairo, colors[5])
CaracasP <- plotter(Caracas, colors[6])
HavanaP <- plotter(Havana, colors[7])
HelsinkiP <- plotter(Helsinki, colors[8])
KolkataP <- plotter(Kolkata, colors[9])
LondonP <- plotter(London, colors[10])
MexicoCityP <- plotter(MexicoCity, colors[11])
MilanP <- plotter(Milan, colors[12])
MontrealP <- plotter(Montreal, colors[13])
MumbaiP <- plotter(Mumbai, colors[14])
NewYorkP <- plotter(NewYork, colors[15])
OsloP <- plotter(Oslo, colors[16])
ParisP <- plotter(Paris, colors[17])
RomeP <- plotter(Rome, colors[18])
TehranP <- plotter(Tehran, colors[19])
ViennaP <- plotter(Vienna, colors[20])
WashingtonDCP <- plotter(WashingtonDC, colors[21])

# Map 1
plot_grid1 <- plot_grid(AmsterdamP, BarcelonaP, BogotaP,
                        BuenosAiresP, CairoP, CaracasP, 
                        HavanaP, HelsinkiP, KolkataP, 
                        nrow=3, ncol=3) +
  theme(plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
        plot.margin = margin(9, 0, 0, 0, "cm"))

ggdraw(plot_grid1) +
  draw_label(label="The Shape of Cities", x=0.5, y=0.965, size=350, fontfamily = font2, fontface = "bold", color = "#4a2720") +
  draw_line(x=c(0, 0.22), y=c(0.965, 0.965), color="#4a2720", size=3.5) +
  draw_line(x=c(0.78, 1), y=c(0.965, 0.965), color="#4a2720", size=3.5) +
  draw_label(label="All roads within the limits of major cities", x=0.5, y=0.917, size=150, fontfamily = font1, color = "#4a2720") +
  draw_label(label="AMSTERDAM", x=0.18, y=0.873, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[1]) +
  draw_label(label="BARCELONA", x=0.5, y=0.873, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[2]) +
  draw_label(label="BOGOTA", x=0.82, y=0.873, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[3]) +
  draw_label(label="BUENOS AIRES", x=0.18, y=0.585, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[4]) +
  draw_label(label="CAIRO", x=0.5, y=0.585, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[5]) +
  draw_label(label="CARACAS", x=0.82, y=0.585, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[6]) +
  draw_label(label="HAVANA", x=0.18, y=0.288, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[7]) +
  draw_label(label="HELSINKI", x=0.5, y=0.288, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[8]) +
  draw_label(label="KOLKATA", x=0.82, y=0.288, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[9]) 


ggsave("~/Desktop/Day 5 (Open Street Map) 1 .png", height = 29.5, width = 25)


# Plot 2 
plot_grid2 <- plot_grid(MontrealP, MumbaiP, NewYorkP,
                        OsloP, ParisP, RomeP,
                        nrow=2, ncol=3) +
  theme(plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
        plot.margin = margin(0.35, 0, 0, 0, "cm"))

ggdraw(plot_grid2) +
  draw_label(label="MONTREAL", x=0.18, y=0.98, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[13]) +
  draw_label(label="MUMBAI", x=0.5, y=0.98, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[14]) +
  draw_label(label="NEW YORK", x=0.82, y=0.98, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[15]) +
  draw_label(label="OLSO", x=0.18, y=0.5, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[16]) +
  draw_label(label="PARIS", x=0.5, y=0.5, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[17]) +
  draw_label(label="ROME", x=0.82, y=0.5, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[18]) 

ggsave("~/Desktop/Day 5 (Open Street Map) 2 .png", height = 12, width = 25)


# Plot 3
plot_grid3 <- plot_grid(LondonP, MexicoCityP, MilanP,
                        nrow=1, ncol=3) +
  theme(plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
        plot.margin = margin(0.6, 0, 0, 0, "cm"))

ggdraw(plot_grid3) +
  draw_label(label="LONDON", x=0.18, y=0.95, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[10]) +
  draw_label(label="MEXICO CITY", x=0.5, y=0.95, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[11]) +
  draw_label(label="MILAN", x=0.82, y=0.95, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[12]) 

ggsave("~/Desktop/Day 5 (Open Street Map) 3 .png", height = 6, width = 25)


# Plot 4
plot_grid4 <- plot_grid(TehranP, ViennaP, WashingtonDCP,
                        nrow=1, ncol=3) +
  theme(plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
        plot.margin = margin(0.65, 0, 2, 0, "cm"))

ggdraw(plot_grid4) +
  draw_label(label="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills", x=0.5, y=0.06, size=75, fontfamily = font1, fontface = "bold", color="#593c43") +
  draw_label(label="TEHRAN", x=0.18, y=0.97, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[19]) +
  draw_label(label="VIENNA", x=0.5, y=0.97, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[20]) +
  draw_label(label="WASHINGTON DC", x=0.82, y=0.97, size=fontsize, fontfamily = font1, fontface = "bold", color = colors[21]) 

ggsave("~/Desktop/Day 5 (Open Street Map) 4 .png", height = 8, width = 25)


# Individual Plots

ggplot() +
  geom_sf(data=Amsterdam[[2]], color=colors[1], size=0.3) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Amsterdam") +
  theme(plot.title = element_text(color=colors[1], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[1], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/Amsterdam.png", height = 8, width = 10)


ggplot() +
  geom_sf(data=Barcelona[[2]], color=colors[2], size=0.3) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Barcelona") +
  theme(plot.title = element_text(color=colors[2], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[2], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/Barcelona.png", height = 8, width = 10)



ggplot() +
  geom_sf(data=Bogota[[2]], color=colors[3], size=0.3) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Bogota") +
  theme(plot.title = element_text(color=colors[3], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[3], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/Bogota.png", height = 8, width = 10)


ggplot() +
  geom_sf(data=BuenosAires[[2]], color=colors[4], size=0.3) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Buenos Aires") +
  theme(plot.title = element_text(color=colors[4], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[4], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/BuenosAires.png", height = 8, width = 10)


ggplot() +
  geom_sf(data=Cairo[[2]], color=colors[5], size=0.1) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Cairo") +
  theme(plot.title = element_text(color=colors[5], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[5], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/Cairo.png", height = 8, width = 10)



ggplot() +
  geom_sf(data=Caracas[[2]], color=colors[6], size=0.3) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Caracas") +
  theme(plot.title = element_text(color=colors[6], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[6], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/Caracas.png", height = 6, width = 10)
  
  
ggplot() +  
  geom_sf(data=Havana[[2]], color=colors[7], size=0.15) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Havana") +
  theme(plot.title = element_text(color=colors[7], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[7], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/Havana.png", height = 6.5, width = 10)

ggplot() + 
  geom_sf(data=Helsinki[[2]], color=colors[8], size=0.3) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Helsinki") +
  theme(plot.title = element_text(color=colors[8], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[8], size=30, hjust=0.5, family=font2, face="bold", vjust=4),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/Helsinki.png", height = 7.5, width = 10)


ggplot() + 
  geom_sf(data=Kolkata[[2]], color=colors[9], size=0.3) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Kolkata") +
  theme(plot.title = element_text(color=colors[9], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[9], size=30, hjust=0.5, family=font2, face="bold", vjust=4),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/Kolkata.png", height = 8, width = 10)

ggplot() + 
  geom_sf(data=London[[2]], color=colors[10], size=0.3) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("London") +
  theme(plot.title = element_text(color=colors[10], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[10], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/London.png", height = 8, width = 10)


ggplot() +
  geom_sf(data=MexicoCity[[2]], color=colors[11], size=0.15) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Mexico City") +
  theme(plot.title = element_text(color=colors[11], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[11], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/MexicoCity.png", height = 8, width = 10)


ggplot() + 
  geom_sf(data=Milan[[2]], color=colors[12], size=0.3) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Milan") +
  theme(plot.title = element_text(color=colors[12], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[12], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/Milan.png", height = 8, width = 10)


ggplot() + 
  geom_sf(data=Montreal[[2]], color=colors[13], size=0.2) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Montreal") +
  theme(plot.title = element_text(color=colors[13], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[13], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/Montreal.png", height = 8, width = 10)


ggplot() + 
  geom_sf(data=Mumbai[[2]], color=colors[14], size=0.3) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Mumbai") +
  theme(plot.title = element_text(color=colors[14], size=160, hjust=0.5, family=font2, face="bold",vjust=0.5),
        plot.caption = element_text(color=colors[14], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/Mumbai.png", height = 8, width = 10)

ggplot() + geom_sf(data=NewYork[[2]], color=colors[15], size=0.3) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("New York") +
  theme(plot.title = element_text(color=colors[15], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[15], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/DesktopNewYork.png", height = 8, width = 10)



ggplot() + 
  geom_sf(data=Oslo[[2]], color=colors[16], size=0.3) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Oslo") +
  theme(plot.title = element_text(color=colors[16], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[16], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/Oslo.png", height = 8, width = 10)


ggplot() + 
  geom_sf(data=Paris[[2]], color=colors[17], size=0.3) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Paris") +
  theme(plot.title = element_text(color=colors[17], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[17], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/Paris.png", height = 8, width = 10)




ggplot() + geom_sf(data=Rome[[2]], color=colors[18], size=0.3) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Rome") +
  theme(plot.title = element_text(color=colors[18], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[18], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/Rome.png", height = 8, width = 10)



ggplot() + 
  geom_sf(data=Tehran[[2]], color=colors[19], size=0.3) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Tehran") +
  theme(plot.title = element_text(color=colors[19], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[19], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/Tehran.png", height = 8, width = 10)


ggplot() + 
  geom_sf(data=Vienna[[2]], color=colors[20], size=0.3) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Vienna") +
  theme(plot.title = element_text(color=colors[20], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[20], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/Vienna.png", height = 8, width = 10)


ggplot() + 
  geom_sf(data=WashingtonDC[[2]], color=colors[21], size=0.3) +
  theme_void() +
  labs(caption="Twitter: @BlakeRobMills | Source: OpenStreetMap.org | GitHub: BlakeRMills") +
  ggtitle("Washington DC") +
  theme(plot.title = element_text(color=colors[21], size=160, hjust=0.5, family=font2, face="bold", vjust=0.5),
        plot.caption = element_text(color=colors[21], size=30, hjust=0.5, family=font2, face="bold", vjust=2),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/WashingtonDC.png", height = 8, width = 10)

