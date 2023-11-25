# Libraries
library(cowplot)
library(geojsonio)
library(grid)
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
set.seed(50)
pal <- moma.colors("Connors", 20) %>% sample

# Data
Top <- geojson_sf("~/30DayMapChallenge/2023/Day 22 - North is not always Up/Data/Manhattan1852.geojson") %>%
  filter(position == "top") %>%
  mutate(Number = str_squish(Number))

Bottom <- geojson_sf("~/30DayMapChallenge/2023/Day 22 - North is not always Up/Data/Manhattan1852.geojson") %>%
  filter(position == "bottom") %>%
  mutate(Number = str_squish(Number))

Man <- geojson_sf("~/30DayMapChallenge/2023/Day 01 - Points/Data/Borough Boundaries.geojson")  %>%
  filter(boro_name == "Manhattan") %>%
  st_cast("POLYGON") %>%
  mutate(Area = st_area(.)) %>%
  arrange(-Area) %>%
  .[c(1), ]

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
  select(geometry) %>% 
  st_intersection(., Man)

# Cleaning 
Top_Roads <- st_intersection(Roads, Top)
Bottom_Roads <- st_intersection(Roads, Bottom)

label_back_top <- Top %>% 
  mutate(center = st_centroid(geometry),
         buff = st_buffer(center, 125))

label_back_bottom <- Bottom %>% 
  mutate(center = st_centroid(geometry),
         buff = st_buffer(center, 250))

# Map
## Map Generation
To <-ggplot() + 
  geom_sf(data=Top %>% filter(Category == "Ward"), aes(color=Number), fill="transparent", size=10) +
  geom_sf(data=Top_Roads %>% filter(Category == "Ward"), aes(color=Number), lwd=0.25, alpha=0.3) +
  geom_sf(data=label_back_top %>% filter(Category == "Ward"), aes(geometry=buff), fill="grey5",
          color="transparent") + 
  geom_sf_text(data=label_back_top %>% filter(Category == "Ward"), aes(color=Number, label=Number, geometry=center), 
               size=2.25, family=font, angle=60, fontface="bold") +
  scale_color_manual(values = pal[c(1:11,13:20)]) +
  theme_void() +
  theme(legend.position = "none")

Bo <- ggplot() + 
  geom_sf(data=Bottom %>% filter(Category == "Ward"), aes(color=Number), fill="transparent", size=10) +
  geom_sf(data=Bottom_Roads %>% filter(Category == "Ward"), aes(color=Number), lwd=0.25, alpha=0.3) +
  geom_sf(data=label_back_bottom %>% filter(Category == "Ward"), aes(geometry=buff), fill="grey5",
          color="transparent") + 
  geom_sf_text(data=label_back_bottom %>% filter(Category == "Ward"), aes(color=Number, label=Number, geometry=center), 
               size=2.25, family=font, angle=60, fontface="bold") +
  theme_void() +
  scale_color_manual(values = pal[c(12, 10)]) +
  theme(legend.position = "none")

base <- ggdraw() +
  draw_plot(Bo, x=-0.32, y=0.05, scale =1.05) +
  draw_plot(To, x=0.15, y=0.1, scale=1.2) + 
  theme(plot.background = element_rect(color="transparent", fill="grey5"))

## Save rotated base
for(i in -60){
  png("~/30DayMapChallenge/2023/Day 22 - North is not always Up/Day 22 - North is not always Up - Base.png", width = 14, height = 11, units="cm", res=500) # open the file
  grid.newpage()
  pushViewport(viewport(name = "rotate", angle = i, clip = "off", gp=gpar(fill="grey5")))
  grid.rect(gp=gpar(col="grey5"), width = 14, height = 11)
  print(base, vp="rotate") 
  dev.off() 
}

## Upload base and and annotate
### Custom fonts don't work with saving in base so this is done in two steps

g <- rectGrob(gp=gpar(fill=alpha("grey5", 1)), width = unit(8, "lines"), height=unit(1.25, "lines"))

ggdraw() +
  draw_image(image = "~/30DayMapChallenge/2023/Day 22 - North is not always Up/Day 22 - North is not always Up - Base.png") +
  theme(plot.background = element_rect(color="transparent", fill="grey5")) +
  draw_text(text="Map", x=0.15, y=0.22, family=font, fontface="bold", size=125, color=pal[1], hjust=0.5) +
  draw_text(text="of the", x=0.1425, y=0.175, family=font, size=60, color=pal[1], hjust = 0.5) +
  draw_text(text="City of New York", x=0.15, y=0.135, family=font, fontface="bold", size=125, color=pal[1], hjust=0.5) +
  draw_text(text="(1852)", x=0.1425, y=0.09, family=font, size=60, color=pal[1], hjust = 0.5) + 
  draw_grob(g, scale=0.2, x=-0.3, y=0.3) +
  draw_text(text = "Proposed Central Park", x=0.2, y=0.8, color=pal[1], family=font, size=40) + 
  draw_text(text="Map shows the wards of New York City in 1852.\nNote: The roads displayed are current roads and do not\naccurately represents the roads in 1852.",
            x=0.98, y=0.12, lineheight=0.4, color=pal[1], size=45, family=font, hjust=1) +
  draw_text(text="Twitter: @BlakeRobMills | Source: Brooklyn Library | GitHub: BlakeRMills", color=pal[1], x=0.5, y=0.015, size=40, fontface = "bold", family = font)  +
  draw_line(x=c(0.95, 0.97), y=c(0.95, 0.9625), color=pal[1], arrow=arrow(length = unit(0.75, "lines")), size=1.25) +
  draw_text(x=0.945, y=0.945, text="N", size=100, family=font, color=pal[1], hjust=1, fontface="bold")

ggsave("~/30DayMapChallenge/2023/Day 22 - North is not always Up/Day 22 - North is not always Up - Annotated.png", height = 11, width = 14)
