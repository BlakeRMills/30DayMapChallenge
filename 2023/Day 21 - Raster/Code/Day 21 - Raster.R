# Libraries
library(cowplot)
library(elevatr)
library(geojsonio)
library(MoMAColors)
library(osmdata)
library(raster)
library(sf)
library(showtext)
library(sysfonts)
library(tidyverse)

# Aes
showtext_auto()
font <- "Advent Pro"
font_add_google(font)

# Functions 
GetElev <- function(area, size){
  
  Elev <- get_elev_raster(locations=area, z=size, clip="bbox", neg_to_na=T)
  Poly <- rasterToPolygons(Elev) %>% st_as_sf()
  colnames(Poly) <- c("Elevation", "geometry")
  Int <- st_intersection(Poly, area)
  Int <- Int %>%
    mutate(Elevation_ft = Elevation*3.28084) %>%
    filter(Elevation_ft > 0 )
  return(Int)
}

# Data
Neigh <- geojson_sf("~/30DayMapChallenge/2023/Day 19 - 5 Minute Map/Data/NYC_Neighborhoods_BetaNYC.geojson") %>%
  filter(grepl("Height", neighborhood)) 
NYC <- geojson_sf("~/30DayMapChallenge/2023/Day 01 - Points/Data/Borough Boundaries.geojson") 
Man <- NYC %>%
  filter(boro_name == "Manhattan") %>%
  st_cast("POLYGON") %>%
  mutate(Area = st_area(.)) %>%
  arrange(-Area) %>%
  .[c(1,5), ]

# Cleaning
MN_Elev <- GetElev(Man, 11)
BK_Elev <- GetElev(NYC %>% filter(boro_name == "Brooklyn"), 10)
BX_Elev <- GetElev(NYC %>% filter(boro_name == "Bronx"), 11)
QN_Elev <- GetElev(NYC %>% filter(boro_name == "Queens"), 10)
SI_Elev <- GetElev(NYC %>% filter(boro_name == "Staten Island") , 11)


# Manhattan
map <- ggplot() +
  geom_sf(data=MN_Elev, aes(fill=Elevation_ft), color="transparent") +
  geom_sf(data=Neigh %>% filter(borough == "Manhattan"), color=alpha("white", 0.75), fill="transparent", size =2) +
  scale_fill_moma_c("Panton", direction = -1) +
  theme_void() + 
  theme(legend.position = c(1.1, 0.3),
        legend.key.width = unit(1.75, "cm"),
        legend.key.height = unit(2.25, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(color="white", hjust = 0.5, size=160, family=font, margin=margin(0, 0, 0, -5.32, "cm"), face="bold"))

man_d <- ggdraw(map) +
  theme(plot.background = element_rect(fill="grey10")) + 
  draw_text(text="Twitter: @BlakeRobMills | GitHub: BlakeRMills", color="white", x=0.5, y=0.015, size=175, fontface = "bold", family = font) + 
  draw_text(text="'Heights' of\nManhattan", x=0.05, y=0.9, hjust=0, size=875, color="white", family=font, fontface="bold", lineheight = 0.09) +
  draw_text(text="Map displays the elevation of Manhattan. Neighborhoods\nthat include the word 'Heights' are highlighted.",
            x=0.05, y=0.78, hjust=0, size=175, color="white", family=font, lineheight = 0.12) +
  draw_text(text="Washington\nHeights", x=0.68, y=0.77, size=190, family=font, fontface="bold", color="white", lineheight=0.09, hjust=0) +
  draw_text(text="Morningside\nHeights", x=0.46, y=0.62, size=190, family=font, fontface="bold", color="white", lineheight=0.09, hjust=1)  +
  draw_text(text="Elevation (ft)", x=0.75, y=0.3, size=260, family=font, fontface="bold", color="white", lineheight=0.09, hjust=0.5, angle=90)

ggsave("~/30DayMapChallenge/2023/Day 21 - Raster/Day 21 - Raster - Manhattan.png", width=11, height = 14, dpi=1000)

# Brooklyn
map <- ggplot() +
  geom_sf(data=BK_Elev, aes(fill=Elevation_ft), color="transparent") +
  geom_sf(data=Neigh %>% filter(borough == "Brooklyn"), color=alpha("white", 0.75), fill="transparent", size =2) +
  scale_fill_moma_c("Panton", direction = -1, limits = c(-78, 225)) +
  theme_void() + 
  theme(legend.position = c(0.9, 0.98),
        legend.key.width = unit(1.75, "cm"),
        legend.key.height = unit(2.25, "cm"),
        legend.title = element_blank(),
        plot.margin = margin(7, 0, 0, 2, "cm"),
        legend.text = element_text(color="white", hjust = 0.5, size=160, family=font, margin=margin(0, 0, 0, -5.32, "cm"), face="bold")
        )

man_b <- ggdraw(map) +
  theme(plot.background = element_rect(fill="grey10")) + 
  draw_text(text="Twitter: @BlakeRobMills | GitHub: BlakeRMills", color="white", x=0.5, y=0.015, size=175, fontface = "bold", family = font) + 
  draw_text(text="'Heights' of\nBrooklyn", x=0.05, y=0.9, hjust=0, size=875, color="white", family=font, fontface="bold", lineheight = 0.09) +
  draw_text(text="Map displays the elevation of Brooklyn. Neighborhoods\nthat include the word 'Heights' are highlighted.",
            x=0.05, y=0.775, hjust=0, size=175, color="white", family=font, lineheight = 0.12) +
  draw_text(text="Dyker\nHeights", x=.205, y=.165, size=190, family=font, fontface="bold", color="white", lineheight=0.09, hjust=1) +
  draw_text(text="Brooklyn\nHeights", x=.26, y=0.59, size=190, family=font, fontface="bold", color="white", lineheight=0.09, hjust=1) +
  draw_text(text="Crown\nHeights", x=0.51, y=0.525, size=190, family=font, fontface="bold", color="white", lineheight=0.09, hjust=0)  +
  draw_text(text="Prospect\nHeights", x=0.385, y=0.48, size=190, family=font, fontface="bold", color="white", lineheight=0.09, hjust=1)  +
  draw_text(text="Elevation (ft)", x=0.79, y=0.775, size=260, family=font, fontface="bold", color="white", lineheight=0.09, hjust=0.5, angle=90)

ggsave("~/30DayMapChallenge/2023/Day 21 - Raster/Day 21 - Raster - Brooklyn.png", width=11, height = 14, dpi=1000)


# Bronx
map <- ggplot() +
  geom_sf(data=BX_Elev, aes(fill=Elevation_ft), color="transparent") +
  geom_sf(data=Neigh %>% filter(borough == "Bronx"), color=alpha("white", 0.75), fill="transparent", size =2) +
  scale_fill_moma_c("Panton", direction = -1) +
  theme_void() + 
  theme(legend.position = c(0.925, 1.025),
        legend.key.width = unit(1.75, "cm"),
        legend.key.height = unit(2.25, "cm"),
        legend.title = element_blank(),
        plot.margin = margin(8, 0, 0, 0, "cm"),
        legend.text = element_text(color="white", hjust = 0.5, size=160, family=font, margin=margin(0, 0, 0, -5.32, "cm"), face="bold")
  )

man_b <- ggdraw(map) +
  theme(plot.background = element_rect(fill="grey10")) + 
  draw_text(text="Twitter: @BlakeRobMills | GitHub: BlakeRMills", color="white", x=0.5, y=0.015, size=175, fontface = "bold", family = font) + 
  draw_text(text="'Heights' of\nThe Bronx", x=0.05, y=0.9, hjust=0, size=875, color="white", family=font, fontface="bold", lineheight = 0.09) +
  draw_text(text="Map displays the elevation of Bronx Neighborhoods\nthat include the word 'Heights' are highlighted.",
            x=0.05, y=0.78, hjust=0, size=175, color="white", family=font, lineheight = 0.12) +
  draw_text(text="University\nHeights", x=.14, y=0.45, size=190, family=font, fontface="bold", color="white", lineheight=0.09, hjust=1) +
  draw_text(text="Morris\nHeights", x=.1675, y=0.3375, size=190, family=font, fontface="bold", color="white", lineheight=0.09, hjust=0) +
   draw_text(text="Elevation (ft)", x=0.79, y=0.79, size=260, family=font, fontface="bold", color="white", lineheight=0.09, hjust=0.5, angle=90)

ggsave("~/30DayMapChallenge/2023/Day 21 - Raster/Day 21 - Raster - Bronx.png", width=11, height = 14, dpi=1000)


# Queens
map <- ggplot() +
  geom_sf(data=QN_Elev, aes(fill=Elevation_ft), color="transparent") +
  geom_sf(data=Neigh %>% filter(borough == "Queens"), color=alpha("white", 0.75), fill="transparent", size =2) +
  scale_fill_moma_c("Panton", direction = -1) +
  theme_void() + 
  theme(legend.key.width = unit(1.7, "cm"),
        legend.position = c(1.125, 1.025),
        legend.key.height = unit(2.25, "cm"),
        legend.title = element_blank(),
        plot.margin = margin(8, 0, 0, 0, "cm"),
        legend.text = element_text(color="white", hjust = 0.5, size=160, family=font, margin=margin(0, 0, 0, -5.32, "cm"), face="bold")
  )

man_q <- ggdraw(map) +
  theme(plot.background = element_rect(fill="grey10")) + 
  draw_text(text="Twitter: @BlakeRobMills | GitHub: BlakeRMills", color="white", x=0.5, y=0.015, size=175, fontface = "bold", family = font) + 
  draw_text(text="'Heights' of\nQueens", x=0.05, y=0.9, hjust=0, size=875, color="white", family=font, fontface="bold", lineheight = 0.09) +
  draw_text(text="Map displays the elevation of Queens Neighborhoods\nthat include the word 'Heights' are highlighted.",
            x=0.05, y=0.78, hjust=0, size=175, color="white", family=font, lineheight = 0.12) +
  draw_text(text="Jackson\nHeights", x=.31, y=0.63, size=190, family=font, fontface="bold", color="white", lineheight=0.09, hjust=1) +
  draw_text(text="Cambria\nHeights", x=.785, y=0.41, size=190, family=font, fontface="bold", color="white", lineheight=0.09, hjust=0) +
  draw_text(text="Elevation (ft)", x=0.86, y=0.79, size=260, family=font, fontface="bold", color="white", lineheight=0.09, hjust=0.5, angle=90)

ggsave("~/30DayMapChallenge/2023/Day 21 - Raster/Day 21 - Raster - Queens.png", width=11, height = 14, dpi=1000)

# Staten Island
map <- ggplot() +
  geom_sf(data=SI_Elev, aes(fill=Elevation_ft), color="transparent") +
  geom_sf(data=Neigh %>% filter(borough == "Staten Island"), color=alpha("white", 0.75), fill="transparent", size =2) +
  scale_fill_moma_c("Panton", direction = -1) +
  theme_void() + 
  theme(legend.key.width = unit(1.7, "cm"),
        legend.position = c(0.975, 0.2),
        legend.key.height = unit(2.25, "cm"),
        legend.title = element_blank(),
        plot.margin = margin(5, 0, 0, 2, "cm"),
        legend.text = element_text(color="white", hjust = 0.5, size=160, family=font, margin=margin(0, 0, 0, -5.32, "cm"), face="bold")
  )

man_si <- ggdraw(map) +
  theme(plot.background = element_rect(fill="grey10")) + 
  draw_text(text="Twitter: @BlakeRobMills | GitHub: BlakeRMills", color="white", x=0.5, y=0.015, size=175, fontface = "bold", family = font) + 
  draw_text(text="'Heights' of\nStaten Island", x=0.05, y=0.9, hjust=0, size=875, color="white", family=font, fontface="bold", lineheight = 0.09) +
  draw_text(text="Map displays the elevation of Staten Island Neighborhoods\nthat include the word 'Heights' are highlighted.",
            x=0.05, y=0.78, hjust=0, size=175, color="white", family=font, lineheight = 0.12) +
  draw_text(text="Arden\nHeights", x=.485, y=0.35, size=190, family=font, fontface="bold", color="white", lineheight=0.09, hjust=0) +
  draw_text(text="Elevation (ft)", x=0.86, y=0.214, size=260, family=font, fontface="bold", color="white", lineheight=0.09, hjust=0.5, angle=90)

ggsave("~/30DayMapChallenge/2023/Day 21 - Raster/Day 21 - Raster - Staten Island.png", width=11, height = 14, dpi=1000)
