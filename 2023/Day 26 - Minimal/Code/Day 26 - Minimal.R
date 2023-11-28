# Libraries
library(cowplot)
library(geojsonio)
library(MoMAColors)
library(osmdata)
library(raster)
library(sf)
library(showtext)
library(sysfonts)
library(tidyverse)

# Data
NYC <- geojson_sf("~/30DayMapChallenge/2023/Day 01 - Points/Data/Borough Boundaries.geojson")
Man <-  geojson_sf("~/30DayMapChallenge/2023/Day 01 - Points/Data/Borough Boundaries.geojson") %>%
  filter(boro_name == "Manhattan") %>%
  st_cast("POLYGON") %>%
  mutate(Area = st_area(.)) %>%
  arrange(-Area) %>%
  .[c(1,5), ] %>%
  st_combine()

x <- st_make_grid(Man, n=c(24, 50))

t <- st_as_sf(x)[Man, ]


ggplot() +
  geom_sf(data=t) 
  geom_sf(data=Man) +
  geom_sf(data = x, fill="transparent", color="red") +
  coord_sf()

ggsave("~/30DayMapChallenge/2023/Day 26 - Minimal/Day 26 - Minimal.png", width=11, height=14)
