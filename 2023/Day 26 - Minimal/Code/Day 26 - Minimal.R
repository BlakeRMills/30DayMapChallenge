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

# Aes

# Function
`%notin%` <- Negate(`%in%`)

# Data
Man <-  geojson_sf("~/30DayMapChallenge/2023/Day 01 - Points/Data/Borough Boundaries.geojson") %>%
  filter(boro_name == "Manhattan") %>%
  st_cast("POLYGON") %>%
  mutate(Area = st_area(.)) %>%
  arrange(-Area) %>%
  .[c(1, 5), ] %>%
  st_combine()

Neigh <- geojson_sf("~/30DayMapChallenge/2023/Day 19 - 5 Minute Map/Data/NYC_Neighborhoods_BetaNYC.geojson") %>%
  filter(borough == "Manhattan", 
         neighborhood %notin% c("Randall's Island", "Roosevelt Island"))

# Cleaning  
Grid <- st_make_grid(Man, n=c(24, 50))

Grid <- st_as_sf(Grid)[Man, ] %>%
  mutate(id = row_number())

main_neigh <- st_intersection(Neigh, Grid) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  dplyr::select(id, intersect_area, neighborhood) %>%   # only select columns needed to merge
  st_drop_geometry() %>%
  group_by(id) %>%
  slice_max(order_by = intersect_area) %>%
  ungroup()

grid_final <- left_join(Grid, main_neigh, by = "id") %>%
  mutate(center = st_centroid(x),
         image = case_when(neighborhood == "Central Park" ~ "~/Desktop/Drawn 8 Bit/Tree.PNG",
                           neighborhood %in% c("Upper East Side", "Upper West Side") ~ "~/Desktop/Drawn 8 Bit/Brownstone_1.PNG",
                           neighborhood %in% c("Harlem", "Lower East Side", "West Village") ~ "~/Desktop/Drawn 8 Bit/Brown3.PNG",
                           neighborhood == "Chinatown" ~ "~/Desktop/Drawn 8 Bit/Lantern.PNG"
                           ))


m <- ggplot() +
  geom_sf(data=grid_final, aes(fill=neighborhood, geometry=x), color="transparent") +
  geom_image(data=grid_final, aes(geometry=center, image=image), stat="sf_coordinates", size =0.0183) +
  theme_void() +
  #theme(legend.position="none") +
  coord_sf()

ggsave("~/30DayMapChallenge/2023/Day 26 - Minimal/Day 26 - Minimal.png", width=11, height=14)
