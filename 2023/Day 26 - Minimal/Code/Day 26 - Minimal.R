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
set.seed(2)
pal <- moma.colors("Liu", 32) %>% sample
showtext_auto()
font <- "Advent Pro"
font_add_google(font)

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
         neighborhood %notin% c("Randall's Island", "Roosevelt Island", "Governors Island", "Liberty Island", "Ellis Island"))

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
         image = case_when(neighborhood %in% c("Central Park", "Battery Park City") ~ "~/Desktop/Drawn 8 Bit/Tree.PNG",
                           neighborhood %in% c("Kips Bay", "Upper West Side") ~ "~/Desktop/Drawn 8 Bit/Brownstone1.PNG",
                           neighborhood %in% c("Harlem", "East Village") ~ "~/Desktop/Drawn 8 Bit/Apartment2.PNG",
                           neighborhood == "Chinatown" ~ "~/Desktop/Drawn 8 Bit/Lantern.PNG",
                           neighborhood == "Financial District" ~ "~/Desktop/Drawn 8 Bit/Bull.PNG",
                           neighborhood == "Tribeca" ~ "~/Desktop/Drawn 8 Bit/Rothko.PNG",
                           neighborhood %in% c("Washington Heights", "Hell's Kitchen") ~ "~/Desktop/Drawn 8 Bit/Apartment1.PNG",
                           neighborhood == "Morningside Heights" ~  "~/Desktop/Drawn 8 Bit/Low.PNG",
                           neighborhood %in% c("SoHo", "NoHo") ~ "~/Desktop/Drawn 8 Bit/Purse.PNG",
                           neighborhood == "Flatiron District" ~ "~/Desktop/Drawn 8 Bit/Flatiron.PNG",
                           neighborhood == "Nolita" ~ "~/Desktop/Drawn 8 Bit/Latte.PNG",
                           neighborhood == "Little Italy" ~ "~/Desktop/Drawn 8 Bit/Pizza.PNG",
                           neighborhood == "Two Bridges" ~ "~/Desktop/Drawn 8 Bit/Bridge.PNG",
                           neighborhood == "Stuyvesant Town" ~ "~/Desktop/Drawn 8 Bit/Tower.PNG",
                           neighborhood == "Murray Hill" ~ "~/Desktop/Drawn 8 Bit/Money.PNG",
                           neighborhood == "Theater District" ~ "~/Desktop/Drawn 8 Bit/Masks.PNG",
                           neighborhood == "Civic Center" ~ "~/Desktop/Drawn 8 Bit/Gov.PNG",
                           neighborhood == "Chelsea" ~ "~/Desktop/Drawn 8 Bit/Warhol.PNG",
                           neighborhood == "Midtown" ~ "~/Desktop/Drawn 8 Bit/Empire.PNG",
                           neighborhood == "Upper East Side" ~ "~/Desktop/Drawn 8 Bit/Brownstone2.PNG",
                           neighborhood %in% c("West Village") ~ "~/Desktop/Drawn 8 Bit/Apartment3.PNG",
                           neighborhood %in% c("Marble Hill", 'Greenwich Village') ~ "~/Desktop/Drawn 8 Bit/Apartment4.PNG",
                           neighborhood %in% c("Inwood", "Lower East Side") ~ "~/Desktop/Drawn 8 Bit/Apartment5.PNG",
                           neighborhood %in% c("East Harlem", "Gramercy") ~ "~/Desktop/Drawn 8 Bit/Apartment6.PNG"
                           ))


m <- ggplot() +
  geom_sf(data=grid_final, aes(fill=neighborhood, geometry=x), color="transparent") +
  geom_image(data=grid_final, aes(geometry=center, image=image), stat="sf_coordinates", size =0.0183) +
  scale_fill_manual(values = pal) +
  theme_void() +
  theme(legend.position="none") +
  coord_sf() 

g <- ggdraw(m) +
  theme(plot.background = element_rect(fill="#fdf9f5", color="transparent")) +
  draw_label(label="Twitter: @BlakeRobMills | GitHub: BlakeRMills", color=pal[1], x=0.5, y=0.0125, size=150, fontface = "bold", fontfamily = font) +
  draw_label(label="8-Bit\nManhattan\nNeighborhoods", color=pal[1], x=0.05, y=0.875, size=650, fontface = "bold", fontfamily = font, lineheight = 0.09, hjust=0) 
  

ggsave("~/30DayMapChallenge/2023/Day 26 - Minimal/Day 26 - Minimal.png", width=11, height=14, dpi=1000)
