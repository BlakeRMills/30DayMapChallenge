# Libraries
library(cowplot)
library(geojsonio)
library(MoMAColors)
library(RSocrata)
library(sf)
library(showtext)
library(sysfonts)
library(tidyverse)

# Aes
showtext_auto()
font <- "Advent Pro"
font_add_google(font)
sf_use_s2(FALSE)

# Data
Trees <- read.socrata("https://data.cityofnewyork.us/resource/erm2-nwe9.csv?complaint_type=New Tree Request")
Man <- geojson_sf("~/30DayMapChallenge/2023/Day 01 - Points/Data/Borough Boundaries.geojson") %>%
  filter(boro_name == "Manhattan") %>%
  st_cast("POLYGON") %>%
  mutate(Area = st_area(.)) %>%
  arrange(-Area) %>%
  .[c(1, 5), ] %>%
  st_combine()

grid <-  st_make_grid(Man, n=c(24, 50)) %>%
  st_as_sf()

# Cleaning
grid <- st_as_sf(grid)[Man, ] %>%
  mutate(center = st_centroid(x))

grid_tree <- Trees %>% 
  filter(closed_date < Sys.Date(),
         created_date > Sys.Date() - years(10),
         is.na(latitude) == FALSE) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
  st_intersection(., Man)

grid$tree_points <- st_intersects(grid, grid_tree) %>% lengths()

# Map
m <- ggplot() + 
  geom_sf(data=grid, aes(size=tree_points, geometry=center, color=tree_points), alpha=0.8) + 
  scale_color_moma_c("Panton", direction = -1, limits=c(0, 300)) +
  theme_void() +
  scale_size_continuous(range=c(1, 10)) +
  theme(legend.position = c(1, 0.3),
        legend.key.width = unit(1.75, "cm"),
        legend.key.height = unit(2.25, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(color="white", hjust = 0.5, size=50, family=font, margin=margin(0, 0, 0, -2.4, "cm"), face="bold")) +
  guides(size = guide_none())

ggdraw(m) +
  theme(plot.background = element_rect(fill="grey10", color="transparent")) +
  draw_text(text="Manhattan\nTree Requests", x=0.05, y=0.915, hjust=0, size=200, color="white", family=font, fontface="bold", lineheight = 0.3) +
  draw_text(text="Map displays the number of 311 call request for new trees in\nManhattan (Nov 2013 - Nov 2023). Dots are sized and colored\nto display number of requests in the surrounding area.", 
            x=0.05, y=0.805, hjust=0, size=50, color="white", family=font, lineheight = 0.325) +
  draw_text(text="Number of Tree Requests", color="white", x=0.73, y=0.3, size=75, fontface = "bold", family = font, angle=90) +
  draw_text(text="Twitter: @BlakeRobMills | NYC Open Data | GitHub: BlakeRMills", color="white", x=0.5, y=0.02, size=50, fontface = "bold", family = font) 
  

ggsave("~/30DayMapChallenge/2023/Day 27 - Dot/Day 27 - Dot.png", height = 14, width = 11) 


