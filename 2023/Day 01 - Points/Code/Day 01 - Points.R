# Libraries
library(rjson)
library(tidyverse)
library(data.table)
library(geojsonio)
library(sf)
library(osmdata)
library(MoMAColors)
library(showtext)
library(cowplot)
library(sysfonts)

# Aes
showtext_auto()
font <- "Advent Pro"
font_add_google(font)

# Data
publicArt <- fromJSON(file="~/30DayMapChallenge/2023/Day 01 - Points/Data/DPR_PublicArt_001.json")
Man <- geojson_sf("~/30DayMapChallenge/2023/Day 01 - Points/Data/Borough Boundaries.geojson") 
Central <- geojson_sf("~/30DayMapChallenge/2023/Day 01 - Points/Data/Parks Properties_20231101.geojson") %>%
  filter(name311 == "Central Park")
points <- data.frame(side = c("left", "right"),
                     lat = c(40.75, 40.74, 40.82, 40.82), 
                     lng = c(-74.02, -73.96, -73.99, -73.92)) %>%
  st_as_sf(., coords = c("lng", "lat"), 
           crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# Cleaning

Man <- Man %>% 
  filter(boro_name == "Manhattan") %>%
  st_cast("POLYGON") %>%
  mutate(Area = st_area(.)) %>%
  arrange(-Area) %>%
  .[1, ]

publicArt <- data.table(t(sapply(publicArt, function(x) unlist(lapply(x, function(x) ifelse(is.null(x),NA,x))))))

publicArt_sf <-  publicArt %>% 
  mutate(lat = as.numeric(lat),
         lng = as.numeric(lng),
         start = as.Date(from_date),
         end = as.Date(to_date),
         time = (end - start) %>% as.numeric(),
         current = end > Sys.Date()) %>%
  filter(is.na(lat) == F, 
         lng < 0,
         lat > 0,
         borough == "M", 
         time >= 0) %>%
  mutate(cur_lat = lat, 
         cur_lng = lng) %>% 
  st_as_sf(., 
           coords = c("lng", "lat"), 
           crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
  st_filter(., Man) %>% 
  arrange(time) %>%
  mutate(near = st_nearest_feature(., points),
         name = ifelse(name == "Action 182 : Site of the Fall - Study of the Renaissance Garden, At 01:01pm Saturday 03 Feb 1968", "Action 182", name),
         cur_lng = ifelse(near %in% c(1,3), cur_lng-0.03, cur_lng+0.03),
         st_lng = st_coordinates(.)[,1],
         st_lat = st_coordinates(.)[,2],
         cur_lat = case_when(name == "LIFE DANCE" ~ cur_lat + 0.001,
                             name == "Hope" ~ cur_lat - 0.001,
                             name == "Franklin Street Four" ~ cur_lat + 0.0045,
                             name == "The wind blows where it wishes" ~ cur_lat + 0.001,
                             name == "Hoodoos" ~ cur_lat + 0.0025,
                             name == "PRANK" ~ cur_lat - 0.0015,
                             name == "Action 182" ~ cur_lat - 0.0018,
                             name == "Column-Untitled No.3" ~ cur_lat - 0.00075,
                             name == "WHAT IF THEY BARK?" ~ cur_lat - 0.0009,
                             name == "Alice on the Wall" ~ cur_lat + 0.0018,
                             name == "Bloomingdale Medallions" ~ cur_lat - 0.002,
                             name == "The Washington Market Park Courts" ~ cur_lat - 0.0006,
                             name == "Setting the Stage for Climate Change" ~ cur_lat + 0.00225,
                             name == "Two Kinds of Nature" ~ cur_lat + 0.0004,
                             name == "FACES OF HARLEM" ~ cur_lat + 0.00175,
                             name == "Joined an Avalanche, Never to be Alone Again" ~ cur_lat - 0.00175,
                             name == "Willie Cole on Park Avenue" ~ cur_lat + 0.00175,
                             name == "Breathing Court" ~ cur_lat + 0.00075,
                             name == "Five Stars" ~ cur_lat - 0.00075,
                             .default = cur_lat),
         cur_lng = ifelse(name == "Washington Heights Youth Connection", cur_lng - 0.02, cur_lng))

# Plot
plot <- ggplot() +
  geom_sf(data=Man, color="transparent", fill="#f6e5d4") +
  geom_sf(data=Central, fill=moma.colors("Kippenberger")[7], color="transparent", alpha=0.4) +
  geom_sf(data=publicArt_sf, aes(color=time), pch = 17, alpha = 0.8) +
  geom_point(data=publicArt_sf %>% filter(current==T), aes(x=cur_lng, y=cur_lat, color=time), alpha = 1, size=0.9) + 
  geom_text(data=publicArt_sf %>% filter(current==T, near %in% c(1,3)), aes(x=cur_lng-0.002, y=cur_lat, label=name), color=moma.colors("Ernst")[8], alpha = 1, family=font, fontface="bold", hjust=1, size=6) +
  geom_text(data=publicArt_sf %>% filter(current==T, near %in% c(2,4)), aes(x=cur_lng+0.002, y=cur_lat, label=name), color=moma.colors("Ernst")[8], alpha = 1, family=font, fontface="bold", hjust=0, size=6) +
  geom_segment(data=publicArt_sf %>% filter(current==T), aes(x=cur_lng, y=cur_lat, xend=st_lng, yend=st_lat, color=time), alpha = 0.8) + 
  scale_color_moma_c("Ernst") + 
  theme_void() +
  scale_x_continuous(limits = c(-74.07, -73.89)) +
  labs(color = "Days on Display") +
  theme(legend.position = c(0.25, 0.73), 
        legend.direction = "horizontal",
        legend.key.width = unit(1.2, "cm"),
        legend.key.height = unit(0.9, "cm"),
        legend.title = element_blank(), 
        legend.text = element_blank()) 
ggdraw(plot) + 
  draw_label(label="Manhattan Public Art", color=moma.colors("Ernst")[8], x=0.025, y=0.93, size=115, fontface = "bold", fontfamily = font, hjust=0) + 
  draw_label(label="Plot displays the locations of Temporary Public Art Displays in Manhattan\nfrom 2011 to 2023, with points colored by number of days on display.\nLabeled works are currently on display as of November 1, 2023.", color=moma.colors("Ernst")[8], x=0.026, y=0.86, size=30, fontfamily = font, hjust=0, lineheight=0.325) + 
  draw_label(label="Twitter: @BlakeRobMills | Source: NYC Open Data | GitHub: BlakeRMills", color=moma.colors("Ernst")[8], x=0.5, y=0.0175, size=30, fontface = "bold", fontfamily = font) + 
  draw_label(label="0", color=moma.colors("Ernst")[8], x=0.115, y=0.697, size=30, fontfamily = font) + 
  draw_label(label="200", color=moma.colors("Ernst")[8], x=0.225, y=0.697, size=30, fontfamily = font) + 
  draw_label(label="400", color=moma.colors("Ernst")[8], x=0.335, y=0.697, size=30, fontfamily = font) + 
  draw_label(label="600", color=moma.colors("Ernst")[8], x=0.445, y=0.697, size=30, fontfamily = font) +
  draw_label(label="Days on Display", color=moma.colors("Ernst")[8], x=0.286, y=0.78, size=50, fontfamily = font, fontface = "bold") +
  theme(plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"))

ggsave("~/Desktop/Day 01 - Points.png", height = 8, width = 7)

