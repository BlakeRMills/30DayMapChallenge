# Libraries
library(cowplot)
library(geojsonio)
library(MoMAColors)
library(tidyverse)
library(sf)
library(showtext)
library(sysfonts)

# Aes
showtext_auto()
font <- "Notable"
font_add_google(font)
pal <- c(moma.colors("Klein")[2],
         "#7D4326", "#08806E", "#3C4349",
         moma.colors("Klein")[c(5, 3)],
         "#654D80",
         moma.colors("Klein")[c(10, 4)])
`%notin%` <- Negate(`%in%`)

# Data
Line <- geojson_sf("~/30DayMapChallenge/2023/Day 11 - Retro/Data/Subway Lines.geojson")
Stations <- geojson_sf("~/30DayMapChallenge/2023/Day 11 - Retro/Data/Subway Stations.geojson")

# Cleaning
Line <- Line %>%
  mutate(color = case_when(grepl("1|2|3", name) ~ "Red",
                           grepl("B|D|F|M", name) ~ "Orange",
                           grepl("N|Q|R|W", name) ~ "Yellow",
                           grepl("4|5|6", name) ~ "Green",
                           grepl("A|C|E", name) ~ "Blue",
                           grepl("J|Z", name) ~ "Brown",
                           grepl("L|S", name) ~ "Grey",
                           grepl("7", name) ~ "Purple",
                           grepl("G", name) ~ "Lime"))

Stations_c <- Stations %>%
  filter(name %in% c("Van Cortlandt Park - 242nd St",
                     "South Ferry",
                     "Wakefield - 241st St",
                     "Harlem - 148 St",
                     "Brooklyn College - Flatbush Ave",
                     "New Lots Ave",
                     "Inwood - 207th St",
                     "Rockaway Park - Beach 116 St",
                     "Bedford Park Blvd",
                     "Brighton Beach",
                     "168th St",
                     "Euclid Ave",
                     "Norwood - 205th St",
                     "Coney Island - Stillwell Av",
                     "Jamaica Ctr - Parsons / Archer",
                     "World Trade Center",
                     "Jamaica - 179th St",
                     "Long Island City - Court Sq",
                     "Church Ave",
                     "Broad St",
                     "8th Ave",
                     "Canarsie - Rockaway Pkwy",
                     "Forest Hills - 71st Av",
                     "Middle Village - Metropolitan Ave",
                     "Astoria - Ditmars Blvd",
                     "96th St",
                     "Bay Ridge - 95th St",
                     "Whitehall St",
                     "Woodlawn",
                     "Crown Hts - Utica Ave",
                     "Brooklyn Bridge - City Hall",
                     "Flushing - Main St",
                     "34th St - Hudson Yards") |
           objectid %in% c("280", "278", "196", "55", "201" )) %>%
  mutate(line = str_remove(line, "Express"),
         color = case_when(grepl("1|2|3", line) ~ "Red",
                           grepl("B|D|F|M", line) ~ "Orange",
                           grepl("N|Q|R|W", line) ~ "Yellow",
                           grepl("4|5|6", line) ~ "Green",
                           grepl("A|C|E", line) ~ "Blue",
                           grepl("J|Z", line) ~ "Brown",
                           grepl("L|S", line) ~ "Grey",
                           grepl("7", line) ~ "Purple",
                           grepl("G", line) ~ "Lime")) %>%
  filter(objectid %notin% c("158", "177", "245", "223", "135", "131", "112", "111", "77",
                            "421", "409", "428", "643", "418", "110", "459", "29", "57", 
                            "58", "265"))

# Map
p <- ggplot() + 
  geom_sf(data = Line, aes(color = color)) +
  geom_sf(data = Stations_c, aes(color = color, geometry=geometry, size=objectid)) +
  theme_void() +
  ggtitle("New York Subway") +
  theme(legend.position = "none",
        plot.title = element_text(family=font, size=92, hjust = 0.5, color = "#68B5AF")) +
  scale_color_manual(values = pal) 

ggdraw(p) +
  theme(plot.margin = margin(0.5, 0, 0.25, 0, "cm"),
        plot.background = element_rect(fill="#211313", color="#211313")) +
  draw_label(label="Twitter: @BlakeRobMills | NYC Open Data | GitHub: BlakeRMills", color="#68B5AF", x=0.5, y=0.005, size=22, fontface = "bold", fontfamily = font)  
  
ggsave("~/30DayMapChallenge/2023/Day 11 - Retro/Day 11 - Retro.png", height = 9, width = 6)
