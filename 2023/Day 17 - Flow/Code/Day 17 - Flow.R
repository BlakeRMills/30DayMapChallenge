# Libraries
library(arrow)
library(cowplot)
library(geojsonio)
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

# Data
taxi <- data.frame()
for(i in 1:9){
  month <- read_parquet(paste("~/30DayMapChallenge/2023/Day 17 - Flow/Data/yellow_tripdata_2023-0", i, ".parquet", sep=""))
  taxi <- rbind(taxi, month)
}
zones <- geojson_sf("~/30DayMapChallenge/2023/Day 17 - Flow/Data/NYC Taxi Zones.geojson")
NYC <- geojson_sf("~/30DayMapChallenge/2023/Day 01 - Points/Data/Borough Boundaries.geojson") %>%
  filter(boro_name == "Manhattan") %>%
  st_cast("POLYGON") %>%
  mutate(Area = st_area(.)) %>%
  arrange(-Area) %>%
  .[c(1,5), ] %>%
  st_combine()

Roads <- opq(bbox=st_bbox(NYC)) %>%
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
  st_intersection(., NYC)

# Cleaning
taxi_PU <- taxi %>%
  count(PULocationID) %>%
  mutate(PULocationID = as.character(PULocationID))

taxi_DO <- taxi %>%
  count(DOLocationID) %>%
  mutate(DOLocationID = as.character(DOLocationID))

zones_sum <- zones %>%
  left_join(., taxi_PU, by = c("location_id" = "PULocationID")) %>%
  left_join(., taxi_DO, by = c("location_id" = "DOLocationID")) %>%
  dplyr::rename(c("PU_n" = "n.x",
                  "DO_n" = "n.y")) %>%
  mutate(diff = DO_n - PU_n)

diff_df <-st_intersection(Roads, zones_sum)

limit <- max(abs(diff_df$diff)) * c(-1, 1)

# Plot
plot <- ggplot() + 
  geom_sf(data=diff_df, aes(color=diff)) +
  scale_color_moma_c("Kippenberger", limit = limit) +
  theme_void() + 
  theme(legend.text = element_blank(),
        legend.title = element_blank(),
        plot.background =element_rect(fill="#fdf9f5", color="#fdf9f5"),
        legend.position = c(0.87, 0.3),
        legend.key.width = unit(1.75, "cm"),
        legend.key.height = unit(2.25, "cm")) +
  guides(color = guide_colorbar(ticks.colour = "transparent"))

final <- ggdraw(plot) +
  theme(plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
        plot.margin = margin(0, 0.5, 0, 0, "cm")) +
  draw_label(label="Twitter: @BlakeRobMills | Source: NYC Open Data | GitHub: BlakeRMills", color=moma.colors("Kippenberger")[1], x=0.5, y=0.0175, size=50, fontface = "bold", fontfamily = font) +
  draw_label(label = "Taxi Trips in\nManhattan", x=0.05, y=0.875, hjust=0, size=270, color=moma.colors("Kippenberger")[1], fontfamily=font, fontface="bold", lineheight = 0.3) +
  draw_label(label = "More Pick-Ups than Drop-Offs", x=0.75, y=0.095, hjust=1, size=58,color=moma.colors("Kippenberger")[1], fontfamily=font) +
  draw_label(label = "More Drop-Offs than Pick-Ups", x=0.7, y=0.495, hjust=0, size=58,color=moma.colors("Kippenberger")[11], fontfamily=font) +
  draw_label(label = "Difference in Pick-Ups & Drop-Offs", x=0.67, y=0.3, angle=90, size=60,color=moma.colors("Kippenberger")[1], fontfamily=font, fontface="bold") +
  draw_label(label = "Map displays Manhattan streets colored by the\ndifference in yellow taxi pick-ups and drop-offs.\n(Jan - Sept 2023)", x=0.05, y=0.74, hjust=0, size=55, color=moma.colors("Kippenberger")[1], fontfamily=font, lineheight = 0.4) +
  annotate(geom = "curve", x=0.76, xend=0.77, y=0.095, yend=0.14, color=moma.colors("Kippenberger")[1], arrow = arrow(length = unit(3, "mm")), curvature = 0.85, linewidth=0.75) +
  annotate(geom = "curve", x=0.69, xend=0.68, y=0.495, yend=0.45, color=moma.colors("Kippenberger")[11], arrow = arrow(length = unit(3, "mm")), curvature = 0.85, linewidth=0.75)

ggsave("~/30DayMapChallenge/2023/Day 17 - Flow/Day 17 - Flow.png", width = 11, height = 14)
