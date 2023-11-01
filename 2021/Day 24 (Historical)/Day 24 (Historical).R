library(geojsonio)
library(sysfonts)
library(showtext)
library(sf)
library(cowplot)
library(tidyverse)

# Aes 
colorsMain <- c("#6a1e15", "#ce6d3c", "#dd956a", "#ce9344", "#e8b960", "#646e3b", "#2c4f4a", "#487e90", "#1c3c55")
fontsize <- 52
labColor <- "#32130f"
showtext_auto()
font_add_google("Amiri")
font_add_google("Cinzel")
font1 <- "Amiri"
font2 <- "Cinzel"

# Data
build <- geojson_sf("~/Desktop/Day 24 (Historical)/Data/Buildings.geojson")
streets <- geojson_sf("~/Desktop/Day 24 (Historical)/Data/Streets.geojson")

# Cleaning 
build <- build %>% dplyr::filter(ESCHEBACH_ != "Unknown") %>%
  mutate(ESCHEBACH_ = factor(ESCHEBACH_, 
                             levels=c("Temples", "Market Garden", "Entertainment Buildings",
                                      "Workshops", "Shops", "Guilds", "Public Buildings",
                                      "Urban Supply System", "Private Dwellings")))


# Plot
Main <- ggplot() +
  geom_sf(data=streets, alpha=0.5, color=labColor, fill="#fdf9f5", size=0.65) +
  geom_sf(data=build, aes(fill=ESCHEBACH_), color="#fdf9f5", size=0.16) +
  scale_fill_manual(values=colorsMain, guide = guide_legend(ncow=1)) +
  coord_sf() + 
  theme_void() +
  theme(legend.key.width = unit(1, "cm"),
        legend.key.height = unit(1, "cm"),
        legend.title = element_blank(),
        legend.text = element_blank(),
        plot.margin = margin(0, 7.1, -3.5, 0.5, "cm"))

# Annotations
ggdraw(Main) +
  draw_text(text="Temple", x=0.815, y=0.595, size=fontsize, color=colorsMain[1], family=font2, hjust=0, fontface="bold") +
  draw_text(text="Market Garden", x=0.815, y=0.55, size=fontsize, color=colorsMain[2], family=font2, hjust=0, fontface="bold") +
  draw_text(text="Entertainment", x=0.815, y=0.506, size=fontsize, color=colorsMain[3], family=font2, hjust=0, fontface="bold") +
  draw_text(text="Workshop", x=0.814, y=0.462, size=fontsize, color=colorsMain[4], family=font2, hjust=0, fontface="bold") +
  draw_text(text="Shop", x=0.815, y=0.418, size=fontsize, color=colorsMain[5], family=font2, hjust=0, fontface="bold") +
  draw_text(text="Guild", x=0.815, y=0.374, size=fontsize, color=colorsMain[6], family=font2, hjust=0, fontface="bold") +
  draw_text(text="Public Building", x=0.815, y=.33, size=fontsize, color=colorsMain[7], family=font2, hjust=0, fontface="bold") +
  draw_text(text="Urban Supply System", x=0.815, y=0.287, size=fontsize, color=colorsMain[8], family=font2, hjust=0, fontface="bold") +
  draw_text(text="Private Dwelling", x=0.815, y=0.243, size=fontsize, color=colorsMain[9], family=font2, hjust=0, fontface="bold") +
  draw_text(text="Pompeii", x=0.5, y=0.93, size=250, color=labColor, family=font2, fontface="bold") +
  draw_text(text="Map shows discovered buildings and streets in the ruins of Pompeii. Buildings are colored based on primary\nfunction as defined by historians Liselotte Eschebach and Jürgen Müller-Trollius (1993).", 
            x=0.5, y=0.832, size=53, color=labColor, family=font1, lineheight=0.3) +
  draw_text(text="Twitter: @BlakeRobMills | Source: Eric Poehler, Pompeii Bibliography and Mapping Project  | GitHub: BlakeRMills", x=0.5, y=0.02, size=40, family=font1, color=labColor, fontface="bold") +
  theme(plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
        panel.background = element_rect(fill="#fdf9f5", color="#fdf9f5")) 

# Save
ggsave("~/Desktop/Pompeii.png", height = 9, width = 15)


