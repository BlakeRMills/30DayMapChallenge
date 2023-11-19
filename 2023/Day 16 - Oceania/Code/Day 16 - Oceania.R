# Libraries
library(cowplot)
library(ggrepel)
library(ggtext)
library(geojsonio)
library(MoMAColors)
library(osmdata)
library(tidyverse)
library(sf)
library(showtext)
library(sysfonts)

# Aes
sf_use_s2(FALSE)
showtext_auto()
font <- "Advent Pro"
font_add_google(font)
`%notin%` <- Negate(`%in%`)

# Data
GBR <- geojson_sf("~/30DayMapChallenge/2023/Day 16 - Oceania/Data/GBR.geojson") 

# Cleaning
GBR <- GBR %>%
  filter(FEAT_NAME != "Sand")

names <- GBR %>%
  mutate(area = st_area(geometry)) %>%
  arrange(-area) %>%
  .[1:15, ] %>%
  filter(GBR_NAME %notin% c("U/N Reef",
                           "Cockburn Reef",
                          "Gallon Reef"),
         nchar(GBR_NAME) < 30) %>%
  mutate(GBR_NAME = str_replace_all(GBR_NAME, " ", "\n"))
  
# Plot 
ggplot() + 
  geom_sf(data=GBR %>% filter(FEAT_NAME=="Mainland"), fill = "#E4E2CD", color="transparent") +
  geom_sf(data=GBR %>% filter(FEAT_NAME !="Mainland"), aes(fill = FEAT_NAME), color="transparent") +
  geom_sf_label(data=names %>% filter(FEAT_NAME == "Mainland"), aes(label=GBR_NAME), color = "#E4E2CD",
               family = font, fontface="bold", lineheight=0.4, size =30, label.size = NA, fill = alpha("#fdf9f5", 0.75),
               nudge_y = -2,  nudge_x = 1) +
  geom_sf_label(data=names %>% filter(FEAT_NAME == "Island"), aes(label=GBR_NAME), color = "#406c6d",
                family = font, fontface="bold", lineheight=0.4, size =10, label.size = NA, fill = alpha("#fdf9f5", 0.75),
                nudge_y = -0.65,  nudge_x = -0.75) +
  geom_sf_label(data=names %>% filter(GBR_NAME != "Mason\nReef", FEAT_NAME == "Reef"), aes(label=GBR_NAME), color = "#934A3B",
                family = font, fontface="bold", lineheight=0.4, size =10, label.size = NA, fill = alpha("#fdf9f5", 0.75),
                nudge_y = 0.5,  nudge_x = 0.65) +
  geom_sf_label(data=names %>% filter(GBR_NAME == "Mason\nReef"), aes(label=GBR_NAME), color = "#934A3B",
                family = font, fontface="bold", lineheight=0.4, size =10, label.size = NA, fill = alpha("#fdf9f5", 0.75),
                nudge_y = 0,  nudge_x = -0.95) +
  scale_fill_manual(values = c("#586b44", "#406c6d", "#934A3B", "#9EB9CD", "#955020")) +
  scale_x_continuous(expand=c(0.1, 0.1)) +
  ggtitle("Great Barrier Reef",
          subtitle = "Map displays <span style = 'color:#586b44'>**Cays**</span>, <span style = 'color:#406c6d'>**Islands**</span>, <span style = 'color:#934A3B'>**Reefs**</span>, and <span style = 'color:#955020'>**Rocks**</span> of the Great Barrier Reef.") + 
  labs(caption="Twitter: @BlakeRobMills | Source: Data.Gov.AU | GitHub: BlakeRMills") +
  theme_void() +
  theme(plot.background =element_rect(fill="#fdf9f5", color="#fdf9f5"),
        legend.position = "none", 
        plot.title = element_text(family=font, face="bold", size=150, color="#406c6d"),
        plot.subtitle = element_textbox(family=font, face="bold", size=50),
        plot.caption = element_text(family=font, face="bold", size=40, color="#406c6d", hjust=0.5, vjust=10),
        plot.margin = margin(0.5, 0, -0.75, 0, "cm"))

ggsave("~/30DayMapChallenge/2023/Day 16 - Oceania/Day 16 - Oceania.png", width = 8.5, height = 10)

