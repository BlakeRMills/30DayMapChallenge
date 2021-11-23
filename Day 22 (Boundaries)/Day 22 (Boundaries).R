# Libraries
library(readxl)
library(cowplot)
library(scales)
library(sysfonts)
library(showtext)
library(tidyverse)
library(geojsonio)

# Aes
showtext_auto()
font_add_google("Fira Sans")
font_add_google("Oswald")
font1 <- "Fira Sans"
font2 <- "Oswald"

# Data
Bound <- geojson_sf("~/Desktop/Day 22 (Boundaries)/Data/Community Districts.geojson")
Health <- read_xlsx("~/Desktop/Day 22 (Boundaries)/Data/CommHealth.xlsx")

# Wrangling
Bound <- Bound %>% filter(boro_cd %in% unique(Health$ID))
Health <- Health %>% select(ID, Borough, Name, Edu_Did_Not_Complete_HS, Poverty, Jail_Incarceration,
                            Rent_Burden, Air_Pollution, Ratio_Bodega_Supermarket) %>% 
  mutate(ID = as.character(ID)) %>% 
  left_join(Bound, by = c("ID" = "boro_cd"))


# Individual Plots
## Poverty
Poverty <- ggplot() + 
  geom_sf(data=Health, aes(fill=Poverty, geometry=geometry), color="transparent") +
  coord_sf() +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Blues")[3:9], 
                       labels=c("10%", "15%", "20%", "25%", "30%"),
                       breaks=c(10,15,20,25,30),
                       guide = guide_colorbar(ticks.linewidth = 4)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        plot.margin = margin(2.5, 0.5, 1.25, 0.5, "cm"),
        legend.key.width = unit(2.75, "cm"),
        legend.key.height = unit(1.5, "cm"),
        legend.text = element_text(size=75, face="bold", family=font2, color="#08306B"))

## Education
Education <- ggplot() + 
  geom_sf(data=Health, aes(fill=Edu_Did_Not_Complete_HS, geometry=geometry), color="transparent") +
  coord_sf() +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds")[3:9],
                       labels=c("10%", "20%", "30%", "40%"),
                       breaks=c(10,20,30,40),
                       guide = guide_colorbar(ticks.linewidth = 4)) +
  theme_void() +
  theme(legend.position = "bottom",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  plot.margin = margin(2.5, 0.5, 1.25, 0.5, "cm"),
                  legend.key.width = unit(2.75, "cm"),
                  legend.key.height = unit(1.5, "cm"),
                  legend.text = element_text(size=75, face="bold", family=font2, color="#67000D"))

## Incarceration
Jail <- ggplot() + 
  geom_sf(data=Health, aes(fill=Jail_Incarceration, geometry=geometry), color="transparent") +
  coord_sf() +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Oranges")[3:9],
                       labels=comma,
                       guide = guide_colorbar(ticks.linewidth = 4)) +
  theme_void() +
  theme(legend.position = "bottom",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  plot.margin = margin(2.5, 0.5, 1.25, 0.5, "cm"),
                  legend.key.width = unit(2.75, "cm"),
                  legend.key.height = unit(1.5, "cm"),
                  legend.text = element_text(size=75, face="bold", family=font2, color="#7F2704"))

## Rent Burden
Rent <- ggplot() + 
  geom_sf(data=Health, aes(fill=Rent_Burden, geometry=geometry), color="transparent") +
  coord_sf() +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Greens")[3:9],
                       breaks=c(40, 45, 50, 55, 60, 65),
                       labels=c("40%", "45%", "50%", "55%", "60%", "65%"),
                       guide = guide_colorbar(ticks.linewidth = 4)) +
  theme_void() +
  theme(legend.position = "bottom",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  plot.margin = margin(2.5, 0.5, 1.25, 0.5, "cm"),
                  legend.key.width = unit(2.75, "cm"),
                  legend.key.height = unit(1.5, "cm"),
                  legend.text = element_text(size=75, face="bold", family=font2, color="#00441B"))

## Bodega Ration
Bodega <- ggplot() + 
  geom_sf(data=Health, aes(fill=Ratio_Bodega_Supermarket, geometry=geometry), color="transparent") +
  coord_sf() +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "RdPu")[3:9],
                       guide = guide_colorbar(ticks.linewidth = 4)) +
  theme_void() +
  theme(legend.position = "bottom",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  plot.margin = margin(2.5, 0.5, 1.25, 0.5, "cm"),
                  legend.key.width = unit(2.75, "cm"),
                  legend.key.height = unit(1.5, "cm"),
                  legend.text = element_text(size=75, face="bold", family=font2, color="#49006A"))

## Pollution
Pollution <- ggplot() + 
  geom_sf(data=Health, aes(fill=Air_Pollution, geometry=geometry), color="transparent") +
  coord_sf() +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Purples")[3:9],
                       guide = guide_colorbar(ticks.linewidth = 4)) +
  theme_void() +
  theme(legend.position = "bottom",
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  plot.margin = margin(2.5, 0.5, 1.25, 0.5, "cm"),
                  legend.key.width = unit(2.75, "cm"),
                  legend.key.height = unit(1.5, "cm"),
                  legend.text = element_text(size=75, face="bold", family=font2, color="#3F007D"))


# All Plots and Annotations
grid <- plot_grid(Poverty, Education, Rent, 
                  Bodega, Jail, Pollution, 
                  nrow=2, ncol=3) +
  theme(plot.margin= margin(8, 2, 1, 2, "cm"),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
        panel.background = element_rect(fill="#fdf9f5", color="#fdf9f5")) +
  draw_text(text="Twitter: @BlakeRobMills | Source: NYC Open Data | GitHub: BlakeRMills", x=0.5, y=0, size=80, color="#3f000a", fontface="bold", family=font1) +
  draw_text(text="Inequality in New York City", x=0.5, y=1.1, size=325, color="#3f000a", fontface="bold", family=font1) +
  draw_text(text="Different ways of mapping disparities in New York City by community district.", x=0.5, y=1.042, size=110, color="#3f000a", family=font2) +
  draw_text(text="Poverty", x=0.17, y=0.985, size=170, color="#08306B", fontface="bold", family=font1) +
  draw_text(text="Education", x=0.5, y=0.985, size=170, color="#67000D", fontface="bold", family=font1) +
  draw_text(text="Rent Burden", x=0.833, y=0.985, size=170, color="#00441B", fontface="bold", family=font1) +
  draw_text(text="Pollution", x=0.833, y=0.48, size=170, color="#3F007D", fontface="bold", family=font1) +
  draw_text(text="Incarceration", x=0.5, y=0.48, size=170, color="#7F2704", fontface="bold", family=font1) +
  draw_text(text="Bodega-Supermarket Ratio", x=0.17, y=0.47, size=170, color="#49006A", fontface="bold", family=font1) +
  draw_text(text="% Living Below Poverty Line", x=0.17, y=0.615, size=90, color="#08306B", fontface="bold", family=font2) +
  draw_text(text="% Adults w/o High School Degree", x=0.5, y=0.615, size=90, color="#67000D", fontface="bold", family=font2) +
  draw_text(text="% Households Rent Burdened", x=0.833, y=0.615, size=90, color="#00441B", fontface="bold", family=font2) +
  draw_text(text="Bodegas per Supermarket", x=0.17, y=0.115, size=90, color="#49006A", fontface="bold", family=font2) +
  draw_text(text="Residents per 100k Incarcerated", x=0.5, y=0.115, size=90, color="#7F2704", fontface="bold", family=font2) +
  draw_text(text="Mcg of Fine Particulates per m3", x=0.833, y=0.115, size=90, color="#3F007D", fontface="bold", family=font2) +
  draw_text(text="Rent Burden represents\nhouseholds paying 30%\nor more of income on rent\nin the past 12 months", x=0.75, y=0.85, size=85, color="#006D2C", lineheight=0.4, family=font2) + 
  draw_text(text="Number of bodegas for\neach supermarket is used as\na proxy for access to\nfresh foods in an area", x=0.085, y=0.35, size=85, color="#7A0177", lineheight=0.4, family=font2) +
  draw_line(x=c(-2, 0.22), y=c(1.1, 1.1), color="#3f000a", size=3.5) +
  draw_line(x=c(0.78, 1.2), y=c(1.1, 1.1), color="#3f000a", size=3.5)


# Save
ggsave("~/Desktop/Day 22 (Boundaries)/NYC Disparities.png", width = 35, height = 25)

