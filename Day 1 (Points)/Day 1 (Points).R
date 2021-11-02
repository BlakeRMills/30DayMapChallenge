library(readr)
library(geojsonio)
library(tidyverse)
library(sf)
library(showtext)
library(sysfonts)
library(cowplot)

#Data
pluto <- read_csv("~/Desktop/pluto_21v3.csv")
borMap <- geojson_sf("~/Desktop/Borough Boundaries.geojson")

#aes
showtext_auto()
font_add_google("Josefin Sans")
font1 <- "Josefin Sans"
labelx <- 0.71
labelfontsize <- 68


#Data Wrangling

borMap <- borMap %>% filter(boro_name == "Manhattan")

pluto <- pluto %>% mutate(BuildingType = case_when(grepl("J", bldgclass)==TRUE ~ "Theatre",
                                                   bldgclass == "P7" ~ "Museum",
                                                   bldgclass == "P8" ~ "Library",
                                                   bldgclass == "Q0" ~ "Open Space",
                                                   bldgclass == "Q1" ~ "Park",
                                                   bldgclass == "Q2" ~ "Playground",
                                                   bldgclass == "QG" ~ "Commuity Garden",
                                                   TRUE ~ "Other")) %>%
  filter(borough == "MN", BuildingType != "Other")


#Main Plot
p1 <- ggplot() + 
  geom_sf(data=borMap, aes(geometry=geometry), color="grey15", fill="grey35") +
  geom_point(data=pluto, aes(x=longitude, y=latitude, color=BuildingType), alpha=0.75, size=2.75) +
  scale_color_manual(values = c("#ffe287", "#ffb47c", "#f58e7d", "#c87e96", "#9d7fae", "#3c8eda", "#1d5ae3")) +
  theme_void() +
  theme(plot.background = element_rect(color="grey15", fill="grey15"),
        panel.background = element_rect(color="grey15", fill="grey15"),
        plot.margin=margin(1.5,7,0.5,0, unit="cm"),
        legend.position = "none")

#Annotate
ggdraw(p1) +
  draw_label(label="Community Garden", color= "#ffe287", x=labelx, y=0.71, size=labelfontsize, fontfamily=font1, fontface = "bold") +
  draw_label(label="Library", color= "#ffb47c", x=labelx, y=0.64, size=labelfontsize, fontfamily=font1, fontface = "bold") +
  draw_label(label="Museum", color= "#f58e7d", x=labelx, y=0.57, size=labelfontsize, fontfamily=font1, fontface = "bold") +
  draw_label(label="Open Space", color= "#c87e96", x=labelx, y=0.5, size=labelfontsize, fontfamily=font1, fontface = "bold") +
  draw_label(label="Park", color= "#9d7fae", x=labelx, y=0.43, size=labelfontsize, fontfamily=font1, fontface = "bold") +
  draw_label(label="Playground", color= "#3c8eda", x=labelx, y=0.36, size=labelfontsize, fontfamily=font1, fontface = "bold") +
  draw_label(label="Theatre", color= "#1d5ae3", x=labelx, y=0.29, size=labelfontsize, fontfamily=font1, fontface = "bold") +
  draw_label(label="Community Spaces in Manhattan", color= "grey90", x=0.5, y=0.95, size=100, fontfamily=font1, fontface = "bold") +
  draw_label(label="Twitter: @BlakeRobMills | Source: NYC Open Data | GitHub: BlakeRMills", color= "grey85", x=0.5, y=0.02, size=35, fontface = "bold", fontfamily = font1)

#Save
ggsave("~/Desktop/Manhattan Spaces (Day 1 Points).png", width = 10, height = 10)

