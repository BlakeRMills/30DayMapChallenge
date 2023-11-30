# Libraries
library(cowplot)
library(geojsonio)
library(ggtext)
library(MoMAColors)
library(readxl)
library(sf)
library(showtext)
library(sysfonts)
library(tidyverse)

# Aes
showtext_auto()
font <- "Advent Pro"
font_add_google(font)
sf_use_s2(FALSE)
pal <- met.brewer("Tam")[c(7, 2)]

# Functions 
generate_points <- function(num_points) {
  angles <- seq(0, 2 * pi, length.out = num_points + 1)[-1]  # Calculate angles
  angles <- angles - angles[1]
  
  y <- 0.5 + 0.395 * cos(angles)
  x <- 0.5 + 0.395 * sin(angles)
  
  return(data.frame(x, y))
}

# Data 
dist <- geojson_sf("~/30DayMapChallenge/2023/Day 28 - Is this a chart of a map?/Data/Community Districts.geojson")
prof <- read_xlsx("~/Desktop/2018-chp-pud.xlsx") %>%
  mutate(ID = as.character(ID))

# Cleaning
data <- left_join(dist, prof, by = c("boro_cd" = "ID")) %>%
  filter(Borough == "Manhattan") %>%
  mutate(name_clean = str_replace_all(Name, "and", "&"),
         name_clean = case_when(name_clean == "Washington Heights & Inwood" ~ "Washington Heights &\nInwood",
                                name_clean == "Morningside Heights & Hamilton Heights" ~ "Morningside Heights &\nHamilton Heights",
                                name_clean == "Stuyvesant Town & Turtle Bay" ~ "Stuyvesant Town &\nTurtle Bay",
                                name_clean == "Greenwich Village & Soho" ~ "Greenwich Village &\nSoho",
                                name_clean == "Lower East Side & Chinatown" ~ "Lower East Side &\nChinatown",
                                .default = name_clean))

# Map 
mar <- 1.3
for(i in data$boro_cd){
  
  n <- data %>% filter(boro_cd==i) %>% .$Ratio_Bodega_Supermarket 
  points <- generate_points(n + 1)
  cols <- c(pal[1], rep(pal[2], n))
  name <- data %>% filter(boro_cd==i) %>% .$name_clean 
  
p <- ggplot() + 
  geom_sf(data=data %>% filter(boro_cd==i), aes(fill=Ratio_Bodega_Supermarket), color="transparent") + 
  scale_fill_met_c("Tam", limits = c(0, 18)) + 
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(mar, mar, mar, mar, "cm")) 

d <- ggdraw(p) +
  draw_text(text=name, x=0.5,y=1.1, size=55, family=font, fontface="bold", lineheight=0.3) + 
  annotate(geom="point", x=points$x, y=points$y, size=6, color=cols) +
  theme(plot.margin = margin(1.4, 0.5, 0, 0.5, "cm"))

assign(paste("D", i, sep=""), d)
}

total_map <- ggplot() + 
  geom_sf(data=data, aes(fill=Ratio_Bodega_Supermarket), color="#fdf9f5", size=2) + 
  scale_fill_met_c("Tam", limits = c(1, 18)) + 
  theme_void() +
  theme(legend.position = c(0.25, 0.7),
        legend.key.height = unit(2.25, "cm"),
        legend.key.width = unit(1.75, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(family=font, face="bold", size=75, margin=margin(0, 0, 0, -2.4, "cm"), hjust=0.5, color="white"))

grid_1 <- plot_grid(D101, D102, D103, D104, D105, D106, D107, D108, D109, D110, D111, D112,
          nrow=4, ncol=3) + 
  theme(plot.background = element_rect(fill="transparent", color="transparent"))

grid_2 <- plot_grid(grid_1, total_map,
          ncol=2, rel_widths = c(0.8, 0.7))  +
  theme(plot.margin = margin(5, 0, 1, 0, "cm"))

ggdraw(grid_2) +
  theme(plot.background = element_rect(fill="#fdf9f5", color="transparent")) +
  draw_text(text="Bodegas per Supermarket in Manhattan", x=0.025, y=0.95, family=font, fontface="bold", size=190, hjust=0) +
  draw_text(text="Plot/Map displays the number of              for each                    by Manhattan Community District.",
            hjust=0, x=0.025, y=0.895, size=60, family=font) + 
  draw_text(text="bodegas", x=0.27, y=0.895, size=60, family=font, fontface="bold", color=pal[2]) + 
  draw_text(text="supermarket", x=0.401, y=0.895, size=60, family=font, fontface="bold", color=pal[1]) +
  draw_text(text="Bodegas per Supermarket", x=0.625, y=0.62, size=80, family=font, fontface="bold", color="black", angle=90) +
  draw_text(text="Central Park", x=0.8, y=0.45, size=55, family=font, color="black", angle=61) +
  draw_label(label="Twitter: @BlakeRobMills | Source: NYC 2018 Community Health Profiles | GitHub: BlakeRMills", color="black",  x=0.5, y=0.015, size=50, 
             fontface = "bold", fontfamily = font) 

ggsave("~/30DayMapChallenge/2023/Day 28 - Is this a chart or a map?/Is this a chart or a map?.png", 
       height = 14, width=16)
