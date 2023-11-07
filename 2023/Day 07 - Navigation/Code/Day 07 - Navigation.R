# Libaries
library(cowplot)
library(ggrepel)
library(MetBrewer)
library(MoMAColors)
library(showtext)
library(sysfonts)
library(tidyverse)

# Aes
showtext_auto()
font <- "Advent Pro"
font_add_google(font)
pal <- moma.colors("Dali")
pal <- met.brewer("Cross")

# Functions
`%notin%` <- Negate(`%in%`)

# Data
cities_df <- data.frame(xmin = c(3.5, 5.5, 4.5, 4.5, 4.5, 2.5, 17.5, 1.5, 12.5, 21.5, 18.25, 0.5, 8.5, 8.25, 24.25, 27.5, 7.5, 16.5, 19.5, 0.5, 7, 1, 11, 16.5, 8.5, 6.5),
                        xmax = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  19.75, 0.5, 8.5, 9.75, 25.75, 27.5, rep(NA, 10)),
                        ymin = c(14.5, 11.5, 7.5, 4.5, 2.5, -0.5, 3.5, 4.5, 14.5, 6.5, 10.5, 7.25, 3.75, 7.5, 8.5, 4.25, 12.5, 11.5, 4.5, 6, 11, 12, 13, 9, 9, 0),
                        ymax = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 10.5, 8.75, 2.25, 7.5, 8.5, 5.75, rep(NA, 10)),
                        name = c("Fallarbor Town", "Lavaridge Town", "Verdanturf Town",
                                 "Oldale Town", "Littleroot Town", "Dewford Town",
                                 "Pacifidlog Town", "Petalburg City", "Fortree City",
                                 "Sootopolis City", "Lilycove City", "Rusboro City",
                                 "Slateport City", "Mauville City", "Mossdeep City", "Ever Grande City",
                                 "Mt. Chimney", "Safari Zone", "Sky Pillar", "Petalburg Woods", "Fiery Path",
                                 "Meteor Falls", "Weather Institute", "Mt. Pyre", "Desert Ruins", "Abandoned Ship"),
                        shape = c("Dot", "Dot", "Dot", "Dot", "Dot", "Dot",  "Dot", "Dot", "Dot", "Dot", "Line", "Line", "Line", "Line", "Line", "Line", rep(NA, 10)),
                        class = c("Town", "Town", "Town", "Town", "Town", "Town", "Town", "City", "City", "City", "City", "City", "City", "City", "City", "City",  rep("Landmark", 10))) %>%
  mutate(x_lab = ifelse(is.na(xmax)==T, xmin, (xmin + xmax)/2),
         y_lab = ifelse(is.na(xmax)==T, ymin, (ymin + ymax)/2),
         name = str_replace_all(name, " ", "\n"))

routes_df <- data.frame(xmin = c(0.5, 0.5, 1.5, 11.5, 11.5, 12.5, 13.5,
                                 13.5, 8.5, 11.5, 14.5, 3.5, 8.5, 5.5,
                                 1.5, 1.5, 0.5, 0.5, 8.5, 4.5, 4.5,
                                 4.5, 6.25, 6.75, 9.5, 11.5, 4.5, 0.5, 4.5,
                                 0.5, 0.5, 2.5, 2.5, 5.5, 8.5, 17.5, 
                                 20.5, 23.75, 24.75, 24.75, 24.75, 24.75, 23.75, 23.75,
                                 19.75, 20.5, 20.5, 20.5, 16.5, 16.5),
                        xmax = c(0.5, 1.5, 4.5, 11.5, 12.5, 13.5, 13.5,
                                 18.5, 11.5, 14.5, 17.5, 8.5, 8.5, 8.5,
                                 3.5, 1.5, 1.5, 0.5, 8.5, 4.5, 4.5,
                                 6.25, 6.75, 8.5, 11.5, 16.5, 8.5, 4.5, 4.5,
                                 0.5, 2.5, 2.5, 5.5, 8.5, 8.5, 20.5, 
                                 23.75, 25.5, 27.5, 25.5, 25.5, 25.5, 25.5, 25.5,
                                 23.75, 23.75, 23.75, 23.5, 16.5, 16.5),
                        ymin = c(0.5, 4.5, 4.5, 14.5, 14.5, 14.5, 10.5,
                                 10.5, 3.5, 3.5, 3.5, 14.5, 14.5, 11.5,
                                 14.5, 14.5, 11.5, 11.5, 7.5, 4.5, 4.5, 
                                 5.5, 5.5, 5.5, 7.5, 7.5, 7.5, 8.5, 8.5,
                                 7.5, 0.5, 0.5, -0.5, -0.5, -0.5, 3.5, 
                                 3.5, 3.5, 4.5, 5.5, 7.5, 6.5, 10.5, 9.5,
                                 10.5, 9.5, 8.6, 5.5, 10.5, 9.25),
                        ymax = c(4.5, 4.5, 4.5, 7.5, 14.5, 14.5, 14.5,
                                 10.5, 3.5, 3.5, 3.5, 14.5, 7.5, 11.5,
                                 14.5, 11.5, 11.5, 8.5, 3.5, 2.5, 5.5,
                                 5.5, 5.5, 5.5, 7.5, 7.5, 7.5, 8.5, 7.5,
                                 4.5, 0.5, -0.5, -0.5, -0.5, 2.5, 3.5, 
                                 3.5, 3.5, 4.5, 5.5, 7.5, 6.5, 10.5, 9.5,
                                 10.5, 9.5, 8.6, 5.5, 7.5, 8.75),
                        route = c("105", "105", "102", "119", "119", "120", "120",
                                  "121", "134", "133", "132", "113", "111", "112",
                                  "114", "114", "115", "115", "110", "101", "103",
                                  "103", "103", "103", "118", "123", "117", "116", "116",
                                  "104", "106", "106", "107", "108", "109", "131", 
                                  "130", "129", "128", "127", "127", "127", "125", "125",
                                  "124", "124", "124", "126", "122", "122"),
                        type = c("Water", "Water", "Land", "Land", "Land", "Land", "Land",
                                 "Land", "Water", "Water", "Water", "Land", "Land", "Land",
                                 "Land", "Land", "Land", "Land", "Land", "Land", "Land",
                                 "Land", "Water", "Land", "Land", "Land", "Land", "Land", "Land",
                                 "Land", "Water", "Water", "Water", "Water", "Water", "Water", 
                                 "Water", "Water", "Water", "Water", "Water", "Water", "Water", "Water",
                                 "Water", "Water", "Water", "Deep Water", "Land", "Water"))

route_labels <- routes_df %>%
  distinct(route, .keep_all = T) %>%
  mutate(x_lab = ifelse(is.na(xmax)==T, xmin, (xmin + xmax)/2),
         y_lab = ifelse(is.na(xmax)==T, ymin, (ymin + ymax)/2),
         route = paste("R.", route)) %>%
  mutate(x_lab = case_when(route == "R. 115" ~ x_lab - 0.5,
                           route == "R. 116" ~ x_lab + 0.5,
                           route == "R. 118" ~ x_lab + 0.5,
                           route == "R. 120" ~ x_lab + 0.5,
                           route == "R. 128" ~ x_lab - 1.25,
                           route == "R. 127" ~ x_lab - 0.25,
                           route == "R. 129" ~ x_lab + 0.25,
                           .default = x_lab),
         y_lab = case_when(route == "R. 115" ~ y_lab - 1.25,
                           route == "R. 103" ~ y_lab + 0.5,
                           route == "R. 120" ~ y_lab - 2,
                           route == "R. 124" ~ y_lab - 1,
                           .default = y_lab)) %>%
  filter(route %notin% c("R. 101", "R. 131", "R. 104"))

area_df <- data.frame(xmin = c(20, 24, 20, 23), 
                      xmax = c(26, 26, 24, 24),
                      ymin = c(8, 3.5, 5.5, 3.5),
                      ymax = c(10.5, 8, 8, 6),
                      type = c("Normal", "Normal", "Deep", "Deep"))

# Palettes 
## Water, City, Deep Water, Land, Landmark, Town
Hiroshige_Pal <- met.brewer("Hiroshige")[c(6, 10, 4, 3, 9, 8)]
Liu_Pal <- moma.colors("Liu")[c(1, 2, 5, 4, 3, 6)]
Signac_Pal <- met.brewer("Signac")[c(10, 11, 2, 14, 3, 13)]
Benedictus_Pal <- met.brewer("Benedictus")[c(12, 4, 13, 9, 5, 1)]

pal <- Benedictus_Pal

# Plot
plot <- ggplot() +
  geom_rect(data=area_df %>% filter(type=="Normal"), aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax), fill=pal[1]) +
  geom_rect(data=area_df %>% filter(type=="Deep"), aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax), fill=pal[3]) +
  geom_segment(data=routes_df %>% filter(type == "Land"), aes(x=xmin, y=ymin, xend=xmax, yend=ymax), linewidth=9, color=pal[4],
               lineend = "round") +
  geom_segment(data=routes_df %>% filter(type == "Deep Water"), aes(x=xmin, y=ymin, xend=xmax, yend=ymax), linewidth=9, color=pal[3],
               lineend = "round") +
  geom_segment(data=routes_df %>% filter(type == "Water"), aes(x=xmin, y=ymin, xend=xmax, yend=ymax), linewidth=9, color=pal[1],
               lineend = "round") +
  geom_point(data=cities_df %>% filter(shape=="Dot", class=="Town"), aes(x=xmin, y=ymin), size=7, color=pal[6]) +
  geom_point(data=cities_df %>% filter(shape=="Dot", class=="City"), aes(x=xmin, y=ymin), size=7, color=pal[2]) +
  geom_segment(data=cities_df %>% filter(shape=="Line", class=="City"), aes(x=xmin, y=ymin, xend=xmax, yend=ymax), linewidth=9, color=pal[2],
               lineend = "round") +
  geom_point(data=cities_df %>% filter(class=="Landmark"), aes(x_lab, y_lab), size=4, color = pal[5]) +
  geom_label_repel(data = cities_df, aes(x=x_lab, y=y_lab, label=name, size=class, color=class), family=font, fontface="bold", lineheight=0.3,
                   fill="#fffbf2", label.padding = unit(0.02, "cm"), label.size=0, alpha=0.9, seed=16) +
  geom_text(data=route_labels, aes(x=x_lab, y=y_lab, label=route), color="white", family=font, fontface="bold", size=7) + 
  scale_color_manual(values = c(pal[2], pal[5], pal[6])) +
  scale_size_manual(values = c(10, 7, 10)) +
  scale_x_continuous(limits = c(-1, 29), expand = c(0,0)) +
  scale_y_continuous(limits = c(-5.5, 24.5), expand = c(0,0)) +
  theme_void() +
  theme(legend.position = "none")

ggdraw(plot) + 
  theme(plot.background = element_rect(fill="#fffbf2", color="#fffbf2")) +
  draw_label(label="Hoenn", x=0.925, y=0.875, hjust=1, size=350, fontface = "bold", fontfamily = font, color=pal[2]) +
  draw_label(label="Twitter: @BlakeRobMills | GitHub: BlakeRMills", x=0.5, y=0.0175, size=35, fontface = "bold", fontfamily = font, color=pal[2]) 


ggsave("~/30DayMapChallenge/2023/Day 07 - Navigation/Day 07 - Naviagtion_Benedictus.png", width = 8, height = 7)
ggsave("~/Desktop/Day 07 - Naviagtion_Benedictus.png", width = 8, height = 7)
