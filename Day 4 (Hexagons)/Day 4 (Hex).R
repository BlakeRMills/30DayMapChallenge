# Libraries
library(readr)
library(plyr)
library(tidyverse)
library(scales)
library(cowplot)
library(tilegramsR)
library(sf)
library(showtext)
library(sysfonts)
library(stringr)

# Aes
showtext_auto()
font_add_google("Comfortaa")
font_add_google("Big Shoulders Display")
font1 <- "Big Shoulders Display"
font2 <- "Comfortaa"

# Data
Bees <- read_csv("~/Desktop/Day 4 (Hexagons)/Bees 87-20.csv")

# Wrangling
Bees <- Bees %>% filter(Period=="MARKETING YEAR"| is.na(Period)==TRUE) %>% # The columns I added have NAs, the whole years are labeled "MARKETING YEAR"
  select(Year, State, Value) %>%
  rbind(c(1987, "ALASKA", NA)) %>%
  complete(Year, State) %>% 
  mutate(State = str_to_title(State))

Bees <- Bees %>% left_join(., (Bees %>% filter(Year==1990) %>% select(-Year) %>% 
                                 dplyr::rename(c("1990"="Value"))),  by="State") %>%
  mutate(Percent1990 = as.numeric(Value)/as.numeric(`1990`),
         Percent1990 = round((Percent1990-1)*100, 2),
         Label = ifelse(is.na(Value)==TRUE, NA, comma(as.numeric(Value))))

# This uses the tilegramR packages to get and wrangle plots - Main Hexes
Hex <- sf_NPR1to1 %>% left_join(., data.frame(state=state.abb, name=state.name), by="state") %>%
  filter(state != "DC")
st_crs(Hex) <- st_crs(Hex) #Fixes warning with old-style crs object

# This uses the tilegramR packages to get and wrangle plots - Hex Centers
Centers <- sf_NPR1to1.centers %>% filter(state != "DC")
st_crs(Centers) <- st_crs(Centers)

# Final join
Bees <- Bees %>% left_join(., Hex, by=c("State" = "name"))  %>%
  left_join(., Centers, by="state") %>%
  dplyr::rename(c("GeomHex"="geometry.x", "GeomCenter"="geometry.y")) %>%
  filter(Year %in% c(1990, 2000, 2010, 2020))

#Year Frames
Bees1990 <- Bees %>% filter(Year==1990)
Bees2000 <- Bees %>% filter(Year==2000)
Bees2010 <- Bees %>% filter(Year==2010)
Bees2020 <- Bees %>% filter(Year==2020)

#Clean up
rm(Hex, Centers, Bees)


#Individual Plots
a <- ggplot(data=Bees1990) +
  geom_sf(aes(geometry=GeomHex, fill=Percent1990), size=1.4, color="#fdf7d8") +  # Main Hexes
  geom_sf_text(data=Bees1990 %>% filter(Percent1990 <= -50 | Percent1990 >= 50), aes(geometry=GeomCenter, label=state), 
               fontface="bold", family=font1, size=20, vjust=-0.15, color="grey95") + # State Label when hex is dark
  geom_sf_text(data=Bees1990 %>% filter(Percent1990 > -50 & Percent1990 < 50), aes(geometry=GeomCenter, label=state), 
               fontface="bold", family=font1, size=20, vjust=-0.15, color="grey15") +# State Label when hex is light
  geom_sf_text(data=Bees1990 %>% filter(is.na(Percent1990)==TRUE), aes(geometry=GeomCenter, label=state), 
               fontface="bold", family=font1, size=20, vjust=-0.15, color="grey15") + # State Label when value is NA
  geom_sf_text(data=Bees1990 %>% filter(Percent1990 <= -50 | Percent1990 >= 50), aes(geometry=GeomCenter, label=Label),
               family=font1, size=20, vjust=1.1, color="grey90") + # Colony Population when hex is dark
  geom_sf_text(data=Bees1990 %>% filter(Percent1990 > -50 & Percent1990 < 50), aes(geometry=GeomCenter, label=Label),
               family=font1, size=20, vjust=1.1, color="grey20") + # Colony Population when hex is light
  scale_fill_gradientn(breaks = c(-90, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100, 120, 140),
                       colors = c("#664817", "#a5782b", "#bc9333", "#d3ac3d", "#f3d651", "#ffee94",
                                  "#b2c2c4", "#6d8a8d", "#4a6c76", "#2d4d5f", "#1f3b4b", "#1a3240", "#142732"),
                       na.value = "#9a9b92",
                       limits = c(-90, 140)) +
  theme_void() +
  theme(plot.margin = margin(1.5, 0, 0, 0, unit="cm"),
        legend.key.width  = unit(4, "cm"),
        legend.key.height  = unit(1.75, "cm"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        legend.position = c(1,1.175),
        legend.direction = "horizontal") +
  guides(fill=guide_colorbar(ticks = FALSE))

b <- ggplot(data=Bees2000) +
  geom_sf(aes(geometry=GeomHex, fill=Percent1990), size=1.4, color="#fdf7d8") +  # Main Hexes
  geom_sf_text(data=Bees2000 %>% filter(Percent1990 <= -50 | Percent1990 >= 50), aes(geometry=GeomCenter, label=state), 
               fontface="bold", family=font1, size=20, vjust=-0.15, color="grey95") + # State Label when hex is dark
  geom_sf_text(data=Bees2000 %>% filter(Percent1990 > -50 & Percent1990 < 50), aes(geometry=GeomCenter, label=state), 
               fontface="bold", family=font1, size=20, vjust=-0.15, color="grey15") +# State Label when hex is light
  geom_sf_text(data=Bees2000 %>% filter(is.na(Percent1990)==TRUE), aes(geometry=GeomCenter, label=state), 
               fontface="bold", family=font1, size=20, vjust=-0.15, color="grey15") + # State Label when value is NA
  geom_sf_text(data=Bees2000 %>% filter(Percent1990 <= -50 | Percent1990 >= 50), aes(geometry=GeomCenter, label=Label),
               family=font1, size=20, vjust=1.1, color="grey90") + # Colony Population when hex is dark
  geom_sf_text(data=Bees2000 %>% filter(Percent1990 > -50 & Percent1990 < 50), aes(geometry=GeomCenter, label=Label),
               family=font1, size=20, vjust=1.1, color="grey20") + # Colony Population when hex is light
  scale_fill_gradientn(breaks = c(-90, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100, 120, 140),
                       colors = c("#664817", "#a5782b", "#bc9333", "#d3ac3d", "#f3d651", "#ffee94",
                                  "#b2c2c4", "#6d8a8d", "#4a6c76", "#2d4d5f", "#1f3b4b", "#1a3240", "#142732"),
                       na.value = "#9a9b92",
                       limits = c(-90, 140)) +
  theme_void() +
  theme(plot.margin = margin(1.5, 0, 0, 0, unit="cm"),
        legend.position = "none")

c <- ggplot(data=Bees2010) +
  geom_sf(aes(geometry=GeomHex, fill=Percent1990), size=1.4, color="#fdf7d8") +  # Main Hexes
  geom_sf_text(data=Bees2010 %>% filter(Percent1990 <= -50 | Percent1990 >= 50), aes(geometry=GeomCenter, label=state), 
               fontface="bold", family=font1, size=20, vjust=-0.15, color="grey95") + # State Label when hex is dark
  geom_sf_text(data=Bees2010 %>% filter(Percent1990 > -50 & Percent1990 < 50), aes(geometry=GeomCenter, label=state), 
               fontface="bold", family=font1, size=20, vjust=-0.15, color="grey15") +# State Label when hex is light
  geom_sf_text(data=Bees2010 %>% filter(is.na(Percent1990)==TRUE), aes(geometry=GeomCenter, label=state), 
               fontface="bold", family=font1, size=20, vjust=-0.15, color="grey15") + # State Label when value is NA
  geom_sf_text(data=Bees2010 %>% filter(Percent1990 <= -50 | Percent1990 >= 50), aes(geometry=GeomCenter, label=Label),
               family=font1, size=20, vjust=1.1, color="grey90") + # Colony Population when hex is dark
  geom_sf_text(data=Bees2010 %>% filter(Percent1990 > -50 & Percent1990 < 50), aes(geometry=GeomCenter, label=Label),
               family=font1, size=20, vjust=1.1, color="grey20") + # Colony Population when hex is light
  scale_fill_gradientn(breaks = c(-90, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100, 120, 140),
                       colors = c("#664817", "#a5782b", "#bc9333", "#d3ac3d", "#f3d651", "#ffee94",
                                  "#b2c2c4", "#6d8a8d", "#4a6c76", "#2d4d5f", "#1f3b4b", "#1a3240", "#142732"),
                       na.value = "#9a9b92",
                       limits = c(-90, 140)) +
  theme_void() +
  theme(plot.margin = margin(1.5, 0, 0, 0, unit="cm"),
        legend.position = "none")

d <- ggplot(data=Bees2020) +
  geom_sf(aes(geometry=GeomHex, fill=Percent1990), size=1.4, color="#fdf7d8") +  # Main Hexes
  geom_sf_text(data=Bees2020 %>% filter(Percent1990 <= -50 | Percent1990 >= 50), aes(geometry=GeomCenter, label=state), 
               fontface="bold", family=font1, size=20, vjust=-0.15, color="grey95") + # State Label when hex is dark
  geom_sf_text(data=Bees2020 %>% filter(Percent1990 > -50 & Percent1990 < 50), aes(geometry=GeomCenter, label=state), 
               fontface="bold", family=font1, size=20, vjust=-0.15, color="grey15") +# State Label when hex is light
  geom_sf_text(data=Bees2020 %>% filter(is.na(Percent1990)==TRUE), aes(geometry=GeomCenter, label=state), 
               fontface="bold", family=font1, size=20, vjust=-0.15, color="grey15") + # State Label when value is NA
  geom_sf_text(data=Bees2020 %>% filter(Percent1990 <= -50 | Percent1990 >= 50), aes(geometry=GeomCenter, label=Label),
               family=font1, size=20, vjust=1.1, color="grey90") + # Colony Population when hex is dark
  geom_sf_text(data=Bees2020 %>% filter(Percent1990 > -50 & Percent1990 < 50), aes(geometry=GeomCenter, label=Label),
               family=font1, size=20, vjust=1.1, color="grey20") + # Colony Population when hex is light
  scale_fill_gradientn(breaks = c(-90, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100, 120, 140),
                       colors = c("#664817", "#a5782b", "#bc9333", "#d3ac3d", "#f3d651", "#ffee94",
                                  "#b2c2c4", "#6d8a8d", "#4a6c76", "#2d4d5f", "#1f3b4b", "#1a3240", "#142732"),
                       na.value = "#9a9b92",
                       limits = c(-90, 140)) +
  theme_void() +
  theme(plot.margin = margin(1.5, 0, 0, 0, unit="cm"),
        legend.position = "none")

# Main Grid
Grid <- plot_grid(a, b, c, d, ncol=2, nrow = 2) +
  theme(plot.background = element_rect(fill="#fdf8e2", color="#fdf8e2"),
        plot.margin= margin(9, 0, 4, 0, unit="cm")) +
  annotate(geom="curve", x=0.26, y= 0.86, xend=0.295, yend = 0.798, curvature= 0.35, size = 1.75, arrow = arrow(length = unit(2, "mm")), color="grey15")

# Annotations
ggdraw(Grid) +
  draw_label(x=0.5, y=0.965, label="The Rise and Fall and Bee Colonies", fontfamily=font2, fontface = "bold", size=225, color="#743700") +
  draw_line(x=c(0, 0.13), y=c(0.965, 0.965), color="#743700", size=3) +
  draw_line(x=c(0.875, 1), y=c(0.96, 0.96), color="#743700", size=3) +
  draw_label(x=0.5, y=0.92, label="Maps show percentage change in number of estimated bee colonies compared to 1990", fontfamily=font2, size=95, color="#743700") +
  draw_label(x=0.5, y=0.015, label= "Twitter: @BlakeRobMills | Source: USDA | GitHub: BlakeRMills", size=75, fontfamily=font2, fontface = "bold", color="#743700") +
  draw_line(x=c(0, 0.28), y=c(0.015, 0.015), color="#743700", size=1.25) +
  draw_line(x=c(0.72, 1), y=c(0.015, 0.015), color="#743700", size=1.25) +
  draw_label(x=0.27, y=0.79, label= "1990", size=175, fontfamily=font2, fontface = "bold", color="#1f3b4b") +
  draw_label(x=0.77, y=0.79, label= "2000", size=175, fontfamily=font2, fontface = "bold", color="#1f3b4b") +
  draw_label(x=0.27, y=0.38, label= "2010", size=175, fontfamily=font2, fontface = "bold", color="#1f3b4b") +
  draw_label(x=0.77, y=0.378, label= "2020", size=175, fontfamily=font2, fontface = "bold", color="#1f3b4b") +
  draw_label(x=0.345, y=0.885, label= "-80%", size=75, fontfamily=font1, fontface = "bold", color="#a5782b") +
  draw_label(x=0.400, y=0.885, label= "-40%", size=75, fontfamily=font1, fontface = "bold", color="#bc9333") +
  draw_label(x=0.455, y=0.885, label= "0%", size=75, fontfamily=font1, fontface = "bold", color="#f3d446") +
  draw_label(x=0.510, y=0.885, label= "+40%", size=75, fontfamily=font1, fontface = "bold", color="#6d8a8d") +
  draw_label(x=0.565, y=0.885, label= "+80%", size=75, fontfamily=font1, fontface = "bold", color="#2d4d5f") +
  draw_label(x=0.620, y=0.885, label= "+120%", size=75, fontfamily=font1, fontface = "bold", color="#1a3240") +
  draw_label(x=0.267, y=0.735, label= "Estimated Total Bee\nColonies in the State", size=42, fontfamily=font2, fontface = "bold", color="grey15", lineheight = 0.39) 



#Save Plot
ggsave("~/Desktop/BeePlot.png", height = 20, width = 25)



