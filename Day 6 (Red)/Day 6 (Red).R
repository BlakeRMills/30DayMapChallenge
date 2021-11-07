#Libraries
library(geojsonio)
library(sf)
library(tidyverse)
library(readxl)
library(cowplot)
library(sysfonts)
library(showtext)
library(ggblur)

# Load Data
redlines <- geojson_sf("~/Desktop/Day 6 (Red)/Data/RedlineMaps.geojson")
police <- read_xlsx("~/Desktop/Day 6 (Red)/Data/PoliceViolence.xlsx")

# Wrangling

## Redline Dfs
Baltimore <- redlines %>% filter(city == "Baltimore")
Boston <- redlines %>% filter(city == "Boston")
Chicago <- redlines %>% filter(city == "Chicago")
Detroit <- redlines %>% filter(city == "Detroit")
Indianapolis <- redlines %>% filter(city == "Indianapolis")
LosAngeles <- redlines %>% filter(city == "Los Angeles")
NewYork <- redlines %>% filter(city %in% c("Manhattan", "Brooklyn", "Queens", "Staten Island", "Bronx")) %>%
  mutate(holc_grade = ifelse(holc_grade=="E", "D", holc_grade))
Philadelphia <- redlines %>% filter(city == "Philadelphia")
StLouis <- redlines %>% filter(city == "St. Louis")

## Police Killings Df
BaltimoreViolence <-  police %>% filter(City=="Baltimore")
BostonViolence <-  police %>% filter(City=="Boston", State=="MA")
ChicagoViolence <-  police %>% filter(City=="Chicago")
DetroitViolence <-  police %>% filter(City=="Detroit", State=="MI")
IndianapolisViolence <-  police %>% filter(City=="Indianapolis")
LAViolence <-  police %>% filter(City=="Los Angeles")
NewYorkViolence <-  police %>% filter(City=="New York")
PhiladelphiaViolence <-  police %>% filter(City=="Philadelphia")
StLouisViolence <-  police %>% filter(City=="St. Louis")

#Aes
showtext_auto()
font_add_google("Fira Sans")
font_add_google("Source Sans Pro")
font1 <- "Fira Sans"
font2 <- "Source Sans Pro"

# Functions
plotter <- function(df1, df2){
  ggplot() +
    geom_sf(data=df1, aes(fill=holc_grade), color="transparent", alpha=0.75) + 
    geom_point(data=df2, aes(y=Latitude, x=Longitude), size=7.25, color="#3f000a", alpha=0.75) +
    scale_fill_manual(values = c("#619370", "#5ba2b6", "#cdcb78", "#b47280")) +
    theme_void()  +
    theme(legend.position = "none",
          plot.title = element_text(size=285, hjust=0.5, vjust=1.5, face="bold", color="#3f000a", family=font1),
          plot.margin = margin(4, 0.25, 0.5, 0.25, unit = "cm")) 
}


# Individual Plots
## LA
LA <- ggplot() +
  geom_sf(data=LosAngeles, aes(fill=holc_grade), color="transparent", alpha=0.75) + 
  geom_point(data=LAViolence, aes(y=Latitude, x=Longitude), size=4, color="#3f000a", alpha=0.75) +
  scale_fill_manual(values = c("#619370", "#5ba2b6", "#cdcb78", "#b47280")) +
  theme_void()  + theme(legend.position = "bottom",
        legend.key.width = unit(5, "cm"),
        legend.key.height = unit(0.8, "cm"),
        legend.title = element_blank(),
        legend.text = element_blank(), 
        plot.margin = margin(4,  0.25, 1.3, 0.25, unit = "cm"),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
        panel.background = element_rect(fill="#fdf9f5", color="#fdf9f5")) 

ggdraw(LA) +
  draw_text(text="A Grade", x=0.238, y=0.0725, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="B Grade", x=0.413, y=0.0725, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="C Grade", x=0.588, y=0.0725, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="D Grade", x=0.763, y=0.0725, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="HOLC Risk Grade:", x=0.5, y=0.104, size=70, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="Twitter: @BlakeRobMills | Source: Mapping Inequality & MappingPoliceViolence.org | GitHub: BlakeRMills", x=0.5, y=0.035, size=36, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="Police Killings of Citizens (2013-2021) plotted over HOLC's Redlining Map", x=0.5, y=0.865, size=50, family=font2, color="#3f000a") +
  draw_text(text="Los Angeles", x=0.5, y=0.93, size=250, family=font1, color="#3f000a", fontface="bold") 
  
ggsave("~/Desktop/Day 6 (Red)/Individual Plots/Los Angeles.png", width = 12, height = 12)

## NYC
NYC <- ggplot() +
  geom_sf(data=NewYork, aes(fill=holc_grade), color="transparent", alpha=0.75) + 
  geom_point(data=NewYorkViolence, aes(y=Latitude, x=Longitude), size=4, color="#3f000a", alpha=0.75) +
  scale_fill_manual(values = c("#619370", "#5ba2b6", "#cdcb78", "#b47280")) +
  theme_void()  + theme(legend.position = "bottom",
                        legend.key.width = unit(5, "cm"),
                        legend.key.height = unit(0.8, "cm"),
                        legend.title = element_blank(),
                        legend.text = element_blank(), 
                        plot.margin = margin(4,  0.25, 1.3, 0.25, unit = "cm"),
                        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
                        panel.background = element_rect(fill="#fdf9f5", color="#fdf9f5")) 

ggdraw(NYC) +
  draw_text(text="A Grade", x=0.238, y=0.055, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="B Grade", x=0.413, y=0.055, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="C Grade", x=0.588, y=0.055, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="D Grade", x=0.763, y=0.055, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="HOLC Risk Grade:", x=0.5, y=0.09, size=70, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="Twitter: @BlakeRobMills | Source: Mapping Inequality & MappingPoliceViolence.org | GitHub: BlakeRMills", x=0.5, y=0.017, size=36, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="Police Killings of Citizens (2013-2021) plotted over HOLC's Redlining Map", x=0.5, y=0.88, size=50, family=font2, color="#3f000a") +
  draw_text(text="New York", x=0.5, y=0.935, size=250, family=font1, color="#3f000a", fontface="bold") 

ggsave("~/Desktop/Day 6 (Red)/Individual Plots/New York City.png", width = 12, height = 12)

## Chicago
ChicagoP <- ggplot() +
  geom_sf(data=Chicago, aes(fill=holc_grade), color="transparent", alpha=0.75) + 
  geom_point(data=ChicagoViolence, aes(y=Latitude, x=Longitude), size=4, color="#3f000a", alpha=0.75) +
  scale_fill_manual(values = c("#619370", "#5ba2b6", "#cdcb78", "#b47280")) +
  theme_void()  + theme(legend.position = c(0.5, -0.01),
                        legend.direction = "horizontal",
                        legend.key.width = unit(5, "cm"),
                        legend.key.height = unit(0.8, "cm"),
                        legend.title = element_blank(),
                        legend.text = element_blank(), 
                        plot.margin = margin(3, 8, 1.8, 8, unit = "cm"),
                        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
                        panel.background = element_rect(fill="#fdf9f5", color="#fdf9f5")) 

ggdraw(ChicagoP) +
  draw_text(text="A Grade", x=0.238, y=0.05, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="B Grade", x=0.413, y=0.05, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="C Grade", x=0.588, y=0.05, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="D Grade", x=0.763, y=0.05, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="HOLC Risk Grade:", x=0.5, y=0.077, size=55, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="Twitter: @BlakeRobMills | Source: Mapping Inequality & MappingPoliceViolence.org | GitHub: BlakeRMills", x=0.5, y=0.015, size=36, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="Police Killings of Citizens (2013-2021) plotted over HOLC's Redlining Map", x=0.5, y=0.884, size=50, family=font2, color="#3f000a") +
  draw_text(text="Chicago", x=0.5, y=0.95, size=250, family=font1, color="#3f000a", fontface="bold") 

ggsave("~/Desktop/Day 6 (Red)/Individual Plots/Chicago.png", width = 12, height = 12)


## Baltimore
Balt <- ggplot() +
  geom_sf(data=Baltimore, aes(fill=holc_grade), color="transparent", alpha=0.75) + 
  geom_point(data=BaltimoreViolence, aes(y=Latitude, x=Longitude), size=4, color="#3f000a", alpha=0.75) +
  scale_fill_manual(values = c("#619370", "#5ba2b6", "#cdcb78", "#b47280")) +
  theme_void()  + theme(legend.position = c(0.5, -0.01),
                        legend.direction = "horizontal",
                        legend.key.width = unit(5, "cm"),
                        legend.key.height = unit(0.8, "cm"),
                        legend.title = element_blank(),
                        legend.text = element_blank(), 
                        plot.margin = margin(4, 3, 1.7, 3, unit = "cm"),
                        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
                        panel.background = element_rect(fill="#fdf9f5", color="#fdf9f5")) 

ggdraw(Balt) +
  draw_text(text="A Grade", x=0.238, y=0.047, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="B Grade", x=0.413, y=0.047, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="C Grade", x=0.588, y=0.047, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="D Grade", x=0.763, y=0.047, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="HOLC Risk Grade:", x=0.5, y=0.074, size=55, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="Twitter: @BlakeRobMills | Source: Mapping Inequality & MappingPoliceViolence.org | GitHub: BlakeRMills", x=0.5, y=0.012, size=36, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="Police Killings of Citizens (2013-2021) plotted over HOLC's Redlining Map", x=0.5, y=0.88, size=50, family=font2, color="#3f000a") +
  draw_text(text="Baltimore", x=0.5, y=0.935, size=250, family=font1, color="#3f000a", fontface="bold") 

ggsave("~/Desktop/Day 6 (Red)/Individual Plots/Baltimore.png", width = 12, height = 12)

## Detroit
DetroitP <- ggplot() +
  geom_sf(data=Detroit, aes(fill=holc_grade), color="transparent", alpha=0.75) + 
  geom_point(data=DetroitViolence, aes(y=Latitude, x=Longitude), size=4, color="#3f000a", alpha=0.75) +
  scale_fill_manual(values = c("#619370", "#5ba2b6", "#cdcb78", "#b47280")) +
  theme_void()  + theme(legend.position = c(0.5, -0.01),
                        legend.direction = "horizontal",
                        legend.key.width = unit(5, "cm"),
                        legend.key.height = unit(0.8, "cm"),
                        legend.title = element_blank(),
                        legend.text = element_blank(), 
                        plot.margin = margin(3, 3, 1.8, 3, unit = "cm"),
                        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
                        panel.background = element_rect(fill="#fdf9f5", color="#fdf9f5")) 

ggdraw(DetroitP) +
  draw_text(text="A Grade", x=0.238, y=0.05, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="B Grade", x=0.413, y=0.05, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="C Grade", x=0.588, y=0.05, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="D Grade", x=0.763, y=0.05, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="HOLC Risk Grade:", x=0.5, y=0.078, size=55, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="Twitter: @BlakeRobMills | Source: Mapping Inequality & MappingPoliceViolence.org | GitHub: BlakeRMills", x=0.5, y=0.014, size=36, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="Police Killings of Citizens (2013-2021) plotted over HOLC's Redlining Map", x=0.5, y=0.89, size=50, family=font2, color="#3f000a") +
  draw_text(text="Detroit", x=0.5, y=0.945, size=250, family=font1, color="#3f000a", fontface="bold") 

ggsave("~/Desktop/Day 6 (Red)/Individual Plots/Detroit.png", width = 12, height = 12)


## St. Louis
StL <- ggplot() +
  geom_sf(data=StLouis, aes(fill=holc_grade), color="transparent", alpha=0.75) + 
  geom_point(data=StLouisViolence, aes(y=Latitude, x=Longitude), size=4, color="#3f000a", alpha=0.75) +
  scale_fill_manual(values = c("#619370", "#5ba2b6", "#cdcb78", "#b47280")) +
  theme_void()  + theme(legend.position = c(0.5, -0.023),
                        legend.direction = "horizontal",
                        legend.key.width = unit(5, "cm"),
                        legend.key.height = unit(0.8, "cm"),
                        legend.title = element_blank(),
                        legend.text = element_blank(), 
                        plot.margin = margin(3, 6, 2, 6, unit = "cm"),
                        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
                        panel.background = element_rect(fill="#fdf9f5", color="#fdf9f5")) 

ggdraw(StL) +
  draw_text(text="A Grade", x=0.238, y=0.046, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="B Grade", x=0.413, y=0.046, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="C Grade", x=0.588, y=0.046, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="D Grade", x=0.763, y=0.046, size=45, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="HOLC Risk Grade:", x=0.5, y=0.073, size=55, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="Twitter: @BlakeRobMills | Source: Mapping Inequality & MappingPoliceViolence.org | GitHub: BlakeRMills", x=0.5, y=0.012, size=36, family=font1, color="#3f000a", fontface="bold") +
  draw_text(text="Police Killings of Citizens (2013-2021) plotted over HOLC's Redlining Map", x=0.5, y=0.89, size=50, family=font2, color="#3f000a") +
  draw_text(text="St. Louis", x=0.5, y=0.945, size=250, family=font1, color="#3f000a", fontface="bold") 

ggsave("~/Desktop/Day 6 (Red)/Individual Plots/StLouis.png", width = 12, height = 12)


# Main Plot

## NYC with legend
NYP <- ggplot() +
  geom_sf(data=NewYork, aes(fill=holc_grade), color="transparent", alpha=0.75) + 
  geom_point(data=NewYorkViolence, aes(y=Latitude, x=Longitude), size=7.25, color="#3f000a", alpha=0.75) +
  scale_fill_manual(values = c("#619370", "#5ba2b6", "#cdcb78", "#b47280")) +
  theme_void()  +
  theme(legend.position = c(0.5, -0.09),
        legend.direction = "horizontal",
        legend.key.width = unit(13, "cm"),
        legend.key.height = unit(2.2, "cm"),
        legend.text = element_blank(),
        legend.title = element_blank(),
        plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
        plot.title = element_text(size=285, hjust=0.5, vjust=1.5, face="bold", color="#3f000a", family=font1),
        plot.margin = margin(4, 0.25, 0.5, 0.25, unit = "cm")) 

#Main Plot
plot_grid(plotter(Baltimore, BaltimoreViolence),
          plotter(Chicago, ChicagoViolence),
          plotter(Detroit, DetroitViolence),
          plotter(LosAngeles, LAViolence),
          NYP,
          plotter(StLouis, StLouisViolence),
          nrow = 2, ncol=3) +
  ggtitle("Police Killings of Citizens over HOLC's Redlining Maps") + 
  labs(caption="Twitter: @BlakeRobMills | Source: Mapping Inequality & MappingPoliceViolence.org | GitHub: BlakeRMills") +
  theme(plot.background = element_rect(fill="#fdf9f5", color="#fdf9f5"),
        plot.margin = margin(5, 0.5, 6, 1, unit="cm"),
        plot.title = element_text(size=300, hjust=0.5, vjust=6, face="bold", color="#3f000a", family=font1),
        plot.caption = element_text(size=125, hjust=0.5, vjust=-15, color="#3f000a", family=font2, face="bold")) +
  annotate(geom="text", label="(Data collected between 2013 and 2021)", x=0.5, y=1.033, size=50, family=font1, color="#3f000a") +
  annotate(geom="text", label="Baltimore", x=0.18, y=0.987, size=85, family=font2, color="#730012") +
  annotate(geom="text", label="Chicago", x=0.5, y=0.987, size=85, family=font2, color="#730012") +
  annotate(geom="text", label="Detroit", x=0.84, y=0.987, size=85, family=font2, color="#730012") +
  annotate(geom="text", label="Los Angeles", x=0.18, y=0.477, size=85, family=font2, color="#730012") +
  annotate(geom="text", label="New York", x=0.5, y=0.477, size=85, family=font2, color="#730012") +
  annotate(geom="text", label="St. Louis", x=0.84, y=0.477, size=85, family=font2, color="#730012")+
  annotate(geom="text", label="A Grade", x=0.312, y=-0.033, size=45, family=font1, color="#3f000a", fontface="bold") +
  annotate(geom="text", label="B Grade", x=0.438, y=-0.033, size=45, family=font1, color="#3f000a", fontface="bold") +
  annotate(geom="text", label="C Grade", x=0.566, y=-0.033, size=45, family=font1, color="#3f000a", fontface="bold") +
  annotate(geom="text", label="D Grade", x=0.695, y=-0.033, size=45, family=font1, color="#3f000a", fontface="bold") +
  annotate(geom="text", label="HOLC Risk Grade:", x=0.5, y=0.019, size=65, family=font1, color="#3f000a", fontface="bold") 
  
ggsave("~/Desktop/Day 6 (Red)/RedlineMap.png", width = 42, height = 35) 
          


