library(geojsonio)
library(sf)
library(tidyverse)
library(readr)
library(plyr)
library(showtext)
library(cowplot)

# Aes
showtext_auto()
font_add_google("Josefin Sans")
font_add_google("Cabin")
font_add_google("Barlow Condensed")
font1 <- "Josefin Sans"
font2 <- "Barlow Condensed"
font3 <- "Cabin"

nycwards <- geojson_sf("~/Desktop/Day 12 (Population)/nyc-wards.geojson") 
census <- read_csv("~/Desktop/Day 12 (Population)/NYSCensus.csv")


MontFix <- data.frame(id="1703 North", name="North", type="st:Ward", validSince=1703, validUntil=1785,
                      Population=1138, CensusYear=1703, geometry=st_union((nycwards %>% filter(name %in% c("East", "Montgomery")))))

nycWards <- nycwards %>%  
  as.data.frame(.) %>% 
  left_join(census, by= c("name", "validSince")) %>%
  filter(id != "1703-1785-montgomery", id != "1703-1785-east", is.na(CensusYear)==FALSE)  %>%
  rbind.fill(MontFix)

x <- nycWards %>% group_by(validSince) %>%
  dplyr::summarize(sum = sum(Population))

y <- nycWards %>% left_join(x, by="validSince") %>% mutate(prop = Population/sum)
  

rm(MontFix, nycwards, census)

plotter <- function(year){
  ggplot() +
    geom_sf(data=nycWards %>% filter(validSince==year), aes(geometry=geometry, fill=Population), color="#fdf7ee") +
    scale_fill_stepsn(limits = c(0, 60000),
                      breaks = c(0, 500, 1000, 2000, 5000, 10000, 20000, 30000, 40000, 50000, 60000),
                      values = scales::rescale(c(0, 500, 1000, 2000, 5000, 10000, 20000, 30000, 40000, 50000, 60000)),
                      colors=rev(c("#1a1b33", "#2c2d54", "#434475", "#646b8e", "#8692a7", "#87a0ac", "#88b6a7", "#6f9774", "#577842", "#3f5730", "#324727")))+
    theme_void() +
    theme(plot.background = element_rect(fill="#fdf7ee", color="#fdf7ee"),
          panel.background = element_rect(fill="#fdf7ee", color="#fdf7ee"),
          legend.position = "none") 
}

Y1808 <- ggplot() +
  geom_sf(data=nycWards %>% filter(validSince==1808), aes(geometry=geometry, fill=Population), color="#fdf7ee") +
  scale_fill_stepsn(limits = c(0, 60000),
                    breaks = c(0, 500, 1000, 2000, 5000, 10000, 20000, 30000, 40000, 50000, 60000),
                    labels=scales::comma,
                    values = scales::rescale(c(0, 500, 1000, 2000, 5000, 10000, 20000, 30000, 40000, 50000, 60000)),
                    colors=rev(c("#1a1b33", "#2c2d54", "#434475", "#646b8e", "#8692a7", "#88b6a7", "#6f9774", "#577842", "#3f5730", "#324727")))+
  theme_void() +
  theme(plot.background = element_rect(fill="#fdf7ee", color="#fdf7ee"),
        panel.background = element_rect(fill="#fdf7ee", color="#fdf7ee"),
        legend.position = c(0.5, 1.06),
        legend.direction = "horizontal",
        legend.key.width = unit(8.5, "cm"),
        legend.key.height = unit(1.2, "cm"),
        legend.title = element_blank(),
        legend.text = element_blank())

grid <- plot_grid(plotter(1703),
          plotter(1789),
          Y1808,
          plotter(1831),
          plotter(1853),
          nrow=1) +
  theme(plot.background = element_rect(fill="#fdf7ee", color="#fdf7ee"),
        panel.background = element_rect(fill="#fdf7ee", color="#fdf7ee"),
        plot.margin = margin(9, 1.25, 0, 1, "cm")) 

ggdraw(grid) +
  draw_label(label = "Twitter: @BlakeRobMills | Source: New York Public Library & New York State Census | GitHub: BlakeRMills", x=0.5, y=0.022, color="#324727", fontface="bold", size=75, fontfamily = font2) +
  draw_label(label="Old New York Population by Ward", x=0.5, y=0.95, size=275, fontface="bold", fontfamily = font1, color="#324727") +
  draw_line(x=c(0, 0.15), y=c(0.95, 0.95), size=3.5, color="#324727") +
  draw_line(x=c(0.85, 1), y=c(0.95, 0.95), size=3.5, color="#324727") +
  draw_label(label="From 1683 to 1938, New York organized itself into wards that served as electoral districts and other administrative functions. Maps show the population of wards\nover time using historical New York Census data from 1703 to 1855. Historical boundaries of wards are provided by the New York Public Library.", 
             x=0.5, y=0.86, size=105, lineheight=0.3, fontfamily=font2, color="#3d5f3c") +
  draw_label(label="1703", x=0.061, y=0.473, size=150, fontface="bold", angle=55, fontfamily = font3, color="#324727") +
  draw_label(label="Population: 4,446", x=0.077, y=0.44, size=110, angle=55, fontfamily = font2, color="#324727") +
  draw_label(label="1790", x=0.258, y=0.472, size=150, fontface="bold", angle=55, fontfamily = font3, color="#88b6a7") +
  draw_label(label="Population: 32,328", x=0.271, y=0.44, size=110, angle=55, fontfamily = font2, color="#88b6a7") +
  draw_label(label="1810", x=0.451, y=0.472, size=150, fontface="bold", angle=55, fontfamily = font3, color="#6f9774") +
  draw_label(label="Population: 96,373", x=0.465, y=0.44, size=110, angle=55, fontfamily = font2, color="#6f9774") +
  draw_label(label="1835", x=0.646, y=0.472, size=150, fontface="bold", angle=55, fontfamily = font3, color="#646b8e") +
  draw_label(label="Population: 268,089", x=.659, y=0.44, size=110, angle=55, fontfamily = font2, color="#646b8e") +
  draw_label(label="1855", x=0.841, y=0.472, size=150, fontface="bold", angle=55, fontfamily = font3, color="#8692a7") +
  draw_label(label="Population: 629,810", x=0.853, y=0.44, size=110, angle=55, fontfamily = font2, color="#8692a7") +
  draw_label(label="500", x=0.28, y=0.7, size=60, fontfamily = font3) +
  draw_label(label="1,000", x=0.335, y=0.7, size=60, fontfamily = font3) +
  draw_label(label="2,000", x=0.39, y=0.7, size=60, fontfamily = font3) +
  draw_label(label="5,000", x=0.445, y=0.7, size=60, fontfamily = font3) +
  draw_label(label="10,000", x=0.5, y=0.7, size=60, fontfamily = font3) +
  draw_label(label="20,000", x=0.555, y=0.7, size=60, fontfamily = font3) +
  draw_label(label="30,000", x=0.61, y=0.7, size=60, fontfamily = font3) +
  draw_label(label="40,000", x=0.665, y=0.7, size=60, fontfamily = font3) +
  draw_label(label="50,000", x=0.72, y=0.7, size=60, fontfamily = font3) +
  draw_label(label="60,000", x=0.775, y=0.7, size=60, fontfamily = font3) +
  draw_label(label="Ward Population", x=0.5, y=0.79, size=150, fontfamily = font1, fontface="bold", color="#1a1b33") +
  draw_line(x=c(0.225, 0.405), y=c(0.785, 0.785), size=1.25, color="#1a1b33") +
  draw_line(x=c(0.595, 0.775), y=c(0.785, 0.785), size=1.25, color="#1a1b33") 
  


ggsave("~/Desktop/OldNyc.png", height = 15, width = 30)

