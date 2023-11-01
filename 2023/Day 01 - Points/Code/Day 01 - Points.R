# Libraries
library(rjson)
library(tidyverse)
library(data.table)
library(geojsonio)
library(sf)
library(osmdata)
library(MoMAColors)
library(showtext)
library(sysfonts)

# Aes
showtext_auto()
font <- "Advent Pro"
font_add_google(font)

# Data
publicArt <- fromJSON(file="~/Desktop/DPR_PublicArt_001.json")
Man <- geojson_sf("~/Desktop/Borough Boundaries.geojson") 
Central <- geojson_sf("~/Desktop/Parks Properties_20231101.geojson") %>%
  filter(name311 == "Central Park")

# Cleaning
publicArt <- data.table(t(sapply(publicArt, function(x) unlist(lapply(x, function(x) ifelse(is.null(x),NA,x))))))

publicArt <-  publicArt %>% 
  mutate(lat = as.numeric(lat),
         lng = as.numeric(lng),
         start = as.Date(from_date),
         end = as.Date(to_date),
         time = (end - start) %>% as.numeric()) %>%
  filter(is.na(lat) == F, 
         lng < 0,
         lat > 0,
         borough == "M") 

Man <- Man %>% 
  filter(boro_name == "Manhattan") %>%
  st_cast("POLYGON") %>%
  mutate(Area = st_area(.)) %>%
  arrange(-Area) %>%
  .[1, ]

ps <- st_as_sf(publicArt, coords = c("lng", "lat"), crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
  st_filter(., Man)






# Plot
ggplot() +
  geom_sf(data=Man) +
  geom_sf(data=Central, fill=moma.colors("Kippenberger")[7], color="transparent") +
  geom_sf(data=ps, aes(color=time)) + 
  scale_color_moma_c("Ernst")

