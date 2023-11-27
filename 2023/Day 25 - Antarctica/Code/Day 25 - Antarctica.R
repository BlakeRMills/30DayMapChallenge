# Libraries
library(cowplot)
library(geojsonio)
library(MoMAColors)
library(osmdata)
library(raster)
library(sf)
library(showtext)
library(sysfonts)
library(tidyverse)

nc_data <- raster('~/Desktop/Antarctica_ice_velocity_2019_2020_1km_v01.1.nc')
agg <- aggregate(nc_data, 10)


xx <- st_as_sf(x)
0-=