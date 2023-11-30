# Libraries
library(cowplot)
library(geojsonio)
library(MoMAColors)
library(sf)
library(showtext)
library(sysfonts)
library(tidycensus)
library(tidyverse)

# Data
dc_income <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = "New York", 
  year = 2020,
  geometry = TRUE
)
