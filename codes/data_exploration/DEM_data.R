# Elevation data preparation
# Author: Jon Went
# Date : 22.08.2023

# ---- 

rm(list=ls())

library(tidyverse)
library(raster)
library(sf)
library(exactextractr)

#---- load elevation data ----

# Data from *SRTM* sourced automatically from raster package, 90m gridded data, https://srtm.csi.cgiar.org/
# raster::getData('alt', country='USA', path="data/elevation")

# keep only contigous mainland USA
elevation <- raster("data/elevation/USA1_msk_alt.grd")

# --- load shapefile of routes -----

#get routes
BBS_routes <- sf::st_read("/Users/jonwent/polybox/Master thesis/Routes shapefile/segments_NLCD.2019_subset2.shp") %>%
  mutate(route = stringr::str_sub(partition,1, 6)) %>% st_transform(5070)

# get buffer distance
buffer_distance <- 500

# build buffered routes
BBS_routes_buffered <- BBS_routes %>%
  st_buffer(buffer_distance) ; rm(BBS_routes, buffer_distance)

# get routes
routes <- unique(BBS_routes_buffered$route)

# ----- Extract data ----

elevation_data <- tibble(partition = BBS_routes_buffered$partition,
                         elev = exact_extract(elevation, BBS_routes_buffered, "mean"),
                         elev.sd = exact_extract(elevation, BBS_routes_buffered, "stdev"))

elevation_data <- elevation_data %>%
  mutate(elev = round(elev, 3),
         elev.sd = round(elev.sd, 3))

save(elevation_data, file = "data/elevation/elevation.rda")

